use std::io::Read;
use std::io::Write;

use thiserror::Error;

use crate::ast::*;
use crate::tape::Tape;
use crate::TapeAddr;
use crate::TapeAddrError;

/// Error type for execution
#[derive(Debug, Error)]
pub enum ExecutionError {
    /// Tape pointer error.
    #[error("Tape pointer error: {0}")]
    TapeError(#[from] TapeAddrError),
    /// Io error during program execution.
    #[error("Unexpected IO Error: {0}")]
    IoError(#[from] std::io::Error),
    /// Aborted by callback
    #[error("Callback aborted execution")]
    Aborted,
}

impl PartialEq for ExecutionError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IoError(l0), Self::IoError(r0)) => l0.kind() == r0.kind(),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

/// Data sent to execution callback
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExecuteCallbackData<'program> {
    /// We are executing a GenOp
    GenOp(&'program GenOp),
    /// We are performing an iteration of a loop
    InSeek,
    /// We are executing a loop (balanced or unbalanced) iteration
    InLoop,
    /// We are executing a SimpOp
    SimpOp(&'program SimpOp),
}

/// Reply type for callback
pub enum ExecuteCallbackResult {
    /// Continue execution
    Continue,
    /// Abort execution
    Abort,
}

/// Simple recursive interpreter implementation
pub fn execute<'program, TapeT: Tape, F>(
    code: &'program [GenOp],
    tape: &mut TapeT,
    tape_ptr: &mut TapeAddr,
    input: &mut impl Read,
    output: &mut impl Write,
    callback: &mut F,
) -> Result<(), ExecutionError>
where
    F: FnMut(ExecuteCallbackData<'program>, &TapeT, &TapeAddr) -> ExecuteCallbackResult,
{
    for instr in code {
        match callback(ExecuteCallbackData::GenOp(instr), tape, tape_ptr) {
            ExecuteCallbackResult::Continue => (),
            ExecuteCallbackResult::Abort => return Err(ExecutionError::Aborted),
        }
        match instr.opcode {
            GeneralOp::Move(n) => *tape_ptr += n,
            GeneralOp::Seek { step, value } => {
                while tape.try_get(*tape_ptr)? != value {
                    match callback(ExecuteCallbackData::InSeek, tape, tape_ptr) {
                        ExecuteCallbackResult::Continue => (),
                        ExecuteCallbackResult::Abort => return Err(ExecutionError::Aborted),
                    }
                    *tape_ptr += step;
                }
            }
            GeneralOp::UnbalancedLoop { ref ops } => {
                while tape.try_get(*tape_ptr)? != 0.into() {
                    match callback(ExecuteCallbackData::InLoop, tape, tape_ptr) {
                        ExecuteCallbackResult::Continue => (),
                        ExecuteCallbackResult::Abort => return Err(ExecutionError::Aborted),
                    }
                    execute(ops, tape, tape_ptr, input, output, callback)?;
                }
            }
            GeneralOp::BasicBlock(ref bb) => {
                execute_basic_block(&bb.ops, tape, tape_ptr, input, output, callback)?;
            }
        }
        if tape_ptr.is_negative() {
            return Err(TapeAddrError::TapeAddrIsNegative.into());
        }
    }
    Ok(())
}

fn execute_basic_block<'program, TapeT: Tape, F>(
    ops: &'program [SimpOp],
    tape: &mut TapeT,
    tape_ptr: &TapeAddr,
    input: &mut impl Read,
    output: &mut impl Write,
    callback: &mut F,
) -> Result<(), ExecutionError>
where
    F: FnMut(ExecuteCallbackData<'program>, &TapeT, &TapeAddr) -> ExecuteCallbackResult,
{
    for instr in ops {
        match callback(ExecuteCallbackData::SimpOp(instr), tape, tape_ptr) {
            ExecuteCallbackResult::Continue => (),
            ExecuteCallbackResult::Abort => return Err(ExecutionError::Aborted),
        }
        let real_addr = *tape_ptr + instr.offset;
        match instr.opcode {
            SimpleOp::BalancedLoop { ref b } => {
                while tape.try_get(real_addr)? != 0.into() {
                    match callback(ExecuteCallbackData::InLoop, tape, tape_ptr) {
                        ExecuteCallbackResult::Continue => (),
                        ExecuteCallbackResult::Abort => return Err(ExecutionError::Aborted),
                    }
                    execute_basic_block(&b.ops, tape, tape_ptr, input, output, callback)?;
                }
            }
            SimpleOp::If { ref b } => {
                if tape.try_get(real_addr)? != 0.into() {
                    execute_basic_block(&b.ops, tape, tape_ptr, input, output, callback)?;
                }
            }
            SimpleOp::Add(n) => tape.try_modify(real_addr, n)?,
            SimpleOp::Set(n) => tape.try_set(real_addr, n)?,
            SimpleOp::EqnSet(ref poly) => {
                tape.try_set(real_addr, poly.evaluate_on_tape(tape, *tape_ptr)?)?;
            }
            SimpleOp::Input => {
                let mut tmp: [u8; 1] = [0; 1];
                // We may need to flush output here if there wasn't a newline.
                output.flush()?;
                match input.read(&mut tmp) {
                    Ok(n_bytes) => {
                        if n_bytes == 0 {
                            tape.try_set(real_addr, 0.into())?;
                        } else {
                            tape.try_set(real_addr, tmp[0].into())?;
                        }
                    }
                    Err(err) => {
                        return Err(ExecutionError::IoError(err));
                    }
                }
            }
            SimpleOp::Output => {
                let tmp: [u8; 1] = [tape.try_get(real_addr)?.into()];
                output.write_all(&tmp)?;
            }
            SimpleOp::OutputConst(ref buf) => {
                output.write_all(buf.as_slice())?;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use crate::parse_source;
    use crate::tape::AbstractTape;
    use crate::tape::VecTape;
    use crate::TapeAddr;

    use super::execute;
    use super::ExecuteCallbackResult;

    #[test]
    fn test_execute() {
        let ast = parse_source(b"+++>-->++[-]>+<>>>>>,.<,,").unwrap();
        let mut tape = VecTape::new();
        let mut tape_ptr: TapeAddr = 0.into();

        let mut input: VecDeque<u8> = VecDeque::from([65, 32]);
        let mut output: Vec<u8> = Vec::new();
        execute(
            &ast,
            &mut tape,
            &mut tape_ptr,
            &mut input,
            &mut output,
            &mut |_, _, _| ExecuteCallbackResult::Continue,
        )
        .unwrap();
        assert_eq!(i64::from(tape_ptr), 6);
        assert_eq!(tape.get(0.into()), 3.into());
        assert_eq!(tape.get(1.into()), 254.into());
        assert_eq!(tape.get(2.into()), 0.into());
        assert_eq!(tape.get(3.into()), 1.into());
        assert_eq!(tape.get(6.into()), 0.into());
        assert_eq!(tape.get(7.into()), 65.into());
        assert_eq!(output, vec![65]);
    }
}
