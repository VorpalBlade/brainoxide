use crate::{
    ast::*,
    tape::{AbstractTape, VecTape},
    ExecuteCallbackResult, ExecutionError, TapeAddr, TapeAddrError,
};

#[derive(Debug, PartialEq)]
pub struct ExecutionState {
    pub result: Option<Result<(), ExecutionError>>,
    pub tape: VecTape,
    pub tape_ptr: TapeAddr,
    pub output: Vec<u8>,
}

impl Default for ExecutionState {
    fn default() -> Self {
        Self {
            result: None,
            tape: Default::default(),
            tape_ptr: 0.into(),
            output: Default::default(),
        }
    }
}

pub fn compare_tapes(tape1: &VecTape, tape2: &VecTape) -> bool {
    for i in 0..std::cmp::max(Into::<i64>::into(tape1.len()), tape2.len().into()) {
        if tape1.get(i.into()) != tape2.get(i.into()) {
            return false;
        }
    }
    true
}

pub fn compare_runs(exec1: &ExecutionState, exec2: &ExecutionState) {
    // Because reordering is possible, the exact result may differ.
    match (
        exec1.result.as_ref().unwrap(),
        exec2.result.as_ref().unwrap(),
    ) {
        (Ok(_), Ok(_)) => {
            // When the program completes, everything should match in the end.
            assert_eq!(exec1.result, exec2.result);
            assert_eq!(exec1.output, exec2.output);
            //assert_eq!(exec1.tape_ptr, exec2.tape_ptr);
            //assert_eq!(exec1.tape, exec2.tape);
        }
        // We have no idea about reordering, errors can change
        (Err(ref e1), Err(ref e2)) => {
            match (e1, e2) {
                // These can differ due to optimisations moving things around
                (ExecutionError::TapeError(_), _) | (_, ExecutionError::TapeError(_)) => (),
                (ExecutionError::Aborted, _) | (_, ExecutionError::Aborted) => (),
                // IO is ordered, we should fail at the same point at least.
                (ExecutionError::IoError(_), ExecutionError::IoError(_)) => {
                    assert_eq!(exec1.result, exec2.result);
                    assert_eq!(exec1.output, exec2.output);
                }
            }
        }
        // Ok -> Err for optimisation is very unlikely, but tape addr too large has been observed
        (Ok(_), Err(ExecutionError::TapeError(TapeAddrError::TapeAddrTooLarge))) => (),
        (Ok(_), Err(err)) => {
            panic!("Error after optimisation: {err:?}");
        }
        // Err -> Ok happens quite often (folding moves into negatives away, etc.)
        (Err(_), Ok(_)) => (),
    }
}

pub fn test_execute(ast: &[GenOp], input: &mut impl std::io::Read) -> ExecutionState {
    let mut instr_count = 0;
    let mut exec = ExecutionState::default();
    exec.result = Some(crate::execute(
        ast,
        &mut exec.tape,
        &mut exec.tape_ptr,
        input,
        &mut exec.output,
        &mut |_, _, _| {
            instr_count += 1;
            if instr_count > 500000 {
                ExecuteCallbackResult::Abort
            } else {
                ExecuteCallbackResult::Continue
            }
        },
    ));

    exec
}
