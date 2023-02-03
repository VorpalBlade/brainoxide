#![no_main]

use brainoxide_fuzz::FuzzInputAST;
use libfuzzer_sys::fuzz_target;

use brainoxide::{execute, tape::VecTape, ExecuteCallbackResult, ExecutionError, TapeAddr};

fuzz_target!(|data: FuzzInputAST| {
    let ast = data.ast;
    let mut input = data.input;

    let mut instr_count = 0;
    let mut tape = VecTape::new();
    let mut tape_ptr: TapeAddr = 0.into();
    let mut output: Vec<u8> = Vec::new();
    let exec_result = execute(
        &ast,
        &mut tape,
        &mut tape_ptr,
        &mut input,
        &mut output,
        &mut |_, _, _| {
            instr_count += 1;
            if instr_count > 500 {
                ExecuteCallbackResult::Abort
            } else {
                ExecuteCallbackResult::Continue
            }
        },
    );
    match exec_result {
        Ok(_) => (),
        Err(err) => match err {
            ExecutionError::TapeError(_) => (),
            ExecutionError::IoError(_) => (),
            ExecutionError::Aborted => (),
        },
    }
});
