//! Fuzz comparing execution with and without optimisation

#![no_main]

use libfuzzer_sys::fuzz_target;

use brainoxide::test_utils::{compare_runs, test_execute};
use brainoxide::{parse_source, settings::get_fuzz_optimisations};
use brainoxide_fuzz::FuzzInputSrc;

fuzz_target!(|data: FuzzInputSrc| {
    let res = parse_source(&data.code);
    let ast = if let Ok(v) = res {
        v
    } else {
        return;
    };
    let input = data.input;

    let exec1 = test_execute(&ast, &mut input.clone());
    let new_ast = brainoxide::optimize(ast.clone(), false, get_fuzz_optimisations().as_slice());
    let exec2 = test_execute(&new_ast, &mut input.clone());
    compare_runs(&exec1, &exec2);
});
