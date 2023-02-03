//! Fuzz comparing execution with and without optimisation
//!
//! This version has no input to the program, but can load raw BF source files
//! as tmin (useful to minimise an external program).

#![no_main]

use std::collections::VecDeque;

use libfuzzer_sys::fuzz_target;

use brainoxide::test_utils::{compare_runs, test_execute};
use brainoxide::{parse_source, settings::get_fuzz_optimisations};

fuzz_target!(|data: &[u8]| {
    let res = parse_source(&data);
    let ast = if let Ok(v) = res {
        v
    } else {
        return;
    };
    let input = VecDeque::new();

    let exec1 = test_execute(&ast, &mut input.clone());
    let new_ast = brainoxide::optimize(ast.clone(), false, get_fuzz_optimisations().as_slice());
    let exec2 = test_execute(&new_ast, &mut input.clone());
    compare_runs(&exec1, &exec2);
});
