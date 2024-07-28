#![no_main]

use brainoxide::ast::*;
use brainoxide::settings::get_fuzz_optimisations;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|ast: Vec<GenOp>| {
    brainoxide::optimize(ast, false, get_fuzz_optimisations().as_slice());
});
