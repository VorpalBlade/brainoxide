#![no_main]

use libfuzzer_sys::fuzz_target;

use brainoxide::{ast::*, settings::get_fuzz_optimisations};

fuzz_target!(|ast: Vec<GenOp>| {
    brainoxide::optimize(ast, false, get_fuzz_optimisations().as_slice());
});
