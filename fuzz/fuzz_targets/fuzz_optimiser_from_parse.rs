#![no_main]

use libfuzzer_sys::fuzz_target;

use brainoxide::{optimize, parse_source, settings::get_fuzz_optimisations};

fuzz_target!(|data: &[u8]| {
    let res = parse_source(data);
    let ast = if let Ok(v) = res {
        v
    } else {
        return;
    };
    optimize(ast, false, get_fuzz_optimisations().as_slice());
});
