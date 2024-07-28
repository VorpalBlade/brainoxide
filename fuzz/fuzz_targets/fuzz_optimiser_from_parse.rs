#![no_main]

use brainoxide::optimize;
use brainoxide::parse_source;
use brainoxide::settings::get_fuzz_optimisations;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let res = parse_source(data);
    let ast = if let Ok(v) = res {
        v
    } else {
        return;
    };
    optimize(ast, false, get_fuzz_optimisations().as_slice());
});
