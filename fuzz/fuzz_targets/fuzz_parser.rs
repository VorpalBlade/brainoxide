#![no_main]

use libfuzzer_sys::fuzz_target;

use brainoxide::{parse_source, ParseError};

fn check_loop_balance(data: &[u8]) -> Option<ParseError> {
    let loop_counts = data.iter().filter_map(|v| match v {
        b'[' => Some(1),
        b']' => Some(-1),
        _ => None,
    });
    let mut acc = 0;
    for e in loop_counts {
        acc += e;
        if acc < 0 {
            return Some(ParseError::TooManyEndLoop);
        }
    }
    if acc != 0 {
        return Some(ParseError::TooManyStartLoop);
    }
    return None;
}

fuzz_target!(|data: &[u8]| {
    match parse_source(data) {
        Ok(_) => {
            assert_eq!(check_loop_balance(data), None);
        }
        Err(e @ ParseError::TooManyStartLoop) | Err(e @ ParseError::TooManyEndLoop) => {
            assert_eq!(check_loop_balance(data), Some(e));
        }
    }
});
