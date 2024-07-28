//! Fuzzing helper function

use std::collections::VecDeque;
use std::fmt::Debug;

use brainoxide::ast::GenOp;

#[derive(Debug, arbitrary::Arbitrary)]
pub struct FuzzInputAST {
    pub ast: Vec<GenOp>,
    pub input: VecDeque<u8>,
}

#[derive(arbitrary::Arbitrary)]
pub struct FuzzInputSrc {
    pub code: Vec<u8>,
    pub input: VecDeque<u8>,
}

impl Debug for FuzzInputSrc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FuzzInputSrc")
            .field("code", &brainoxide::byte_utils::as_bstr(&self.code))
            .field("input", &self.input)
            .finish()
    }
}
