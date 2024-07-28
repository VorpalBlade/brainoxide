//! # BrainOxide - An optimizing Brainfuck compiler/interpreter
//!
//! **NOTE! This is a command line program. This library does NOT provide a
//! stable API, or even an API meant to be consumed by external code at all.**
//!
//! You have been warned.

// Re-export some symbols.
pub use codegen::generate_c;
pub use interpreter::execute;
pub use interpreter::ExecuteCallbackData;
pub use interpreter::ExecuteCallbackResult;
pub use interpreter::ExecutionError;
pub use optimizers::optimize;
pub use parser::parse_source;
pub use parser::ParseError;
pub use types::BfNum;
pub use types::TapeAddr;
pub use types::TapeAddrError;

pub mod ast;
mod bbdag;
#[doc(hidden)]
pub mod byte_utils;
mod codegen;
pub mod equation;
mod interpreter;
mod opt_types;
mod optimizers;
mod parser;
pub mod settings;
pub mod tape;
#[doc(hidden)]
pub mod test_utils;
pub mod types;
