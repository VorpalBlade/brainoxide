//! Various common types for the optimiser

use crate::BfNum;

/// Describes how a loop iterates
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LoopIter {
    /// Unknown (unbalanced loop, derived from IO, inner loop, ...)
    Unknown,
    /// Loop index is *set* to zero during loop. This is an if-statement
    SingleIteration,
    /// We know that the loop index never reaches zero (not touched, or set to non-zero)
    ///
    /// If the loop is entered, it will never exit
    Infinite,
    /// Simple stepping with the specified step size
    Step(BfNum),
}

/// Describes the entry condition for a BbDag or basic block
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EntryType {
    /// We are at the start of the program, everything is known to be 0
    BeginningOfProgram,
    /// Last basic block in program, at top level, with no unbalanced loop after it.
    EndOfProgram,
    /// After the end of an unbalanced loop or seek, we know that tape
    /// offset 0 is zero.
    Index0Is0,
    /// At the beginning of balanced/unbalanced loops we have no idea.
    ///
    /// Note: Technically we know that the loop index is *not* zero, but
    /// that doesn't help much.
    Unknown,
}
