//! Types and functions for the AST.

use crate::{equation::Equation, BfNum, TapeAddr};

/// Trait for functions all opcodes support.
pub trait Opcode: PartialEq + Eq {
    /// This tells us if the op code changes position or not.
    fn changes_pos(&self) -> bool;
    /// Is this an IO operation?
    fn has_io(&self) -> bool;
}

/// A simple operation (one that does not move the tape pointer)
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum SimpleOp {
    /// A simple balanced loop
    BalancedLoop { b: SimpleBlock },
    /// A loop that excutes 0 or 1 times.
    If { b: SimpleBlock },
    /// A modification (fused +/- operations)
    Add(BfNum),
    /// A set operation (the optimiser figured out the exact value)
    Set(BfNum),
    /// A polyset operation: Something like `x[2] = 2*x[1] + (-3)*x[3]`
    EqnSet(Equation),
    /// Read input, one byte
    Input,
    /// Write output, one byte
    Output,
    /// Fixed output. We cannot use String, as BF may not use UTF-8.
    ///
    /// We drop the BfNum here, as this makes output easier, and we will
    /// never need wrapping semantics on this again (we will only
    /// append/prepend values).
    OutputConst(Vec<u8>),
}

impl SimpleOp {
    /// Create a new loop
    pub(crate) fn new_loop(ops: Vec<Operation<SimpleOp>>) -> Self {
        Self::BalancedLoop {
            b: SimpleBlock { ops },
        }
    }

    // Offset the current operation.
    fn offset(&mut self, offset: TapeAddr) {
        match self {
            SimpleOp::BalancedLoop { ref mut b } => b.offset(offset),
            SimpleOp::If { ref mut b } => b.offset(offset),
            SimpleOp::Add(_) => (),
            SimpleOp::Set(_) => (),
            SimpleOp::EqnSet(ref mut poly) => poly.offset(offset),
            SimpleOp::Input => (),
            SimpleOp::Output => (),
            SimpleOp::OutputConst(_) => (),
        }
    }
}

impl Opcode for SimpleOp {
    fn changes_pos(&self) -> bool {
        false
    }

    fn has_io(&self) -> bool {
        match self {
            Self::Input | Self::Output | Self::OutputConst(_) => true,
            Self::BalancedLoop { ref b } => b.ops.iter().any(|x| x.has_io()),
            _ => false,
        }
    }
}

/// A block of simple operations
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct SimpleBlock {
    pub(crate) ops: Vec<Operation<SimpleOp>>,
}

impl SimpleBlock {
    pub(crate) fn new() -> Self {
        Self { ops: vec![] }
    }

    /// Move the simple block by a specific offset
    pub(crate) fn offset(&mut self, offset: TapeAddr) {
        for e in self.ops.iter_mut() {
            e.offset += offset;
            e.opcode.offset(offset);
        }
    }
}

impl Default for SimpleBlock {
    fn default() -> Self {
        Self::new()
    }
}

/// A complex opcode, that *may* move the tape pointer.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum GeneralOp {
    Move(TapeAddr),
    // Difficult operations, we will loose track of where we are (at least in the general case)
    Seek { step: TapeAddr, value: BfNum },
    UnbalancedLoop { ops: Vec<Operation<GeneralOp>> },
    // A block with only simple operations, all offsets are known
    BasicBlock(SimpleBlock),
}

impl Opcode for GeneralOp {
    // This tells us if the op code changes position or not.
    fn changes_pos(&self) -> bool {
        match self {
            GeneralOp::Move(_) => true,
            GeneralOp::Seek { step: _, value: _ } => true,
            GeneralOp::UnbalancedLoop { ops: _ } => true,
            GeneralOp::BasicBlock(_) => false,
        }
    }

    fn has_io(&self) -> bool {
        match self {
            GeneralOp::Move(_) => false,
            GeneralOp::Seek { step: _, value: _ } => false,
            GeneralOp::UnbalancedLoop { ref ops } => ops.iter().any(|x| x.has_io()),
            GeneralOp::BasicBlock(ref sb) => sb.ops.iter().any(|x| x.has_io()),
        }
    }
}

/// General information about an operation. Consists of an opcode and
/// the offset to the current tape counter.
#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Operation<T: Opcode> {
    pub(crate) offset: TapeAddr,
    pub(crate) source_loc: usize,
    pub(crate) opcode: T,
}

impl<T: Opcode> Operation<T> {
    pub(crate) fn synth(opcode: T, offset: TapeAddr) -> Self {
        Self {
            offset,
            source_loc: 0,
            opcode,
        }
    }
}

impl<T: Opcode> Operation<T> {
    pub(crate) fn changes_pos(&self) -> bool {
        self.opcode.changes_pos()
    }

    pub(crate) fn has_io(&self) -> bool {
        self.opcode.has_io()
    }
}

/// An `Operation<SimpleOp>`
pub type SimpOp = Operation<SimpleOp>;
/// An `Operation<GeneralOp>`
pub type GenOp = Operation<GeneralOp>;
