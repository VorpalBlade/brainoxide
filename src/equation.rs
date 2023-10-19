//! Represents a recursive equation
use std::{
    collections::HashSet,
    ops::{Add, Mul},
};

use crate::{
    tape::{AbstractTape, Tape, TapeProvenance},
    BfNum, TapeAddr, TapeAddrError,
};

/// An equation, representing a set of fused and lowered BF instructions.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum Equation {
    /// A memory load from the BF tape
    Mem(TapeAddr),
    /// A constant
    Const(BfNum),
    /// Multiplying a number of child equations.
    Mul(Vec<Equation>),
    /// Adding a number of child equations.
    Add(Vec<Equation>),
}

impl Equation {
    /// Update equation, offseting all memory accesses
    ///
    /// Used to reorder with respect to a move instruction
    pub(crate) fn offset(&mut self, offset: TapeAddr) {
        self.visit_mut(&mut |e| {
            if let Equation::Mem(addr) = e {
                *addr += offset;
            }
        })
    }

    /// Tape offsets that this equation uses
    pub(crate) fn offsets(&self) -> HashSet<TapeAddr> {
        let mut offsets = HashSet::new();
        self.visit(&mut |e| {
            if let Equation::Mem(addr) = e {
                offsets.insert(*addr);
            }
        });
        offsets
    }

    /// Evaulate on an abstract tape (partial evaluation)
    pub(crate) fn evaluate_on_abstract_tape(
        &self,
        tape: &impl AbstractTape<TapeProvenance>,
        tape_offset: TapeAddr,
    ) -> Self {
        match self {
            Equation::Mem(addr) => {
                let prov = tape.get(tape_offset + *addr);
                match prov {
                    TapeProvenance::Unknown(_) => self.clone(),
                    TapeProvenance::Exactly(n) => Equation::Const(n),
                    TapeProvenance::Equation(_) => self.clone(), // TODO: We could maybe inline this. Beware aliasing.
                }
            }
            a @ Equation::Const(_) => a.clone(),
            Equation::Mul(vals) => {
                let mut unknowns = vec![];
                let mut eq_const: BfNum = 1.into();
                for e in vals {
                    match e.evaluate_on_abstract_tape(tape, tape_offset) {
                        Equation::Const(c) => eq_const *= c,
                        a => unknowns.push(a),
                    }
                }
                if eq_const == 1.into() {
                    Equation::Mul(unknowns)
                } else if eq_const == 0.into() {
                    Equation::Const(0.into())
                } else if unknowns.is_empty() {
                    Equation::Const(eq_const)
                } else if unknowns.len() == 1 && eq_const == 1.into() {
                    unknowns.pop().unwrap()
                } else {
                    unknowns.push(Equation::Const(eq_const));
                    Equation::Mul(unknowns)
                }
            }
            Equation::Add(vals) => {
                let mut unknowns = vec![];
                let mut eq_const: BfNum = 0.into();
                for e in vals {
                    match e.evaluate_on_abstract_tape(tape, tape_offset) {
                        Equation::Const(c) => eq_const += c,
                        Equation::Add(inner) => unknowns.extend(inner),
                        a => unknowns.push(a),
                    }
                }
                // TODO: We should drop the entire thing if len = 0 && eq_const == 0
                if unknowns.is_empty() {
                    Equation::Const(eq_const)
                } else if unknowns.len() == 1 && eq_const == 0.into() {
                    unknowns.pop().unwrap()
                } else {
                    if eq_const != 0.into() {
                        unknowns.push(Equation::Const(eq_const));
                    }
                    Equation::Add(unknowns)
                }
            }
        }
    }

    /// Evalute this equation on a tape to get the result.
    pub(crate) fn evaluate_on_tape(
        &self,
        tape: &impl Tape,
        tape_offset: TapeAddr,
    ) -> Result<BfNum, TapeAddrError> {
        match self {
            Equation::Mem(addr) => tape.try_get(tape_offset + *addr),
            Equation::Const(n) => Ok(*n),
            Equation::Mul(vals) => vals.iter().try_fold(1.into(), |acc, v| {
                Ok(acc * v.evaluate_on_tape(tape, tape_offset)?)
            }),
            Equation::Add(vals) => vals.iter().try_fold(0.into(), |acc, v| {
                Ok(acc + v.evaluate_on_tape(tape, tape_offset)?)
            }),
        }
    }

    /// Recursively visit through an equation, children first.
    pub(crate) fn visit<F>(&self, visitor: &mut F)
    where
        F: FnMut(&Self),
    {
        match self {
            Equation::Const(_) | Equation::Mem(_) => (),
            Equation::Mul(entries) | Equation::Add(entries) => {
                for e in entries {
                    e.visit(visitor);
                }
            }
        }
        visitor(self);
    }

    /// Recursively visit through an equation, children first.
    pub(crate) fn visit_mut<F>(&mut self, visitor: &mut F)
    where
        F: FnMut(&mut Self),
    {
        match self {
            Equation::Const(_) | Equation::Mem(_) => (),
            Equation::Mul(entries) | Equation::Add(entries) => {
                for e in entries {
                    e.visit_mut(visitor);
                }
            }
        }
        visitor(self);
    }
}

impl Add for Equation {
    type Output = Equation;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Equation::Const(a), Equation::Const(b)) => Equation::Const(a + b),
            (Equation::Const(a), o) | (o, Equation::Const(a)) if a == 0.into() => o,
            (Equation::Add(mut a), Equation::Add(b)) => {
                a.extend(b);
                Equation::Add(a)
            }
            (Equation::Add(mut a), o) | (o, Equation::Add(mut a)) => {
                a.push(o);
                Equation::Add(a)
            }
            (a, b) => Equation::Add(vec![a, b]),
        }
    }
}

impl Mul for Equation {
    type Output = Equation;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Equation::Const(a), Equation::Const(b)) => Equation::Const(a * b),
            (Equation::Const(f), _) | (_, Equation::Const(f)) if f == 0.into() => {
                Equation::Const(0.into())
            }
            (Equation::Const(f), o) | (o, Equation::Const(f)) if f == 1.into() => o,
            (a, b) => Equation::Mul(vec![a, b]),
        }
    }
}
