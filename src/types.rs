//! Fundamental data types used throughout brainoxide

use std::{
    fmt::Display,
    num::Wrapping,
    ops::{Add, AddAssign, Mul, MulAssign, Sub, SubAssign},
};
use thiserror::Error;

/// Error type for TapeAddr operations
#[derive(Debug, Clone, Copy, Error, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub enum TapeAddrError {
    /// The tape addr is negative in a context where this is not allowed.
    #[error("Tape pointer moved too far left (before start of tape)")]
    TapeAddrIsNegative,
    /// Tape address is too large (depends on tape implementation)
    #[error("Tape pointer moved too far to the right")]
    TapeAddrTooLarge,
}

/// Newtype for tape pointer / tape offset
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct TapeAddr(pub i64);

impl TapeAddr {
    pub fn new(val: i64) -> Self {
        Self(val)
    }

    pub fn is_negative(&self) -> bool {
        self.0 < 0
    }

    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl From<i32> for TapeAddr {
    fn from(value: i32) -> Self {
        Self(value as i64)
    }
}

impl From<i64> for TapeAddr {
    fn from(value: i64) -> Self {
        Self(value)
    }
}

impl From<usize> for TapeAddr {
    fn from(value: usize) -> Self {
        Self(value as i64)
    }
}

impl From<TapeAddr> for i64 {
    fn from(value: TapeAddr) -> Self {
        value.0
    }
}

impl TryFrom<TapeAddr> for usize {
    type Error = TapeAddrError;

    fn try_from(value: TapeAddr) -> Result<Self, Self::Error> {
        if value.0 < 0 {
            Err(TapeAddrError::TapeAddrIsNegative)
        } else {
            Ok(value.0 as Self)
        }
    }
}

impl Add for TapeAddr {
    type Output = TapeAddr;

    fn add(self, rhs: Self) -> Self::Output {
        TapeAddr(self.0.wrapping_add(rhs.0))
    }
}

impl AddAssign for TapeAddr {
    fn add_assign(&mut self, rhs: Self) {
        // Unlikely to be hit in practise (good luck getting 2^64 ">" in a program!,
        // but fuzzing directly on AST can easily trigger this).
        self.0 = self.0.wrapping_add(rhs.0);
    }
}

impl Sub for TapeAddr {
    type Output = TapeAddr;

    fn sub(self, rhs: Self) -> Self::Output {
        TapeAddr(self.0 - rhs.0)
    }
}

impl SubAssign for TapeAddr {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl Display for TapeAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A BF number (u8 with wrapping semantics).
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct BfNum(Wrapping<u8>);

impl Add for BfNum {
    type Output = BfNum;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for BfNum {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0;
    }
}

impl Sub for BfNum {
    type Output = BfNum;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl SubAssign for BfNum {
    fn sub_assign(&mut self, rhs: Self) {
        self.0 -= rhs.0;
    }
}

impl Mul for BfNum {
    type Output = BfNum;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl MulAssign for BfNum {
    fn mul_assign(&mut self, rhs: Self) {
        self.0 *= rhs.0;
    }
}

impl From<i64> for BfNum {
    fn from(value: i64) -> Self {
        Self(Wrapping::<u8>(value.rem_euclid(256) as u8))
    }
}

impl From<i32> for BfNum {
    fn from(value: i32) -> Self {
        Self(Wrapping::<u8>(value.rem_euclid(256) as u8))
    }
}

impl From<u8> for BfNum {
    fn from(value: u8) -> Self {
        Self(Wrapping::<u8>(value))
    }
}

impl From<BfNum> for Wrapping<u8> {
    fn from(value: BfNum) -> Self {
        value.0
    }
}

impl From<BfNum> for u8 {
    fn from(value: BfNum) -> Self {
        value.0 .0
    }
}

impl From<BfNum> for i64 {
    fn from(value: BfNum) -> Self {
        value.0 .0 as i64
    }
}

impl Display for BfNum {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
