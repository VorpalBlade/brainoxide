//! Implementations of the BF tape

use std::collections::HashMap;

use crate::{equation::Equation, BfNum, TapeAddr, TapeAddrError};

/// A trait implementing a tape for some type. This is used for abstract
/// interpretation in the optimiser.
pub trait AbstractTape<T: PartialEq + Clone> {
    fn get(&self, offset: TapeAddr) -> T;
    fn set(&mut self, offset: TapeAddr, value: T);

    fn try_get(&self, offset: TapeAddr) -> Result<T, TapeAddrError>;
    fn try_set(&mut self, offset: TapeAddr, value: T) -> Result<(), TapeAddrError>;
}

/// A trait implementing a tape for the BF program memory
pub trait Tape: AbstractTape<BfNum> {
    fn modify(&mut self, offset: TapeAddr, diff: BfNum);
    fn slice(&mut self, offset: TapeAddr, len: usize) -> &[BfNum];
    fn slice_mut(&mut self, offset: TapeAddr, len: usize) -> &mut [BfNum];

    fn try_modify(&mut self, offset: TapeAddr, diff: BfNum) -> Result<(), TapeAddrError>;
}

/// A tape implemented with a Vec.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VecTape {
    data: Vec<BfNum>,
}

impl VecTape {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    /// Get a length value for the tape.
    ///
    /// All this means is that no value above this address is non-zero.
    /// It does not guarrantee that there is a non-zero value below this
    /// length.
    pub fn len(&self) -> TapeAddr {
        self.data.len().into()
    }

    fn ensure_size(&mut self, offset: TapeAddr) -> Result<(), TapeAddrError> {
        let offusize: usize = offset.try_into()?;
        #[cfg(fuzzing)]
        if offusize > 2usize.pow(20) {
            return Err(TapeAddrError::TapeAddrTooLarge);
        }
        if self.data.len() < offusize + 1 {
            self.data.resize(offusize + 1, 0.into());
        }
        Ok(())
    }
}

impl Default for VecTape {
    fn default() -> Self {
        Self::new()
    }
}

impl AbstractTape<BfNum> for VecTape {
    fn get(&self, offset: TapeAddr) -> BfNum {
        let idx: usize = offset.try_into().unwrap();
        match self.data.get(idx) {
            Some(value) => *value,
            None => 0.into(),
        }
    }

    fn set(&mut self, offset: TapeAddr, value: BfNum) {
        self.ensure_size(offset).unwrap();
        let idx: usize = offset.try_into().unwrap();
        self.data[idx] = value;
    }

    fn try_get(&self, offset: TapeAddr) -> Result<BfNum, TapeAddrError> {
        let idx: usize = offset.try_into()?;
        match self.data.get(idx) {
            Some(value) => Ok(*value),
            None => Ok(0.into()),
        }
    }

    fn try_set(&mut self, offset: TapeAddr, value: BfNum) -> Result<(), TapeAddrError> {
        self.ensure_size(offset)?;
        let idx: usize = offset.try_into()?;
        self.data[idx] = value;
        Ok(())
    }
}

impl Tape for VecTape {
    fn modify(&mut self, offset: TapeAddr, diff: BfNum) {
        self.ensure_size(offset).unwrap();
        let idx: usize = offset.try_into().unwrap();
        let cell = self.data.get_mut(idx).unwrap();
        *cell += diff;
    }

    fn slice(&mut self, offset: TapeAddr, len: usize) -> &[BfNum] {
        let idx: usize = offset.try_into().unwrap();
        self.ensure_size(offset + len.into()).unwrap();
        &self.data[idx..idx + len]
    }

    fn slice_mut(&mut self, offset: TapeAddr, len: usize) -> &mut [BfNum] {
        let idx: usize = offset.try_into().unwrap();
        self.ensure_size(offset + len.into()).unwrap();
        &mut self.data[idx..idx + len]
    }

    fn try_modify(&mut self, offset: TapeAddr, diff: BfNum) -> Result<(), TapeAddrError> {
        self.ensure_size(offset)?;
        let idx: usize = offset.try_into()?;
        let cell = self.data.get_mut(idx).unwrap();
        *cell += diff;
        Ok(())
    }
}

/// Describes the state of the tape for the optimizer
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TapeProvenance {
    /// Unknown value at an offset
    Unknown(TapeAddr),
    /// Value is known
    Exactly(BfNum),
    /// We know this as an equation
    Equation(Equation),
    // We know this as an equation
    //Polynomial(Polynomial),
    // TODO: Separate "known varying" value from unknown?
    // E.g. for input. What would we gain from the extra complexity?
    // Possibly we could fold things like input(42) - input(42) = 0
}

/// A tape for the optimizer
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct TapeState {
    contents: HashMap<TapeAddr, TapeProvenance>,
    default_non_neg: Option<TapeProvenance>,
}

impl TapeState {
    pub fn new() -> Self {
        Self {
            contents: HashMap::new(),
            default_non_neg: None,
        }
    }
    /// Set the default value for non-negative offsets
    /// Used to evaluate the first basic block where we know the tape to be all zero.
    pub fn set_default_non_negative(&mut self, default: TapeProvenance) {
        self.default_non_neg = Some(default);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&TapeAddr, &TapeProvenance)> {
        self.contents.iter()
    }
}

impl Default for TapeState {
    fn default() -> Self {
        Self::new()
    }
}

impl AbstractTape<TapeProvenance> for TapeState {
    fn get(&self, offset: TapeAddr) -> TapeProvenance {
        match self.contents.get(&offset) {
            Some(x) => x.clone(),
            None if self.default_non_neg.is_some() && offset >= 0.into() => {
                self.default_non_neg.clone().unwrap()
            }
            None => TapeProvenance::Unknown(offset),
        }
    }

    fn set(&mut self, offset: TapeAddr, value: TapeProvenance) {
        self.contents.insert(offset, value);
    }

    fn try_get(&self, offset: TapeAddr) -> Result<TapeProvenance, TapeAddrError> {
        Ok(self.get(offset))
    }

    fn try_set(&mut self, offset: TapeAddr, value: TapeProvenance) -> Result<(), TapeAddrError> {
        self.set(offset, value);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{AbstractTape, Tape, VecTape};

    #[test]
    fn test_vec_tape() {
        let mut tape = VecTape::new();
        tape.set(2.into(), 5.into());
        assert_eq!(tape.get(2.into()), 5.into());
        assert_eq!(tape.try_get(2.into()), Ok(5.into()));
        tape.modify(2.into(), 255.into());
        assert_eq!(tape.get(2.into()), 4.into());
        tape.modify(8.into(), 200.into());
        assert_eq!(tape.get(8.into()), 200.into());

        assert_eq!(
            tape.try_get((-1).into()),
            Err(crate::TapeAddrError::TapeAddrIsNegative)
        );
        assert_eq!(
            tape.try_set((-1).into(), 200.into()),
            Err(crate::TapeAddrError::TapeAddrIsNegative)
        );
    }
}
