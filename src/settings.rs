//! Settings defining what to do

/// Set of basic block optimisations to enable
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BasicBlockOptimisationPass {
    /// Simplify & lower loops
    SimplifyLoops,
    /// Constant propagation from the start
    PropagateEntryConditions,
    /// Merge constant outputs
    MergeConstantOutputs,
    /// Drop dead stores
    DropDead,
}

/// Set of optimisations to enable
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OptimisationPass {
    /// General: Shift moves as far right as possible, fuse basic blocks.
    ShiftAndFuse,
    /// General: Peephole.
    ///
    /// * Lowers balanced loops into basic blocks.
    /// * Converts `[>]` etc into seek.
    Peephole,
    /// Perform basic block optimisations
    BasicBlock(Vec<BasicBlockOptimisationPass>),
    /// Drop stores and moves that cannot be observed at the end of the
    /// program, but that are technically not dead.
    ///
    /// Incompatible with fuzzing, since it verifies the tape state and pointer.
    DropUnobservableAtEnd,
}

/// Get standard optimisations
pub fn get_default_optimisations() -> Vec<OptimisationPass> {
    vec![
        OptimisationPass::ShiftAndFuse,
        OptimisationPass::Peephole,
        OptimisationPass::ShiftAndFuse,
        OptimisationPass::BasicBlock(vec![
            BasicBlockOptimisationPass::SimplifyLoops,
            BasicBlockOptimisationPass::PropagateEntryConditions,
            BasicBlockOptimisationPass::MergeConstantOutputs,
            BasicBlockOptimisationPass::DropDead,
        ]),
        OptimisationPass::DropUnobservableAtEnd,
    ]
}

/// Get optimisations compatible with fuzzing.
pub fn get_fuzz_optimisations() -> Vec<OptimisationPass> {
    get_default_optimisations()
        .into_iter()
        .filter(|x| *x != OptimisationPass::DropUnobservableAtEnd)
        .collect()
}
