// Optimisers for inside a basic block

use std::collections::HashSet;

use petgraph::{algo::toposort, prelude::*};

use crate::{
    ast::*,
    bbdag::{BbDag, BbDagEdge, BbDagNode, BbDagOp, DagProperties},
    equation::Equation,
    opt_types::{EntryType, LoopIter},
    settings::BasicBlockOptimisationPass,
    tape::AbstractTape,
    tape::{TapeProvenance, TapeState},
};

/// Propagate entry conditions for the basic block.
///
/// This is a wrapper that sets up the tate state.
///
/// The final tape state is returned, and can be used to optimise the entry
/// condition of following loop.
fn propagate_entry_conditions(dag: &mut BbDag, entry: EntryType) -> TapeState {
    let mut tape = TapeState::new();
    match entry {
        EntryType::BeginningOfProgram => {
            tape.set_default_non_negative(TapeProvenance::Exactly(0.into()))
        }
        EntryType::Index0Is0 => tape.set(0.into(), TapeProvenance::Exactly(0.into())),
        EntryType::Unknown | EntryType::EndOfProgram => (),
    }
    propagate_entry_conditions_inner(dag, &mut tape);
    tape
}

/// Recrsive implementation of propagating entry conditions
fn propagate_entry_conditions_inner(dag: &mut BbDag, tape: &mut TapeState) {
    for idx in toposort(&dag.graph, None).unwrap().iter() {
        let node = dag.graph.node_weight(*idx).unwrap().clone();
        let prov = tape.get(node.offset);

        match node.op {
            BbDagOp::Beginning => (),
            BbDagOp::Loop(_) => {
                // TODO: We could do loop unrolling / lowering if:
                // * Exactly(0) -> drop loop
                // * Exactly(1) -> Lower to If if loop behaviour is simple (-1)
                // * Infinite loop detection?
                let mut inner_tape = TapeState::new();
                propagate_entry_conditions_inner(
                    dag.get_inner_body_mut(idx).unwrap(),
                    &mut inner_tape,
                );
                // We don't yet know what the inner values are, set all to unknown
                for (idx, _) in inner_tape.iter() {
                    tape.set(*idx, TapeProvenance::Unknown(*idx));
                }
                tape.set(node.offset, TapeProvenance::Exactly(0.into()));
            }
            BbDagOp::If(_) => {
                // TODO: Unroll/remove if-op if we know the value
                let mut inner_tape = TapeState::new();
                propagate_entry_conditions_inner(
                    dag.get_inner_body_mut(idx).unwrap(),
                    &mut inner_tape,
                );
                // We don't yet know what the inner values are, set all to unknown
                for (idx, _) in inner_tape.iter() {
                    tape.set(*idx, TapeProvenance::Unknown(*idx));
                }
            }
            BbDagOp::AddConst(x) => match prov {
                TapeProvenance::Exactly(y) => {
                    // Replace with set node, drop using
                    let newval = x + y;
                    dag.graph.node_weight_mut(*idx).unwrap().op = BbDagOp::Set(newval);
                    dag.remove_edges_to_node(idx, Direction::Incoming, |edge| {
                        if let BbDagEdge::Using { addr: u, cond: _ } = edge {
                            *u == node.offset
                        } else {
                            false
                        }
                    });
                    tape.set(node.offset, TapeProvenance::Exactly(newval));
                }
                TapeProvenance::Unknown(_) => {
                    tape.set(node.offset, TapeProvenance::Unknown(node.offset))
                }
                TapeProvenance::Equation(_) => {
                    // Buggy! Self ref not handled properly
                    //let new_eqn = eqn.clone() + Equation::Const(x);
                    //dag.graph.node_weight_mut(*idx).unwrap().op =
                    //    BbDagOp::Equation(new_eqn.clone());
                    //tape.set(node.offset, TapeProvenance::Equation(new_eqn))
                    tape.set(node.offset, TapeProvenance::Unknown(node.offset))
                }
            },
            BbDagOp::Set(n) => {
                tape.set(node.offset, TapeProvenance::Exactly(n));
            }
            BbDagOp::Equation(ref eqn) => {
                //dbg!(&eqn);
                //dbg!(&tape);
                let new_eqn = eqn.evaluate_on_abstract_tape(tape, 0.into());
                //dbg!(&new_eqn);
                let mut new_node = BbDagNode {
                    op: BbDagOp::Equation(new_eqn.clone()),
                    offset: node.offset,
                };
                match new_eqn {
                    Equation::Const(num) => {
                        new_node.op = BbDagOp::Set(num);
                        tape.set(node.offset, TapeProvenance::Exactly(num));
                    }
                    Equation::Mul(_) | Equation::Add(_) | Equation::Mem(_) => {
                        tape.set(node.offset, TapeProvenance::Equation(new_eqn));
                    }
                }
                dag.replace_with(idx, vec![new_node]);
            }
            BbDagOp::Input => {
                tape.set(node.offset, TapeProvenance::Unknown(node.offset));
            }
            BbDagOp::Output => {
                match prov {
                    TapeProvenance::Exactly(value) => {
                        dag.graph.node_weight_mut(*idx).unwrap().op =
                            BbDagOp::OutputConst(vec![value.into()]);
                    }
                    TapeProvenance::Unknown(_) => (),
                    TapeProvenance::Equation(_) => (),
                };
            }
            BbDagOp::OutputConst(_) => (),
        }
    }
}

// TODO: May not be useful since entry condition does this too
// fn propagate_const(dag: &mut BbDag) {}

/// This pass attempts to convert inner loops to ifs and equations
fn simplify_loops(dag: &mut BbDag) {
    for idx in toposort(&dag.graph, None).unwrap().iter() {
        let node = dag.graph.node_weight_mut(*idx).unwrap();
        if node.has_body() {
            match simplify_loops_inner(node) {
                SimpLoopResult::KeepLoop => (),
                SimpLoopResult::NewCode(new_code) => {
                    dag.replace_with(idx, new_code);
                }
            }
        }
    }
}

/// Signals to the caller what to do.
#[derive(Debug, Clone)]
enum SimpLoopResult {
    /// Keep (the possibly edited) loop
    KeepLoop,
    /// Replace the entire loop with this new code
    NewCode(Vec<BbDagNode>),
}

fn simplify_loops_inner(dag_node: &mut BbDagNode) -> SimpLoopResult {
    // Optimise any inner loops first.
    {
        let body = dag_node.body_mut().unwrap();
        for idx in toposort(&body.graph, None).unwrap() {
            let node = body.graph.node_weight_mut(idx).unwrap();
            if node.has_loop() {
                match simplify_loops_inner(node) {
                    SimpLoopResult::KeepLoop => (),
                    SimpLoopResult::NewCode(new_code) => {
                        body.replace_with(&idx, new_code);
                    }
                }
            }
        }
    }

    // TODO: We may be able to do better (for constant outputs for example), but then it gets complicated
    // For example: We should still be able to convert to an if statement.
    if dag_node.has_io() {
        return SimpLoopResult::KeepLoop;
    }

    // At this point we need to re-do the has_loop check, as we might have collapsed them.
    if !dag_node.body().unwrap().has_conditional() {
        let loop_idx_offset = dag_node.offset;
        match dag_node.loop_indexing_behaviour().unwrap() {
            LoopIter::Unknown => return SimpLoopResult::KeepLoop,
            LoopIter::SingleIteration => {
                // Replace with an if-statement
                return SimpLoopResult::NewCode(vec![BbDagNode {
                    op: BbDagOp::If(Box::new(dag_node.body().unwrap().clone())),
                    offset: dag_node.offset,
                }]);
            }
            LoopIter::Infinite => return SimpLoopResult::KeepLoop,
            LoopIter::Step(n) if n == 255.into() => {
                // Equations:
                // Example: [->+>++>--<<<] should be replaced with:
                //   Equation(1, 1*x[0])
                //   Equation(2, 2*x[0])
                //   Equation(3, -2*x[0])
                //   Set(0, 0)
                // As can be see, putting the loop index set last is important.
                // This is not helped by the fact that we are working in u8, so,
                // what we actually have is:
                // Add(0, 255)
                // Add(1, 1)
                // Add(2, 2)
                // Add(3, 254)
                // But this should be fine in wrapping arithmetics.

                // We need to track multiple writes to the samme cell.
                let mut tape = TapeState::new();

                let mut result: Vec<BbDagNode> = vec![];
                let mut failed = false;
                let body = dag_node.body_mut().unwrap();
                let mut needs_if = false;
                for idx in toposort(&body.graph, None).unwrap() {
                    let node = body.graph.node_weight(idx).unwrap();
                    // To prevent possible accesses before the start of the tape,
                    // we need to make sure to add an if statement around the block if all of:
                    // * We access any index before loop index
                    // * One of those indices are negative the current tape offset
                    //   (since the current tape offset must always be valid)
                    if node.offset < std::cmp::min(loop_idx_offset, 0.into()) {
                        needs_if = true;
                    }

                    match node.op {
                        BbDagOp::Beginning => (),
                        BbDagOp::Loop(_) => unreachable!(),
                        BbDagOp::If(_) => unreachable!(),
                        BbDagOp::AddConst(_) if node.offset == loop_idx_offset => (), // Handled after
                        BbDagOp::AddConst(c) => match tape.get(node.offset) {
                            TapeProvenance::Unknown(_) => {
                                let eqn = Equation::Mem(node.offset)
                                    + Equation::Const(c) * Equation::Mem(loop_idx_offset);
                                result.push(BbDagNode {
                                    op: BbDagOp::Equation(eqn.clone()),
                                    offset: node.offset,
                                });
                                tape.set(node.offset, TapeProvenance::Equation(eqn))
                            }
                            TapeProvenance::Exactly(val) => {
                                result.push(BbDagNode {
                                    op: BbDagOp::Set(c + val),
                                    offset: node.offset,
                                });
                                tape.set(node.offset, TapeProvenance::Exactly(c + val));
                            }
                            TapeProvenance::Equation(_) => {
                                // TODO: Equations. Issue of aliasing with previous loop iterations.
                                failed = true;
                                break;
                            }
                        },
                        BbDagOp::Set(val) => {
                            // Sets needs to be wrapped in conditional code
                            needs_if = true;
                            tape.set(node.offset, TapeProvenance::Exactly(val));
                            result.push(node.clone())
                        }
                        BbDagOp::Equation(_) => {
                            // TODO: Equations. Issue of aliasing with previous loop iterations.
                            failed = true;
                            break;
                            // Equations need to be wrapped conditionally unless we
                            // have a top level add with the self offset.
                            //if let Equation::Add(ref v) = eqn {
                            //    match v.as_slice() {
                            //        [Equation::Mem(a), Equation::Mem(b)]
                            //            if (*a == node.offset && *b == loop_idx_offset)
                            //                || (*b == node.offset && *a == loop_idx_offset) =>
                            //        {
                            //            ()
                            //        }
                            //        _ => needs_if = true,
                            //    }
                            //} else {
                            //    needs_if = true;
                            //}
                            //needs_if = true;
                            //result.push(BbDagNode {
                            //    op: BbDagOp::Equation(
                            //        Equation::Mem(node.offset)
                            //            + eqn.clone() * Equation::Mem(loop_idx_offset),
                            //    ),
                            //    offset: node.offset,
                            //})
                        }
                        BbDagOp::Input => unreachable!(),
                        BbDagOp::Output => unreachable!(),
                        BbDagOp::OutputConst(_) => unreachable!(),
                    }
                }
                if failed {
                    return SimpLoopResult::KeepLoop;
                } else if needs_if {
                    let new_inner_dag: BbDag = result.into();
                    return SimpLoopResult::NewCode(vec![
                        BbDagNode {
                            op: BbDagOp::If(Box::new(new_inner_dag)),
                            offset: loop_idx_offset,
                        },
                        BbDagNode {
                            op: BbDagOp::Set(0.into()),
                            offset: loop_idx_offset,
                        },
                    ]);
                } else {
                    result.push(BbDagNode {
                        op: BbDagOp::Set(0.into()),
                        offset: loop_idx_offset,
                    });
                    return SimpLoopResult::NewCode(result);
                }
            }
            LoopIter::Step(_) => return SimpLoopResult::KeepLoop,
        }
    }
    SimpLoopResult::KeepLoop
}

/// An instruction is dead if all of:
/// * It is not IO
/// * There are no users in the block of this (no outgoing Using-edges)
/// * It is not in the final live set (unless last_block is set).
fn drop_dead(dag: &mut BbDag, last_block: bool) {
    // Live nodes
    let live_set: HashSet<_> = HashSet::from_iter(dag.live.values().cloned());

    'outer: for idx in Vec::from_iter(dag.graph.node_indices()) {
        // If it has a body, be recursive:
        if let Some(body) = dag.graph.node_weight_mut(idx).unwrap().body_mut() {
            drop_dead(body, false);
        }

        // If it is in the live set, we can't drop it
        if !last_block && live_set.contains(&idx) {
            continue;
        }

        // Check if we need to keep this node
        for edge in dag.graph.edges_directed(idx, Direction::Outgoing) {
            match edge.weight() {
                BbDagEdge::Using { .. } => continue 'outer,
                BbDagEdge::Replacing { cond, .. } | BbDagEdge::Clobbering { cond, .. } if *cond => {
                    continue 'outer
                }
                BbDagEdge::Replacing { .. } => (),
                BbDagEdge::Clobbering { .. } => (),
                BbDagEdge::IO => continue 'outer,
            }
        }
        for edge in dag.graph.edges_directed(idx, Direction::Incoming) {
            match edge.weight() {
                BbDagEdge::Using { .. } => (),
                BbDagEdge::Replacing { .. } => (),
                BbDagEdge::Clobbering { .. } => (),
                BbDagEdge::IO => continue 'outer,
            }
        }

        // Node is dead, drop it
        if last_block {
            dag.remove_node_maybe_live(&idx);
        } else {
            dag.remove_node(&idx);
        }
    }
}

/// This pass merges multiple OutputConst into a single one.
fn merge_const_outputs(dag: &mut BbDag) {
    let mut cur_io = dag.last_io;
    while let Some(cur_idx) = cur_io {
        let mut prev_io = None;
        for e in dag.graph.edges_directed(cur_idx, Direction::Incoming) {
            if let BbDagEdge::IO = e.weight() {
                prev_io = Some(e.source());
                break;
            }
        }
        {
            let op2 = &mut dag.graph.node_weight_mut(cur_idx).unwrap().op;
            if let Some(body) = op2.body_mut() {
                merge_const_outputs(body)
            }
        }

        let op2 = &dag.graph.node_weight(cur_idx).unwrap().op;
        if let Some(prev_idx) = prev_io {
            let op1 = &dag.graph.node_weight(prev_idx).unwrap().op;
            if let (BbDagOp::OutputConst(first), BbDagOp::OutputConst(second)) = (op1, op2) {
                let combined = Vec::from_iter(first.iter().chain(second.iter()).cloned());
                let mut added_nodes = dag.replace_with(
                    &prev_idx,
                    vec![BbDagNode {
                        op: BbDagOp::OutputConst(combined),
                        offset: 0.into(),
                    }],
                );
                // Snip out op2
                dag.remove_node(&cur_idx);
                assert_eq!(added_nodes.len(), 1);
                prev_io = added_nodes.pop();
            }
        }
        cur_io = prev_io;
    }
}

/// Perform optimisations on a single basic block
pub(crate) fn opt_simple_block(
    block: SimpleBlock,
    entry: EntryType,
    debug: bool,
    passes: &[BasicBlockOptimisationPass],
) -> SimpleBlock {
    let mut dag: BbDag = block.into();
    for pass in passes {
        if debug {
            dbg!(format!("---- DAG before: {:?}", pass));
            dbg!(dag.to_dot());
        }
        match pass {
            BasicBlockOptimisationPass::SimplifyLoops => simplify_loops(&mut dag),
            BasicBlockOptimisationPass::PropagateEntryConditions => {
                propagate_entry_conditions(&mut dag, entry);
            }
            BasicBlockOptimisationPass::MergeConstantOutputs => merge_const_outputs(&mut dag),
            BasicBlockOptimisationPass::DropDead => {
                drop_dead(&mut dag, entry == EntryType::EndOfProgram)
            }
        }
    }
    if debug {
        dbg!("---- DAG at end");
        dbg!(dag.to_dot());
    }
    dag.into()
}

/// Run passes that only operate on basic blocks, and only on one at a time.
pub fn run_bb_passes(
    ops: Vec<GenOp>,
    is_outermost: bool,
    debug: bool,
    passes: &[BasicBlockOptimisationPass],
) -> Vec<GenOp> {
    ops.into_iter()
        .enumerate()
        .map(|(block_idx, op)| match op.opcode {
            GeneralOp::BasicBlock(inner) => {
                let entry;
                #[allow(clippy::if_same_then_else)]
                if block_idx == 0 && is_outermost {
                    entry = EntryType::BeginningOfProgram
                } else if is_outermost {
                    entry = EntryType::Index0Is0
                } else if !is_outermost && block_idx > 0 {
                    // SAFETY: Moves are shifted to after basic blocks and BBs
                    // merged. Thus there was a seek or unbalanced loop before us.
                    entry = EntryType::Index0Is0
                } else {
                    entry = EntryType::Unknown
                }
                let new_simple_block = opt_simple_block(inner, entry, debug, passes);
                GenOp {
                    offset: op.offset,
                    source_loc: op.source_loc,
                    opcode: GeneralOp::BasicBlock(new_simple_block),
                }
            }
            GeneralOp::UnbalancedLoop { ops } => {
                let new_ops = run_bb_passes(ops, false, debug, passes);
                GenOp {
                    offset: op.offset,
                    source_loc: op.source_loc,
                    opcode: GeneralOp::UnbalancedLoop { ops: new_ops },
                }
            }
            _ => op,
        })
        .collect()
}

#[cfg(test)]
mod tests {}
