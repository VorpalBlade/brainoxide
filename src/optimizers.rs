mod bb_opt;

use std::vec;

use crate::{
    ast::*,
    opt_types::EntryType,
    settings::{BasicBlockOptimisationPass, OptimisationPass},
    TapeAddr,
};

use self::bb_opt::opt_simple_block;

/// Run optimisation passes.
pub fn optimize(mut ast: Vec<GenOp>, debug: bool, passes: &[OptimisationPass]) -> Vec<GenOp> {
    for pass in passes {
        if debug {
            dbg!(format!("--- AST before {pass:?}"));
            dbg!(&ast);
        }
        match pass {
            OptimisationPass::ShiftAndFuse => {
                ast = shift_moves_to_end(ast);
                ast = fuse_bbs(ast);
            }
            OptimisationPass::Peephole => {
                ast = peephole(ast, true);
            }
            OptimisationPass::BasicBlock(bb_opts) => {
                ast = bb_opt::run_bb_passes(ast, true, debug, bb_opts.as_slice());
            }
            OptimisationPass::DropUnobservableAtEnd => drop_unobservable_at_end(&mut ast, debug),
        }
    }
    ast
}

pub(crate) fn shift_moves_to_end(ops: Vec<GenOp>) -> Vec<GenOp> {
    // For sure not longer than ops vector
    let mut result: Vec<GenOp> = Vec::with_capacity(ops.len());
    // Keep track of offsets from moves
    let mut offset: TapeAddr = 0.into();
    for gop in ops {
        if let GeneralOp::Move(n) = gop.opcode {
            offset += n;
            continue;
        }
        if let GeneralOp::BasicBlock(mut sb) = gop.opcode {
            sb.offset(offset);
            result.push(GenOp {
                offset: 0.into(),
                source_loc: gop.source_loc,
                opcode: GeneralOp::BasicBlock(sb),
            });
            continue;
        }
        // Commit move otherwise
        if !offset.is_zero() {
            result.push(GenOp::synth(GeneralOp::Move(offset), 0.into()));
            offset = 0.into();
        }
        if let GeneralOp::UnbalancedLoop { ops: _ } = gop.opcode {
            result.push(rec_uloop(gop, shift_moves_to_end));
            continue;
        }
        result.push(gop);
    }
    if !offset.is_zero() {
        result.push(GenOp::synth(GeneralOp::Move(offset), 0.into()));
    }
    result
}

fn peephole(ops: Vec<GenOp>, is_top_level: bool) -> Vec<GenOp> {
    // For sure not longer than ops vector
    let mut result: Vec<GenOp> = Vec::with_capacity(ops.len());
    for gop in ops {
        match gop.opcode {
            GeneralOp::Move(_) => result.push(gop),
            GeneralOp::Seek { step: _, value: _ } => result.push(gop),
            GeneralOp::UnbalancedLoop { ref ops } => {
                if ops.len() == 1 {
                    let iop = ops.get(0).unwrap();
                    match iop.opcode {
                        GeneralOp::Move(n) => result.push(GenOp {
                            offset: gop.offset,
                            source_loc: gop.source_loc,
                            opcode: GeneralOp::Seek {
                                step: n,
                                value: 0.into(),
                            },
                        }),
                        GeneralOp::BasicBlock(ref sb) => {
                            result.push(GenOp {
                                opcode: GeneralOp::BasicBlock(SimpleBlock {
                                    ops: vec![SimpOp {
                                        offset: iop.offset,
                                        source_loc: gop.source_loc,
                                        opcode: SimpleOp::BalancedLoop { b: sb.clone() },
                                    }],
                                }),
                                offset: iop.offset,
                                source_loc: gop.source_loc,
                            });
                        }
                        _ => result.push(rec_uloop(gop, |x| peephole(x, false))),
                    }
                } else {
                    result.push(rec_uloop(gop, |x| peephole(x, false)))
                }
            }
            GeneralOp::BasicBlock(_) => result.push(gop),
        }
    }
    // Final move/seek is unobservable, though this may change the program from
    // erroring to not erroring. Many optimisations might change that
    // (eg <.>> into > at the start of the program), and that is not considered
    // a bug.
    if is_top_level && !result.is_empty() {
        match result.last().unwrap().opcode {
            GeneralOp::Move(_) => {
                result.pop();
            }
            GeneralOp::Seek { step: _, value: _ } => {
                result.pop();
            }
            GeneralOp::UnbalancedLoop { ops: _ } => (),
            GeneralOp::BasicBlock(_) => (),
        }
    }
    result
}

fn drop_unobservable_at_end(ops: &mut Vec<GenOp>, debug: bool) {
    // Handle trailing blocks specially (drop unobservable behaviour).
    while let Some(last_op) = ops.last() {
        match last_op.opcode {
            GeneralOp::Move(_) => {
                ops.pop();
            }
            GeneralOp::Seek { .. } => break,
            GeneralOp::UnbalancedLoop { .. } => break,
            GeneralOp::BasicBlock(ref inner) => {
                let new_simple_block = opt_simple_block(
                    inner.clone(),
                    EntryType::EndOfProgram,
                    debug,
                    &[BasicBlockOptimisationPass::DropDead],
                );
                let new_op = GenOp {
                    offset: last_op.offset,
                    source_loc: last_op.source_loc,
                    opcode: GeneralOp::BasicBlock(new_simple_block),
                };
                ops.pop();
                ops.push(new_op);
                break;
            }
        }
    }
}

/// Recursive unbalanced loop pattern
fn rec_uloop<F>(op: GenOp, func: F) -> GenOp
where
    F: Fn(Vec<GenOp>) -> Vec<GenOp>,
{
    if let GeneralOp::UnbalancedLoop { ops } = op.opcode {
        let newops = func(ops);
        GenOp {
            opcode: GeneralOp::UnbalancedLoop { ops: newops },
            source_loc: op.source_loc,
            offset: op.offset,
        }
    } else {
        unreachable!();
    }
}

pub(crate) fn fuse_bbs(ops: Vec<GenOp>) -> Vec<GenOp> {
    // For sure not longer than ops vector
    let mut result: Vec<GenOp> = Vec::with_capacity(ops.len());
    let mut bb_acc: Option<SimpleBlock> = None;
    for gop in ops {
        if let GeneralOp::BasicBlock(mut sb) = gop.opcode {
            if bb_acc.is_some() {
                bb_acc.as_mut().unwrap().ops.append(&mut sb.ops);
            } else {
                bb_acc = Some(sb);
            }
            continue;
        }
        if bb_acc.is_some() {
            result.push(GenOp {
                offset: 0.into(),
                source_loc: 0,
                opcode: GeneralOp::BasicBlock(bb_acc.take().unwrap()),
            });
        }
        if let GeneralOp::UnbalancedLoop { ops: _ } = gop.opcode {
            result.push(rec_uloop(gop, fuse_bbs));
            continue;
        }
        result.push(gop);
    }
    if bb_acc.is_some() {
        result.push(GenOp {
            offset: 0.into(),
            source_loc: 0,
            opcode: GeneralOp::BasicBlock(bb_acc.take().unwrap()),
        });
    }
    result
}

#[cfg(test)]
mod tests {
    use crate::{ast::*, parse_source};

    #[test]
    fn test_shift_moves_to_end() {
        let parsed_ast = parse_source(b"++>[-]>++").unwrap();
        assert_eq!(parsed_ast.len(), 5);
        let result = super::shift_moves_to_end(parsed_ast);
        assert_eq!(result.len(), 4);
        assert_eq!(result.last().unwrap().opcode, GeneralOp::Move(2.into()));
    }

    #[test]
    fn test_fuse_bbs() {
        let parsed_ast = parse_source(b"++[-]++").unwrap();
        assert_eq!(parsed_ast.len(), 3);
        let result = super::fuse_bbs(parsed_ast);
        assert_eq!(result.len(), 1);
    }
}
