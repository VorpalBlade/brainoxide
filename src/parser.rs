use std::vec;

use thiserror::Error;

use crate::ast::*;
use crate::BfNum;
use crate::TapeAddr;

/// Tokens in source file
#[derive(Debug, PartialEq, Clone, Copy)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
enum Token {
    Left,
    Right,
    Add,
    Subtract,
    Input,
    Output,
    BeginLoop,
    EndLoop,
    EndOfFile,
}

/// Parses source code, producing a stream of tokens.
fn lexer(source_code: &'_ [u8]) -> impl Iterator<Item = (usize, Token)> + '_ {
    // Tokenise and discard unknown tokens
    let tokens = source_code
        .iter()
        .enumerate() // For keeping track of source location
        .filter_map(|(pos, c)| match c {
            b'<' => Some((pos, Token::Left)),
            b'>' => Some((pos, Token::Right)),
            b'+' => Some((pos, Token::Add)),
            b'-' => Some((pos, Token::Subtract)),
            b'.' => Some((pos, Token::Output)),
            b',' => Some((pos, Token::Input)),
            b'[' => Some((pos, Token::BeginLoop)),
            b']' => Some((pos, Token::EndLoop)),
            //b'!' => Some((pos, Token::EndOfFile)),
            _ => None,
        })
        .chain([(0, Token::EndOfFile)]);
    tokens
}

/// Keeps track of parser state. The parser has a stack of these for dealing
/// with loops.
#[derive(Debug)]
struct ParseState {
    /// The current basic block we are building (only balanced stuff)
    pub cur_bb: Vec<SimpOp>,
    /// A stream of general operations in the current context
    pub operations: Vec<GenOp>,
    /// Current offset due to moves
    pub offset: TapeAddr,
    /// Modification to the current cell due to +/-
    pub cur_mod: BfNum,
}

impl ParseState {
    fn new() -> Self {
        Self {
            cur_bb: vec![],
            operations: vec![],
            offset: 0.into(),
            cur_mod: 0.into(),
        }
    }
}

/// Errors during parsing
#[derive(Debug, Error, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    /// To many `[` encountered.
    #[error("Too many start of loops ([) encountered")]
    TooManyStartLoop,
    /// To many `]` encountered.
    #[error("Too many end of loops (]) encountered")]
    TooManyEndLoop,
}

/// Build AST.
///
/// We already do some optimisation here, specifically collapsing multiple
/// values and moves. This makes this code a lot more complex.
fn build_ast(tokens: impl Iterator<Item = (usize, Token)>) -> Result<Vec<GenOp>, ParseError> {
    // This is a stack of parser state. We use a stack to be able to handle loops.
    let mut parse_stack = vec![ParseState::new()];

    for (src_offset, token) in tokens {
        let cstate = parse_stack.last_mut().unwrap();
        match token {
            Token::Add => cstate.cur_mod += 1.into(),
            Token::Subtract => cstate.cur_mod -= 1.into(),
            _ => {
                // Check if we have a pending modification first, if so commit it.
                if cstate.cur_mod != 0.into() {
                    cstate.cur_bb.push(SimpOp {
                        offset: cstate.offset,
                        source_loc: src_offset,
                        opcode: SimpleOp::Add(cstate.cur_mod),
                    });
                    cstate.cur_mod = 0.into();
                }
                match token {
                    Token::Left => cstate.offset -= 1.into(),
                    Token::Right => cstate.offset += 1.into(),
                    Token::Input => cstate.cur_bb.push(SimpOp {
                        offset: cstate.offset,
                        source_loc: src_offset,
                        opcode: SimpleOp::Input,
                    }),
                    Token::Output => cstate.cur_bb.push(SimpOp {
                        offset: cstate.offset,
                        source_loc: src_offset,
                        opcode: SimpleOp::Output,
                    }),
                    _ => {
                        // Check if we need to commit a BB
                        if !cstate.cur_bb.is_empty() {
                            cstate.operations.push(GenOp {
                                offset: 0.into(),
                                source_loc: src_offset,
                                opcode: GeneralOp::BasicBlock(SimpleBlock {
                                    ops: cstate.cur_bb.clone(),
                                }),
                            });
                            cstate.cur_bb.clear();
                        }
                        // Check for moves, and commit them.
                        if cstate.offset != 0.into() {
                            cstate.operations.push(GenOp {
                                offset: 0.into(),
                                source_loc: src_offset,
                                opcode: GeneralOp::Move(cstate.offset),
                            });
                            cstate.offset = 0.into();
                        }

                        match token {
                            Token::BeginLoop => parse_stack.push(ParseState::new()),
                            Token::EndLoop => {
                                let changes_pos = cstate.operations.iter().any(|x| x.changes_pos());
                                let loop_op = if changes_pos {
                                    GenOp {
                                        offset: 0.into(),
                                        source_loc: src_offset,
                                        opcode: GeneralOp::UnbalancedLoop {
                                            ops: cstate.operations.clone(),
                                        },
                                    }
                                } else {
                                    // This is a balanced loop, extract the basic blocks so we can
                                    // put it in a simple loop
                                    let mut bbs: Vec<SimpOp> = vec![];
                                    for bb in cstate.operations.clone() {
                                        if let GeneralOp::BasicBlock(sb) = bb.opcode {
                                            bbs.extend(sb.ops);
                                        } else {
                                            unreachable!();
                                        }
                                    }
                                    GenOp {
                                        offset: 0.into(),
                                        source_loc: src_offset,
                                        opcode: GeneralOp::BasicBlock(SimpleBlock {
                                            ops: vec![SimpOp {
                                                offset: 0.into(),
                                                source_loc: src_offset,
                                                opcode: SimpleOp::new_loop(bbs),
                                            }],
                                        }),
                                    }
                                };
                                parse_stack.pop().unwrap();
                                if parse_stack.is_empty() {
                                    return Err(ParseError::TooManyEndLoop);
                                }
                                parse_stack.last_mut().unwrap().operations.push(loop_op);
                            }
                            Token::EndOfFile => {
                                // The main reason for this token is to make sure everything gets
                                // committed. The second reason is to support ! for embedded input,
                                // just ignore the input after (for now)
                                break;
                            }
                            _ => unreachable!(),
                        };
                    }
                };
            }
        }
    }
    if parse_stack.len() != 1 {
        return Err(ParseError::TooManyStartLoop);
    }
    Ok(parse_stack[0].operations.clone())
}

/// Parse source code into AST
pub fn parse_source(source_code: &[u8]) -> Result<Vec<GenOp>, ParseError> {
    // Tokenise and discard unknown tokens
    let tokens = lexer(source_code);
    // Convert token stream to AST
    build_ast(tokens)
}

#[cfg(test)]
mod tests {
    #[test]
    fn simple_parse() {
        super::parse_source(b"++>->,>.").unwrap();
        super::parse_source(b"++>->,>.>[-]").unwrap();
        super::parse_source(b"++>->,>.>[-[+>]]").unwrap();

        // Everything after ! gets ignored (to support embedded input)
        //super::parse_source(b"+![[").unwrap();

        assert_eq!(
            super::parse_source(b"++>->,>.>[-]]"),
            Err(super::ParseError::TooManyEndLoop)
        );
        assert_eq!(
            super::parse_source(b"++>->,>.>[-]["),
            Err(super::ParseError::TooManyStartLoop)
        );
    }
}
