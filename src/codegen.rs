//! Code generation

use std::iter::repeat;

use crate::{
    ast::{GenOp, SimpleBlock},
    byte_utils::as_bstr,
    equation::Equation,
};

/// Generate C code
///
/// Tracing turns out adding #line directives that corresponds to the source position.
/// Currently this corresponds to the character index, not the actual line.
pub fn generate_c(code: &Vec<GenOp>, trace: bool) -> String {
    let mut s: String = "#include <stdint.h>\n#include <stdio.h>\n\n".into();
    s += "static uint8_t tape[100000] = { 0 };\nstatic uint8_t *ptr = &tape[0];\n";
    s += "static void seek_0(intptr_t off) { while (*ptr) { ptr += off; } }\n\n";
    s += "int main(void) {\n";
    s += generate_c_inner(code, 1, trace).as_str();
    s += "}\n";
    s
}

/// Recursive code gen for generic unbalanced BF.
fn generate_c_inner(code: &Vec<GenOp>, indentation: i32, trace: bool) -> String {
    let mut s: String = String::new();
    for op in code {
        if trace && op.source_loc != 0 {
            s += format!("{}#line {}\n", indent(indentation), op.source_loc).as_str();
        }
        match op.opcode {
            crate::ast::GeneralOp::Move(n) => {
                s += format!("{}ptr += {};\n", indent(indentation), n).as_str()
            }
            crate::ast::GeneralOp::Seek { step, value } if value == 0.into() => {
                s += format!("{}seek_0({});\n", indent(indentation), step).as_str()
            }
            crate::ast::GeneralOp::Seek { step, value } => {
                s += format!("{}seek({}, {});\n", indent(indentation), step, value).as_str()
            }
            crate::ast::GeneralOp::UnbalancedLoop { ref ops } => {
                s += indent(indentation).as_str();
                s += "while (*ptr) {\n";
                s += generate_c_inner(ops, indentation + 1, trace).as_str();
                s += indent(indentation).as_str();
                s += "}\n";
            }
            crate::ast::GeneralOp::BasicBlock(ref body) => {
                s += generate_c_basic_block(body, indentation, trace).as_str();
            }
        }
    }
    s
}

/// Recursive code gen for a basic block
fn generate_c_basic_block(code: &SimpleBlock, indentation: i32, trace: bool) -> String {
    let mut s: String = String::new();
    for op in &code.ops {
        if trace && op.source_loc != 0 {
            s += format!("{}#line {}\n", indent(indentation), op.source_loc).as_str();
        }
        s += indent(indentation).as_str();
        match op.opcode {
            crate::ast::SimpleOp::BalancedLoop { ref b } => {
                s += format!("while (ptr[{}]) {{\n", op.offset).as_str();
                s += generate_c_basic_block(b, indentation + 1, trace).as_str();
                s += indent(indentation).as_str();
                s += "}\n";
            }
            crate::ast::SimpleOp::If { ref b } => {
                s += format!("if (ptr[{}]) {{\n", op.offset).as_str();
                s += generate_c_basic_block(b, indentation + 1, trace).as_str();
                s += indent(indentation).as_str();
                s += "}\n";
            }
            crate::ast::SimpleOp::Add(n) => s += format!("ptr[{}] += {};\n", op.offset, n).as_str(),
            crate::ast::SimpleOp::Set(n) => s += format!("ptr[{}] = {};\n", op.offset, n).as_str(),
            crate::ast::SimpleOp::EqnSet(ref eqn) => {
                s += format!("ptr[{}] = {};\n", op.offset, format_eqn_c(eqn)).as_str()
            }
            crate::ast::SimpleOp::Input => {
                s += format!("ptr[{}] = getchar();\n", op.offset).as_str();
            }
            crate::ast::SimpleOp::Output => {
                s += format!("putchar(ptr[{}]);\n", op.offset).as_str();
            }
            crate::ast::SimpleOp::OutputConst(ref data) => {
                // TODO: Fix this
                let bstr = as_bstr(data);
                s += format!("fwrite(\"{}\", 1, {}, stdout);\n", bstr, data.len()).as_str();
            }
        }
    }
    s
}

/// Compute the indentation string for a given indentation level.
fn indent(i: i32) -> String {
    String::from_iter(repeat("  ").take(i as usize))
}

/// Format an equation to C code
fn format_eqn_c(eqn: &Equation) -> String {
    match eqn {
        Equation::Mem(n) => format!("ptr[{n}]"),
        Equation::Const(n) => format!("{n}"),
        Equation::Mul(v) => {
            let mut s = String::new();
            let mut is_first = true;
            for e in v {
                if !is_first {
                    s += " * ";
                }
                is_first = false;
                s += format_eqn_c(e).as_str();
            }
            s
        }
        Equation::Add(v) => {
            let mut s = String::new();
            let mut is_first = true;
            s += "(";
            for e in v {
                if !is_first {
                    s += " + ";
                }
                is_first = false;
                s += format_eqn_c(e).as_str();
            }
            s += ")";
            s
        }
    }
}

#[cfg(test)]
mod tests {
    use super::indent;

    #[test]
    fn test_indent() {
        assert_eq!(indent(2), "    ");
    }
}
