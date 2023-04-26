#[cfg(target_os = "linux")]
use std::os::linux::fs::MetadataExt;

use std::{
    io::{self, Read},
    path::PathBuf,
};

use thiserror::Error;

use brainoxide::{
    execute, generate_c, optimize, parse_source, settings::get_default_optimisations,
    tape::VecTape, ExecuteCallbackData, ExecuteCallbackResult, ExecutionError, ParseError,
    TapeAddr,
};
use clap::{Parser, ValueEnum};

#[derive(Debug, Error)]
pub enum ProgramError {
    #[error("IO error: {0}")]
    IoError(#[from] io::Error),
    #[error("Parsing error: {0}")]
    ParserError(#[from] ParseError),
    #[error("Execution error: {0}")]
    ExecutionError(#[from] ExecutionError),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Mode {
    /// Interpret the program
    Interpret,
    /// Generate C code for the program
    GenC,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input Brainfuck source file
    input_file: PathBuf,

    /// Select program mode
    #[arg(short, long, value_name = "MODE")]
    mode: Option<Mode>,

    /// Enable optimisation
    #[arg(short, long, default_value_t = false)]
    optimise: bool,

    /// Enable debug output for optimiser
    #[arg(long, default_value_t = false)]
    debug_opt: bool,

    /// Enable debug output for interpreter/code gen
    #[arg(long, default_value_t = false)]
    debug_back_end: bool,
}

fn main() -> Result<(), ProgramError> {
    let args = Args::parse();

    let mut file = std::fs::File::open(args.input_file)?;

    #[cfg(target_os = "linux")]
    let mut buf = Vec::with_capacity(file.metadata()?.st_size() as usize);
    #[cfg(not(target_os = "linux"))]
    let mut buf = Vec::new();

    file.read_to_end(&mut buf)?;

    let mut ast = parse_source(buf.as_slice())?;

    if args.debug_opt {
        dbg!("--- AST BEFORE OPTIMISATION ---");
        dbg!(&ast);
    }

    if args.optimise {
        ast = optimize(ast, args.debug_opt, get_default_optimisations().as_slice());
        if args.debug_opt {
            dbg!("--- AST AFTER OPTIMISATION ---");
            dbg!(&ast);
        }
    }

    let trace = args.debug_back_end;

    match args.mode.unwrap_or(Mode::Interpret) {
        Mode::Interpret => {
            let mut tape = VecTape::new();
            let mut tape_ptr: TapeAddr = 0.into();
            execute(
                &ast,
                &mut tape,
                &mut tape_ptr,
                &mut std::io::stdin().lock(),
                &mut std::io::stdout().lock(),
                &mut |data, _, tp| {
                    if trace {
                        match data {
                            ExecuteCallbackData::GenOp(op) => {
                                dbg!((tp, op));
                            }
                            ExecuteCallbackData::SimpOp(op) => {
                                dbg!((tp, op));
                            }
                            ExecuteCallbackData::InSeek => (),
                            ExecuteCallbackData::InLoop => (),
                        };
                    }
                    ExecuteCallbackResult::Continue
                },
            )?;
        }
        Mode::GenC => {
            println!("{}", generate_c(&ast, args.debug_back_end));
        }
    }

    Ok(())
}
