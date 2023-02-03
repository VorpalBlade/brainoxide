//! This test all the regressions in the regressions directory.

use std::{
    collections::VecDeque, env, error::Error, io::Read, os::linux::fs::MetadataExt, path::PathBuf,
};

use brainoxide::{
    optimize, parse_source,
    settings::get_fuzz_optimisations,
    test_utils::{compare_tapes, test_execute},
};

fn find_regressions() -> Result<Vec<PathBuf>, Box<dyn Error>> {
    let mut path: PathBuf = env::var("CARGO_MANIFEST_DIR")?.into();
    path.push("tests");
    path.push("regressions");

    let mut results = vec![];
    for entry in std::fs::read_dir(path)? {
        let entry = entry?;
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        if let Some("bf") = path.extension().and_then(|x| x.to_str()) {
            results.push(path);
        }
    }
    Ok(results)
}

fn load_file(path: &PathBuf) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut file = std::fs::File::open(path)?;
    let mut buf = Vec::with_capacity(file.metadata()?.st_size() as usize);
    file.read_to_end(&mut buf)?;
    Ok(buf)
}

fn run_reproducer(
    reproducer: &PathBuf,
    input: Option<PathBuf>,
    expected_output: PathBuf,
) -> Result<(), Box<dyn Error>> {
    let src = load_file(reproducer).unwrap();
    let out = load_file(&expected_output).unwrap();
    let input: VecDeque<_> = if let Some(f) = input {
        load_file(&f).unwrap().into()
    } else {
        VecDeque::new()
    };
    let ast = parse_source(src.as_slice())?;

    let exec1 = test_execute(&ast, &mut input.clone());
    let new_ast = optimize(ast.clone(), false, get_fuzz_optimisations().as_slice());
    let exec2 = test_execute(&new_ast, &mut input.clone());

    assert_eq!(exec1.result, Some(Ok(())));
    assert_eq!(exec2.result, Some(Ok(())));
    assert_eq!(exec1.output, out);
    assert_eq!(exec2.output, out);
    assert_eq!(exec1.tape_ptr, exec2.tape_ptr);
    assert!(compare_tapes(&exec1.tape, &exec2.tape));

    Ok(())
}

#[test]
fn test_regressions() {
    for reproducer in find_regressions().unwrap() {
        let in_file = reproducer.with_extension("in");
        let in_file = if in_file.exists() {
            Some(in_file)
        } else {
            None
        };
        let out_file = reproducer.with_extension("out");
        match run_reproducer(&reproducer, in_file, out_file) {
            Ok(_) => (),
            Err(err) => panic!("{err:?} {reproducer:?}"),
        }
    }
}
