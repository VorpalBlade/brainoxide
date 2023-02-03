# BrainOxide

An optimizing [Brainfuck] interpreter/compiler-to-C in Rust.

Made as a project to learn Rust. Code is for sure not idiomatic and quite messy
in places.

Features:

* [Optimisation](#optimisations)
* Fuzz tested
* Regression test suite
* Two backend modes: interpret or generate C code

## Why?

Does the world need yet another BF implementation? Not really. Did I have fun
making it, and did I learn things? Yes.

## Optimisations

BrainOxide performs the following optimisations:

* Instruction merging
* Loop lowering: while → if, while → equations / set
* Constant folding
* IO operation merging: Merges constant outputs into a single byte array output.
* Dead store elimination
* Peephole optimisation: `[>]`, `[>>]`, ... → various seeks

BrainOxide internally differentiates between balanced and unbalanced code
sections. This refers to the amount of `>` vs `<`. In a balanced code sections
we know all offsets, and are thus able to optimise better. In unbalanced
sections we have very limited abilities to optimise.

For balanced code sections BrainOxide builds a directed acyclic graph (DAG) and
performs the optimisation on the DAG.

### Optimisation wish list

* Partial evaluation until first input instruction:

  This could potentially constant fold large programs into a single `fputs()` call.
  It would have to be opt-in. Additionally, it only makes sense for code gen mode.

* Better equation handling:

  Right now the lowering of loops into equations is fairly
  limited and simplistic. The code cannot lower loops already containing equations for
  example, and only a loop index stride of `-1` is supported.

* Loop/If unrolling:

  Unroll loops when we know the iteration count statically, if it makes sense.
  A heuristic would be needed.

## Code stability

This code base does not have a stable API. It is not meant to be used
as a library. The command line flags are not stable either, as there
are some changes I'd like to make to allow more flexible logging options.

This software has only been tested on an up-to-date install of Arch Linux x86-64.
If it happens to work on non-Linux, that is a happy accident.

## Minimum supported rust version (MSRV)

YMMV. It works on rustc 1.67.0 as of writing. It should continue working on
the latest stable Rust (except fuzz testing that needs nightly). I will not
test on older versions than whatever happens to be the latest stable when I
make a change.

So in summary: There is no MSRV policy.

## License

BrainOxide is released under GPL 3.0 (only, not "or later").

[Brainfuck]: https://en.wikipedia.org/wiki/Brainfuck
