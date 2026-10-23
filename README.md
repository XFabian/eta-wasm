# Wetac

A compiler for the Eta programming language, written in OCaml. It targets
WebAssembly, emitting WAT (the text format) which you then assemble with
`wat2wasm`.

Eta is a small imperative language with 64-bit integers, booleans, arrays,
records and multiple return values. It comes from Cornell's
[CS 4120 Introduction to Compilers](https://www.cs.cornell.edu/courses/cs4120/2026sp/),
which publishes an [Eta language specification](https://www.cs.cornell.edu/courses/cs4120/2026sp/project/language.pdf).
This is an independent implementation written from that spec — the course
targets x86, this one targets WebAssembly.

```
main(args: int[][]): int {
    arr: int[] = {1, 2, 3, 4, 5}
    total: int = 0
    i: int = 0
    while i < length(arr) {
        total = total + arr[i]
        i = i + 1
    }
    return total
}
```

## Building

You need OCaml (4.14 or later) and opam. From a clone:

```sh
opam install . --deps-only
dune build
```

The compiler is then at `_build/default/bin/main.exe`, or run it with
`dune exec wetac --`.

To run the generated code, and to run the test suite, you also need
[wabt](https://github.com/WebAssembly/wabt) for `wat2wasm` and
[wasmer](https://wasmer.io/) to execute the module. On Ubuntu:

```sh
sudo apt install wabt
curl https://get.wasmer.io -sSfL | sh
```

Only the `wasmer` command line tool is needed, not the OCaml bindings.

## Usage

```sh
wetac --codegen program.eta      # writes program.wat
wat2wasm program.wat -o program.wasm
wasmer run program.wasm --invoke main -- 0
```

The compiler can stop after any stage, which is useful for looking at the
intermediate output:

| Flag | Stops after |
| --- | --- |
| `--parse` | parsing, prints the AST |
| `--validate` | name resolution and typechecking, prints the resolved AST |
| `--wtac` | WTac generation, prints the IR |
| `--optimize` | the optimizer, dumps the optimized IR (see Optimizations) |
| `--codegen` | WASM emission (default) |

Programs that allocate need `malloc.wat` linked in. It is a bump allocator
exporting `eta_malloc`, with no `free`.

## Language

- 64-bit `int`, `bool`
- arrays, with literals (`{1, 2, 3}`) and string literals (`"abc"`, which are
  arrays of character codes), `length()`, and multiple dimensions
- records, with field access and nested types
- multiple return values and multiple assignment
- `if` / `else`, `while`, recursion and mutual recursion
- globals, which must be initialized to a constant

## Pipeline

Source is lexed with ocamllex and parsed with Menhir into an AST (`lib/ast.ml`).
`lib/resolve.ml` resolves names and makes them unique, `lib/typecheck.ml` runs
the type checker. The AST is then lowered to WTac (`lib/wtac.ml`), a three
address IR that keeps WASM's structured control flow — it has `block`, `loop`
and `br` rather than arbitrary jumps, so nothing has to be relooped on the way
out. `lib/emit.ml` prints WAT from it.

Ints are `i64`, everything else (`bool`, array and record pointers) is `i32`.
Arrays and records live on the heap behind a pointer.

## Optimizations

`lib/optimizations/` builds a CFG from WTac and reconstructs structured control
flow from it afterwards. On top of that:

- dominator and post-dominator trees
- a generic dataflow solver
- reaching definitions
- liveness
- constant propagation
- dead block removal

These are not wired into codegen. `--codegen` emits WAT straight from the
unoptimized IR. The optimizer runs only under the `--optimize` flag, which dumps
the optimized IR next to the source as `<name>_opt.debug.wtac` for inspection;
it is a work in progress and not part of the default path.

## Tests

```sh
dune test
```

This needs `wat2wasm` and `wasmer` on PATH — the cram tests compile and execute
the generated modules, and will fail without them. See Building above.

There are inline expect tests for the parser and typechecker, and cram tests
that compile `.eta` files end to end and run them under wasmer. The cram tests
in `test/cram.t` expect `main` to return 0; each program checks its own result
and returns non-zero on failure. `test/invalid_parse.t` and
`test/invalid_semantic.t` cover programs that should be rejected.

## Known gaps

- The optimizer is not connected to code generation; it runs only under
  `--optimize` (see above).
- Memory is never freed. There is no `free` and no GC — tolerable for
  short-lived WASM modules that get torn down anyway, but it is a leak.
- Post-dominators do not behave correctly for functions with early returns, see
  `test/cfg_early_return.eta` and the notes in `NOTES.md`.
- `use` statements are not lexed, so the standard library imports in
  `test/cram.t/simple_interp.eta` do not parse.
- Underscore (`_`) as a discard target does not emit correctly.
- Several modules under `lib/` are earlier iterations kept for reference and are
  excluded from the build.

Design notes and open questions are in [NOTES.md](NOTES.md).

## License

MIT, see [LICENSE](LICENSE).
