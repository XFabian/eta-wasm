# Notes

Working notes, open questions and references. Not documentation — see the README
for that.

## Ideas / things to do

- Multi assign: extend the `Assign` lhs to hold either expressions or var
  declarations, i.e. `type T = V of var_decl | E of expr`.
- Add a tuple type for functions so the lists can go away everywhere. This is
  only for internal reasoning — tuple types do not exist at the WTac level,
  where a pointer gets added as the first param and the caller allocates.
- Take another Eta compiler and compare against it. Pull in their tests.
- Extend `Mem` to take an offset argument, to ease offset computations. Same for
  load and store.
- Clean up WTac generation.
- Delete unused functions (as a pass).
- Write a REPL that evaluates terms and puts them into a global context.
- Use an LLM to generate valid `.eta` files as a parser test suite, instructing
  it to return 0 when the program is correct and something else otherwise (e.g.
  that the right branch was taken).
- Use wasmer's `create-exe` on a simple program and check the disassembly.
- How do I test performance? In wasmer, or profile Rust through the wasmer call?

## Tricky CFG

See `test/cfg_early_return.eta`. Reconstructing it is tricky because the
postdominator of the if-header is the exit node, which is wrong — so everything
after the `if` gets put into one of the branches.

Also: how are exceptions handled in a CFG?

See the `KEEP*.png` files for the CFG shapes this refers to.

## Not working

- Underscore emission.
- These two don't parse:
  ```
  MINUS VAR LBRACK VAR LBRACK INT RBRACK RBRACK
  MINUS VAR LBRACK VAR RBRACK
  ```

## Problems with arrays

Declaring arrays with a size is unpleasant. The sizes are not part of the type,
so they are awkward to parse, but you need the specification to do anything with
them. It is annoying that the type ends up carrying information that is not part
of the type.

You cannot change the size of an array. You don't need to specify the length
when there is an initializer, since the size is known then. Access to an array is
a memory location after a call to malloc.

You can declare `x: int[]` but it is not useful except as a function parameter —
you need `x = y` or something to initialize it, otherwise it is unusable. So
what is the point? It makes sense for function arguments, but not declarations.
Better: allow `x: int[n]` for dynamic sizes and disallow the bare `[]` form. You
need the size to allocate enough memory.

Later: I understand it now. `x[3][]` allocates space for 3 pointers to arrays,
which is why you assign arrays to `x[0]` etc. Those arrays live elsewhere and
are not laid out contiguously. `x[3][4]` should be laid out contiguously, since
all sizes are known statically. So it is not really a problem, just a bit
annoying.

A parameter should not have a size — `(x: int[3])` should not be allowed, that
is not a type.

## Memory

We malloc but never free. Write a pass to free arrays when they go out of scope
and are no longer referenced. Not always possible without reference counting and
a GC.

Even without freeing it is not so bad: in WASM the memory is spun up on every
program start and torn down at the end. It is a little sandbox.

No dynamically sized structures — which is probably fine. WASM's memory is
manually managed.

WASM can define multiple memories, and has something like `.data` in x86 for
initializing parts of memory: `(data (i32.const 0) "Hi")`.

## Recursion in WASM

Local parameters cannot be accessed across stack frames, like caller-saved
registers. Rust-to-WASM uses its own stack management on top of the WASM stack
(see https://surma.dev/things/rust-to-webassembly/). Note the WASM stack is
really implicit.

## Virtual dispatch

Probably done through tables and `call_indirect`.

- https://fitzgen.com/2018/04/26/how-does-dynamic-dispatch-work-in-wasm.html
- https://developer.mozilla.org/en-US/docs/WebAssembly/Guides/Understanding_the_text_format#multiple_memories

## Questions for the language

Can a global initializer only be a constant?

## References

- LLVM optimization passes: https://llvm.org/docs/Passes.html
- Overview of different analyses:
  https://www.cs.colostate.edu/~cs553/ClassNotes/lecture05-dataflow-CSE.ppt.pdf
- WASM intro: https://rsms.me/wasm-intro#addressing-memory
- Every byte of a WASM module:
  https://danielmangum.com/posts/every-byte-wasm-module/
- GADT example:
  https://blog.mads-hartmann.com/ocaml/2015/01/05/gadt-ocaml.html

## Misc

`dune utop` is not working, linking issues?
