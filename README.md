# Wetac
This is a compiler implementing the ETA programming language.
The target is WASM or more specifically WAT

An implementation of malloc can be linked

### TODO Tomorrow 16.04
## Stuff to do
- Multi assign 
    - Extend Assign lhs to contain not only expressions but a expressions or var declarations type T = V of var_decl
- Add a tuple Type for functoins so that you can remove the list everywhere
    - This is used only for internal reasoning. This wil change the types of functions on the wtac level, where tuple types do not exist.
    A pointer as the first param will be added. Allocation has to be done by the caller.
- use another eta compiler and compare. Alos use their tests and integrate them
- use cram tests with wasmer to execute the wat files generated

- Extend mem to take an additional offset as argument to ease offset computations
- clean up Wtac generation

- Use LLMs to generate .eta files that are valid and use them as a test suite for the parser. Instruct he LLM to output 0 if the code is correct and something else if not.
For example that the correct branch is executed.

## Tricky CFG
See cfg_early_return.eta for a tricky CFG
Reconstructing that is tricky because the postdominator of the if Header is the exit node which is wrong. So everything after the if
will be put into one of the branches.


Early return would not be that probematic. I nee to modify the blcok to not have the terminator insde them and add the terminator later.
This adds code to an if but this is inherently not wrong for such cases.
This is a little more cleaner htan trying to have the very same structure.

What happens if I early return in a loop?
Also how are excectpions handled in a CFG?




How can I test the performance?
In wasmer? or profile Rust with the wasmer call




- [ ] Implement Beyond Relooper for a more standard TAC that has no loop and block constructs. The analysis is mostly independent of that.
- [ ] Fix tests
## Not working
Underscores emission


MINUS VAR LBRACK VAR LBRACK INT RBRACK RBRACK
MINUS VAR LBRACK VAR RBRACK
### Wasmer stuff



Installation of libwasmer was troublesome
I additionally installed clang right now (did not help)

I installed wasmer 3.0.0 because libwasmer apparently depends on that (checked cargo.toml of libwasmer) (did not help)

I uninstalled wasmer by removing .wasmer folder (did not help)

I reinstalled rustup (did not help)
I did chown -R xaver:xaver /home/xaver/.rustup /home/xaver/.cargo
and chmod -R u+w /home/xaver/.rustup /home/xaver/.cargo
but pretty sure they did not do anything

I installed libwasmer manually by checking the make file and extracting the cargo command there (that worked)

used opam install wasmer (not libwasmer again)
Now it worked.
Need to check again

dune utop is not working because of linking issues?

TODO: Use Wasmer optoin to create-exe to create exe for simple program and then check the disasembly





So no dynamically sized structures? Which can be fine I guess
WASMs memory is manually managed

Dynamic dispatch in wasm https://fitzgen.com/2018/04/26/how-does-dynamic-dispatch-work-in-wasm.html

For Memory 
Wasm can define mutliple memories
Even has something similar to .data in x86 to initialize parts of the memory
 (data (i32.const 0) "Hi")

Virtual dispatch is probably done through tables (See https://developer.mozilla.org/en-US/docs/WebAssembly/Guides/Understanding_the_text_format#multiple_memories tables)
in wasm

and call_indirect

How can we handle recursion in WASM?
local parameters cannot be accessed across stack frames. Like caller save registers
Rust to wasm uses ots own stack management on top of the wasm stack
See https://surma.dev/things/rust-to-webassembly/
Note that the WASM stack is really implicit.




Article on intro for WASM: https://rsms.me/wasm-intro#addressing-memory
and every byte of wasm 
https://danielmangum.com/posts/every-byte-wasm-module/


A [GADT example](https://blog.mads-hartmann.com/ocaml/2015/01/05/gadt-ocaml.html)
### Questions for language
Global initialzier can be only a constant?


#### Problems with arraysdune runt
Declaringg arrays with a size sucks. this sizes are not part of the type so it is shitty to parse. But you ned those specifications to at least do somthing with them.

This is probably checked during type checking or not.
It sucks that the type is used to contain additoinal infomration that is not part of the type

You cannot modify the size of an array here.
We do not need to specify th elengths when we have an initlaizeor. Since we know the size then
Access to an array is a memry location after a call to malloc

you can declare x:int[] but it is not useful except when its a function paramter
you need x = y or something to init that array. Otherwise not usable!

So what is the point?
I get it in functoin arguments.
But not in declarations
allos x: int[n] for dynamic sizes but disallow this [] bullshit
You need th esize to allocate neough mem

I understand now
x[3][] allocates space for 3 pointers to arrays. That is why we assign arrays to x[0] etc.
These arrays themselves can be somewehere else and are not layed out at teh same space
x[3][4] should be layed out in that way. Since we know all sizes statically.

So it is not a problem actually. Still a little bit annoying.
A parameter should not have a size (x : int[3]) not allowed that is not a type!

For students

WE mallo stuff but we are never freeing.
Write this pass to free arrays when they are out of scope and not accessed anymore! So no references.
Of course not always possible unless we do reference counting and GC!

Even when we are not freeing it is not to bad since we are in WASM and it spis up a memory everytime we start th eprogram, which is teared down in the end.
Its a little sandbox




