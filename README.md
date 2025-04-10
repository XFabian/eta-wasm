aTesting how compilation works to WebAssembly 


Starting with simple expressoins and going to variables etc.

Maybe follow course with BOA language and stuff

- [ ] Compile expression language down to real web assembly that can be executed

This targets the WASM text format, which uses S expressions (has the ending .wat)
The binary fomrat has the ending (.wasm)

Using wasmer and external functions it is trivial to instrument the generated wasm code which makes it easy to inspect.
Maybe using debug prints defined in Ocaml and then inserted there


I want to use WASI once to make the external function and not do it in Ocaml or Rust
An example is here: https://blog.dkwr.de/development/wasi-load-fd-write/
see hello.wat for it

We cannot execute system calls inside wasm. That is not a capability
So instead people defined the WebAssembly System Interface(WASI). which is also still developing
It is an abstraction layer that different runtimes can now implement for systems. For example
there is an fd_write in WASI. This can be mapped to the linux system call by a runtime to provide write functoinality to the OS!
However, this abstraction can also be used in ht ebroweser. By providing a seperate implementation of the WASI fd_write function suitable for th ebrowser. This abstraction allows to write different implementatoins and you could even write your own!
So it provides an interface
See here for soe explanations: https://blog.dkwr.de/development/wasi-rust/
and a more technical explanation here: https://hacks.mozilla.org/2019/03/standardizing-wasi-a-webassembly-system-interface/ This article seems very good!

Now, there are runtimes that implement the WASI specification which allow you to execute a WASM module natively


In my opinion the Wsmer rust lib seems better to use. 
Maybe we can provide print utilites that the students can use and we create it with our rust library.
Students can provide their own printers as well

See the following example: https://github.com/wasmerio/wasmer/blob/main/examples/hello_world.rs

### Wasmer stuff

wasmer run add.wasm --invoke add 5 3

invokes a specific function if start is not registered

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

Also there are validation methods on the wasm code. That you can run for checks of your wasm files!

### Entry Point
WASM has a start directive (See here: https://webassembly.github.io/spec/core/syntax/modules.html#syntax-start)
However, it is used of rht efollwing things:
Initializing global variables

Setting up memory or tables

Performing setup tasks before exports are used

Not meant for main program logic!

In WASI (preview 1) we have the _start (Similar to POSIX) as entry point
Just add (export "_start" (func $main)) in the end 

### Running in the web 
See index.html for a simple test
Spin up a http server
For example with python3 -m http.server 8000


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

There is a whole tool conventoins repo https://github.com/WebAssembly/tool-conventions/
that diusses tuff like ABI and a global sta k pointer


https://blog.dkwr.de/development/wasm-variable-instructions/
We can introduce local variables using 
(local i32)
or 
(local $x i32)


So how about structs? 

S

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
A parameter should not have a size (x : int[3]) not allowed that is not a type