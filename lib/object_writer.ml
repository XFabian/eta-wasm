(* Writes WASM objectfiles so they can be consumed by wasm-ld

1. Best case would be to add the cutom lnking section to the generated wasm
file. This is easiest.
Long term we would like to just emit this wasm object file instead
of writing the wat file
https://danielmangum.com/posts/every-byte-wasm-module/ might be helpful
Also the write in Lobster. https://github.com/aardappel/lobster/blob/master/docs/source/implementation_wasm_REMOVED.md

*)



