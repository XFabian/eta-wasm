# Auto-generated cram tests

  $ wetac --codegen multi_return.eta;
  > wat2wasm multi_return.wat -o multi_return.wasm;
  > wasmer run multi_return.wasm --invoke main -- 0

  $ wetac --codegen simple_arith.eta;
  > wat2wasm simple_arith.wat -o simple_arith.wasm;
  > wasmer run simple_arith.wasm --invoke main -- 0

  $ wetac --codegen simple_array_sum.eta;
  > wat2wasm simple_array_sum.wat -o simple_array_sum.wasm;
  > wasmer run simple_array_sum.wasm --invoke main -- 0

  $ wetac --codegen simple_cond.eta;
  > wat2wasm simple_cond.wat -o simple_cond.wasm;
  > wasmer run simple_cond.wasm --invoke main -- 0

  $ wetac --codegen while_loop_sum.eta;
  > wat2wasm while_loop_sum.wat -o while_loop_sum.wasm;
  > wasmer run while_loop_sum.wasm --invoke main -- 0

