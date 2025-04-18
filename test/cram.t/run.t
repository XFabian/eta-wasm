# Auto-generated cram tests

  $ wetac --codegen multi_return.eta;
  > wat2wasm multi_return.wat -o multi_return.wasm;
  > wasmer run multi_return.wasm --invoke main -- 0

  $ wetac --codegen program10.eta;
  > wat2wasm program10.wat -o program10.wasm;
  > wasmer run program10.wasm --invoke main -- 0

  $ wetac --codegen program6.eta;
  > wat2wasm program6.wat -o program6.wasm;
  > wasmer run program6.wasm --invoke main -- 0

  $ wetac --codegen program7.eta;
  > wat2wasm program7.wat -o program7.wasm;
  > wasmer run program7.wasm --invoke main -- 0

  $ wetac --codegen program8.eta;
  > wat2wasm program8.wat -o program8.wasm;
  > wasmer run program8.wasm --invoke main -- 0

  $ wetac --codegen program9.eta;
  > wat2wasm program9.wat -o program9.wasm;
  > wasmer run program9.wasm --invoke main -- 0

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

