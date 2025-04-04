(module
    (func $add (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add)
  (func $main (result i32)
    i32.const 100
    i32.const 100
    i32.add
  )

  (export "_start" (func $main))
  (export "add" (func $add))
)