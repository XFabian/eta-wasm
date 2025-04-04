(module
  (import "env" "print" (func $print (param i32)))
  (func $add (param $a i32) (param $b i32)
    local.get $a
    local.get $b
    i32.add
    call $print
  )
  (export "add_and_print" (func $add))
)
