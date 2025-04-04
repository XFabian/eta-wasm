(module
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))

  (memory (export "memory") 1)
  (data (i32.const 8) "Result: \00")

  (func $print_i32 (param $n i32)
    (i32.store (i32.const 16) (local.get $n))
    (call $fd_write
      (i32.const 1) ;; file descriptor 1 (stdout)
      (i32.const 8) ;; pointer to "Result: "
      (i32.const 1) ;; number of strings
      (i32.const 20) ;; ignored
    )
    drop
  )

  (func $add_and_print (param $a i32) (param $b i32)
    local.get $a
    local.get $b
    i32.add
    call $print_i32
  )
  (export "add_and_print" (func $add_and_print))
)
