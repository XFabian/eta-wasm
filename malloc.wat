(func $eta_malloc (export "eta_malloc") (param $size i32) (result i32)
    (local $idx_aligned i32)
    (local $heap_ptr_val i32)

    ;; heap_ptr_val = heap_ptr
    global.get $__heap_base
    local.set $heap_ptr_val

    ;; idx_aligned = (heap_ptr + 15) & ~15
    local.get $heap_ptr_val
    i32.const 15
    i32.add
    i32.const -16  ;; = ~15 = 0xFFFFFFF0
    i32.and
    local.set $idx_aligned

    ;; heap_ptr = idx_aligned + size
    local.get $idx_aligned
    local.get $size
    i32.add
    global.set $__heap_base

    ;; return idx_aligned
    local.get $idx_aligned
  )