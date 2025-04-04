;; hello.wat
(module
    ;; define the expected type for fd_write
    (type $write_type (func (param i32 i32 i32 i32) (result i32)))
    (import "wasi_snapshot_preview1" "fd_write" (func $write (type $write_type)))
    
    ;; Define a memory that is one page in size (64KiB).
    (memory (export "memory") 1)
    
    ;; At offset 66 and 80 in the memory, we store the data.
    (data (i32.const 66) "Hello, World!\n")
    (data (i32.const 80) "Howdy!\n")
    
    
    (func $main (export "hello") (result i32)
        ;; Store the iovs
        (i32.store (i32.const 0) (i32.const 66)) ;; Start of data (= *buf)
        (i32.store (i32.const 4) (i32.const 14)) ;; Length of data (= buf_len)

        (i32.store (i32.const 8) (i32.const 80)) ;; Start of data (= *buf)
        (i32.store (i32.const 12) (i32.const 8)) ;; Length of data (= buf_len)

        i32.const 1 ;; fd param. In Unix this means: 0 = stdin, 1 = stdout, 2 = stderr

        ;; here are different combinations possible:
        ;; 0,1 would read only "Hello, World!"
        ;; 8,1 would read only 'Howdy'
        i32.const 0 ;; iovs -> Offset. Points to first iovec
        i32.const 2 ;; iovs_len -> How many iovs should be read, set it to 2 to read also Howdy

        i32.const 92 ;; Pointer to the place in memory where the number of written bytes shall be placed.

        call $write ;; call fd_write and drop the result code
        drop

        (i32.load (i32.const 92)) ;; output the number of bytes written
    )
)