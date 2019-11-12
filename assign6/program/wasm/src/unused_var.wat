(module 
  (import "wasm-alloc" "alloc" (func $alloc (param i32) (result i32)))
  (import "wasm-alloc" "dealloc" (func $dealloc (param i32)))
  (import "memcpy" "memcpy" (func $memcpy (param i32) (param i32) (param i32)))
  (import "memcpy" "memory" (memory 0))
  (global $length (mut i32) (i32.const 0))
 (func $main  (result i32) (local $x i32) (local $xlen i32) (local $s i32) (local $store i32) (i32.const 5) (call $alloc) (set_local $s) (get_local $s) (i32.const 0) (i32.add) (i32.const 104) (set_local $store) (i32.const 4) (i32.mul) (get_local $store) (i32.store) (get_local $s) (i32.const 1) (i32.add) (i32.const 101) (set_local $store) (i32.const 4) (i32.mul) (get_local $store) (i32.store) (get_local $s) (i32.const 2) (i32.add) (i32.const 108) (set_local $store) (i32.const 4) (i32.mul) (get_local $store) (i32.store) (get_local $s) (i32.const 3) (i32.add) (i32.const 108) (set_local $store) (i32.const 4) (i32.mul) (get_local $store) (i32.store) (get_local $s) (i32.const 4) (i32.add) (i32.const 111) (set_local $store) (i32.const 4) (i32.mul) (get_local $store) (i32.store) (get_local $s) (i32.const 5) (set_local $xlen) (set_local $x)) (export "main" (func $main)))