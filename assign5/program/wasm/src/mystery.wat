(module
  (func $mystery (param $n i32) (result i32) (local $x i32)
    (i32.const 0)
    (set_local $x)
    (block $while_loop_break
        (loop $while_loop
            (get_local $x)
            (i32.const 1)
            (i32.add)
            (set_local $x)

            (i32.const 1)
            (get_local $n)
            (i32.eq)
            (br_if $while_loop_break)

            (block $if_break
                (get_local $n)
                (i32.const 2)
                (i32.rem_u)
                (i32.const 0)
                (i32.eq)
                (br_if $if_break)

                (i32.const 3)
                (get_local $n)
                (i32.mul)
                (i32.const 1)
                (i32.add)
                (set_local $n)
                (br $while_loop))

            (get_local $n)
            (i32.const 2)
            (i32.div_u)
            (set_local $n)
            (br $while_loop)))

    (get_local $x)
    (return)
    )
  (export "mystery" (func $mystery))
  )
