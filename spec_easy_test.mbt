///|
test "r6rs easy: numbers and booleans" {
  inspect(
    value_to_string(eval_program(
      (
        #|(+ 1 2 3)
      )
    )),
    content="6",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(+)
      )
    )),
    content="0",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(*)
      )
    )),
    content="1",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(- 10)
      )
    )),
    content="-10",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(- 10 3 2)
      )
    )),
    content="5",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(= 2 2 2)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(< 1 2 3)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(< 1 2 2)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|#t
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|#f
      )
    )),
    content="#f",
  )
}

///|
test "r6rs easy: lists and quote" {
  inspect(
    value_to_string(eval_program(
      (
        #|'abc
      )
    )),
    content="abc",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|'()
      )
    )),
    content="()",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|'(1 2 3)
      )
    )),
    content="(1 2 3)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(cons 1 2)
      )
    )),
    content="(1 . 2)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(cons 1 (cons 2 ()))
      )
    )),
    content="(1 2)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(car '(9 8))
      )
    )),
    content="9",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(cdr '(9 8))
      )
    )),
    content="(8)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(null? ())
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(pair? '(1 2))
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(list 1 2 3)
      )
    )),
    content="(1 2 3)",
  )
}

///|
test "r6rs easy: strings" {
  inspect(
    value_to_string(eval_program(
      (
        #|"hi"
      )
    )),
    content="\"hi\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(begin (define s "ok") s)
      )
    )),
    content="\"ok\"",
  )
}
