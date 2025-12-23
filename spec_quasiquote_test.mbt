///|
test "r6rs quasiquote: basics and unquote" {
  let program = (
    (
      #|(quasiquote (1 2 3))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 3)")

  let program2 = (
    (
      #|`(1 ,(+ 1 2) 4)
    )
  )
  inspect(value_to_string(eval_program(program2)), content="(1 3 4)")

  let program3 = (
    (
      #|`(1 . ,(+ 1 2))
    )
  )
  inspect(value_to_string(eval_program(program3)), content="(1 . 3)")
}

///|
test "r6rs quasiquote: unquote-splicing and nesting" {
  let program = (
    (
      #|`(1 ,@(list 2 3) 4)
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 3 4)")

  let program2 = (
    (
      #|(let ((x 1))
      #|  `(a `(b ,x) ,x))
    )
  )
  inspect(
    value_to_string(eval_program(program2)),
    content="(a (quasiquote (b (unquote x))) 1)",
  )
}
