///|
test "r6rs quasiquote: basics and unquote" {
  inspect(value_to_string(eval_program("(quasiquote (1 2 3))")), content="(1 2 3)")
  inspect(value_to_string(eval_program("`(1 ,(+ 1 2) 4)")), content="(1 3 4)")
  inspect(value_to_string(eval_program("`(1 . ,(+ 1 2))")), content="(1 . 3)")
}

///|
test "r6rs quasiquote: unquote-splicing and nesting" {
  inspect(value_to_string(eval_program("`(1 ,@(list 2 3) 4)")), content="(1 2 3 4)")
  let program = "(let ((x 1)) `(a `(b ,x) ,x))"
  inspect(
    value_to_string(eval_program(program)),
    content="(a (quasiquote (b (unquote x))) 1)",
  )
}
