///|
test "r6rs syntax: helpers" {
  inspect(
    value_to_string(eval_program(
      (
        #|(syntax->datum (syntax (a b)))
      )
    )),
    content="(a b)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(identifier? 'a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(identifier? '(a b))
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(datum->syntax 'a '(1 2))
      )
    )),
    content="(1 2)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(let ((x 1)) (syntax x))
      )
    )),
    content="x",
  )
}
