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
        #|(syntax->datum (datum->syntax 'a '(1 2)))
      )
    )),
    content="(1 2)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(let ((x 1)) (syntax->datum (syntax x)))
      )
    )),
    content="x",
  )
}

///|
test "r6rs syntax: predicates and identifier equality" {
  inspect(
    value_to_string(eval_program(
      (
        #|(syntax? 1)
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(syntax? (syntax (a b)))
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(free-identifier=? 'a 'a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(bound-identifier=? 'a 'b)
      )
    )),
    content="#f",
  )
}
