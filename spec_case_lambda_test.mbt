///|
test "r6rs case-lambda: arity dispatch" {
  let program =
    #|((case-lambda ((x) (+ x 1))
    #|             ((x y) (+ x y)))
    #|  3)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="4",
  )
  let program2 =
    #|((case-lambda ((x) (+ x 1))
    #|             ((x y) (+ x y)))
    #|  3 4)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program2)),
    content="7",
  )
}
