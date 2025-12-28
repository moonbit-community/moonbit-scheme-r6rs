///|
test "r6rs case-lambda: arity dispatch" {
  let program =
    #|((case-lambda ((x) (+ x 1))
    #|             ((x y) (+ x y)))
    #|  3)
  inspect(value_to_string(eval_program(program)), content="4")
  let program2 =
    #|((case-lambda ((x) (+ x 1))
    #|             ((x y) (+ x y)))
    #|  3 4)
  inspect(value_to_string(eval_program(program2)), content="7")
}
