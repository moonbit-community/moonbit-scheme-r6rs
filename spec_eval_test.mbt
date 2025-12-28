///|
test "r6rs eval and environment" {
  let basic =
    #|(let ((env (environment '(rnrs base))))
    #|  (eval '(+ 1 2) env))
  inspect(value_to_string(eval_program(basic)), content="3")
  let only_prog =
    #|(let ((env (environment '(only (rnrs base) +))))
    #|  (eval '(+ 4 5) env))
  inspect(value_to_string(eval_program(only_prog)), content="9")
  let define_prog =
    #|(let ((env (environment '(rnrs base))))
    #|  (eval '(define x 10) env)
    #|  (eval 'x env))
  inspect(value_to_string(eval_program(define_prog)), content="10")
}

///|
test "r6rs eval: invalid environment" {
  let bad = try? eval_program(
    (
      #|(eval '(+ 1 2) 1)
    ),
  )
  match bad {
    Err(err) =>
      inspect(err, content="EvalError(\"type error: environment expected\")")
    _ => fail("expected EvalError for invalid environment")
  }
}
