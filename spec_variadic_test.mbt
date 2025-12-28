///|
test "r6rs variadic formals" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(let ((f (lambda (x . rest) rest)))
          #|  (f 1 2 3))
        ),
      ),
    ),
    content="(2 3)",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(let ((f (lambda args args)))
          #|  (f 1 2))
        ),
      ),
    ),
    content="(1 2)",
  )
  let define_prog =
    #|(define (collect a . rest) rest)
    #|(collect 1 2 3 4)
  inspect(value_to_string(eval_program(define_prog)), content="(2 3 4)")
  let case_lambda_prog =
    #|(let ((f (case-lambda
    #|           ((x) x)
    #|           ((x . rest) (cons x rest)))))
    #|  (f 1 2 3))
  inspect(value_to_string(eval_program(case_lambda_prog)), content="(1 2 3)")
  let let_values_prog =
    #|(let-values (((a b . rest) (values 1 2 3 4)))
    #|  rest)
  inspect(value_to_string(eval_program(let_values_prog)), content="(3 4)")
  let let_star_values_prog =
    #|(let*-values (((a b . rest) (values 1 2 3 4)))
    #|  rest)
  inspect(value_to_string(eval_program(let_star_values_prog)), content="(3 4)")
  let define_values_prog =
    #|(let ()
    #|  (define-values (a b . rest) (values 1 2 3))
    #|  rest)
  inspect(value_to_string(eval_program(define_values_prog)), content="(3)")
}
