///|
test "r6rs macro: vector patterns and templates" {
  let program = (
    (
      #|(define-syntax swap-vec
      #|  (syntax-rules ()
      #|    ((swap-vec #(a b)) (vector b a))))
      #|(swap-vec #(1 2))
    )
  )
  inspect(value_to_string(eval_program(program)), content="#(2 1)")

  let program2 = (
    (
      #|(define-syntax vec
      #|  (syntax-rules ()
      #|    ((vec x ...) #(x ...))))
      #|(vec 1 2 3)
    )
  )
  inspect(value_to_string(eval_program(program2)), content="#(1 2 3)")

  let program3 = (
    (
      #|(define-syntax vec->list
      #|  (syntax-rules ()
      #|    ((vec->list #(a ...)) (list a ...))))
      #|(vec->list #(1 2 3))
    )
  )
  inspect(value_to_string(eval_program(program3)), content="(1 2 3)")
}
