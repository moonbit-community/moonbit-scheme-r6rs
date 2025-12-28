///|
test "r6rs make-parameter: converter" {
  let program =
    #|(begin
    #|  (define p (make-parameter 1 (lambda (x) (+ x 1))))
    #|  (list (p) (parameterize ((p 5)) (p)) (p)))
  inspect(value_to_string(eval_program(program)), content="(2 6 2)")
}

///|
test "r6rs make-parameter: converter on set" {
  let program =
    #|(begin
    #|  (define p (make-parameter 0 (lambda (x) (+ x 2))))
    #|  (p 5)
    #|  (p))
  inspect(value_to_string(eval_program(program)), content="7")
}
