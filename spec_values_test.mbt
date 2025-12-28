///|
test "r6rs values: call-with-values" {
  let program =
    #|(call-with-values
    #|  (lambda () (values 1 2))
    #|  (lambda (x y) (+ x y)))
  inspect(value_to_string(eval_program(program)), content="3")
  let program2 =
    #|(call-with-values
    #|  (lambda () 5)
    #|  (lambda (x) (+ x 1)))
  inspect(value_to_string(eval_program(program2)), content="6")
}

///|
test "r6rs values: let-values vs let*-values" {
  let program =
    #|(let ((x 1))
    #|  (let-values (((x) 2) ((y) x))
    #|    (+ x y)))
  inspect(value_to_string(eval_program(program)), content="3")
  let program2 =
    #|(let ((x 1))
    #|  (let*-values (((x) 2) ((y) x))
    #|    (+ x y)))
  inspect(value_to_string(eval_program(program2)), content="4")
}

///|
test "r6rs values: define-values" {
  let program =
    #|(begin
    #|  (define-values (x y) (values 10 20))
    #|  (+ x y))
  inspect(value_to_string(eval_program(program)), content="30")
}
