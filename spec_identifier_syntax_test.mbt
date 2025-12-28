///|
test "r6rs identifier-syntax: reference and set!" {
  let program =
    #|(begin
    #|  (define storage 1)
    #|  (define-syntax counter
    #|    (identifier-syntax
    #|      (counter storage)
    #|      ((set! counter v) (set! storage v))))
    #|  (list counter (begin (set! counter 5) counter)))
  inspect(value_to_string(eval_program(program)), content="(1 5)")
}
