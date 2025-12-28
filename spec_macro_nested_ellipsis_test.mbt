///|
test "r6rs macro: nested ellipsis list" {
  let program =
    #|(define-syntax pack
    #|  (syntax-rules ()
    #|    ((pack ((a ...) ...)) (list (list a ...) ...))))
    #|(pack ((1 2) (3 4 5)))
  inspect(value_to_string(eval_program(program)), content="((1 2) (3 4 5))")
}

///|
test "r6rs macro: nested ellipsis list empty" {
  let program =
    #|(define-syntax pack
    #|  (syntax-rules ()
    #|    ((pack ((a ...) ...)) (list (list a ...) ...))))
    #|(pack (() (1 2)))
  inspect(value_to_string(eval_program(program)), content="(() (1 2))")
}

///|
test "r6rs macro: nested ellipsis vector template" {
  let program =
    #|(define-syntax matrix
    #|  (syntax-rules ()
    #|    ((matrix ((a ...) ...)) (vector (vector a ...) ...))))
    #|(matrix ((1 2) (3 4)))
  inspect(value_to_string(eval_program(program)), content="#(#(1 2) #(3 4))")
}

///|
test "r6rs macro: nested ellipsis vector pattern" {
  let program =
    #|(define-syntax vpack
    #|  (syntax-rules ()
    #|    ((vpack #((a ...) ...)) (list (list a ...) ...))))
    #|(vpack #((1 2) (3 4)))
  inspect(value_to_string(eval_program(program)), content="((1 2) (3 4))")
}
