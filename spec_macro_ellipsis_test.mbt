///|
test "r6rs macro: ellipsis in templates" {
  let program = (
    #|(define-syntax when
    #|  (syntax-rules ()
    #|    ((when test body ...)
    #|     (if test (begin body ...) #f))))
    #|(let ((x 0))
    #|  (when #t (set! x 1) (set! x (+ x 1)))
    #|  x)
  )
  inspect(value_to_string(eval_program(program)), content="2")

  let program2 = (
    #|(define-syntax when
    #|  (syntax-rules ()
    #|    ((when test body ...)
    #|     (if test (begin body ...) #f))))
    #|(let ((x 0))
    #|  (when #f (set! x 1))
    #|  x)
  )
  inspect(value_to_string(eval_program(program2)), content="0")
}

///|
test "r6rs macro: ellipsis with list patterns" {
  let program = (
    #|(define-syntax pairs
    #|  (syntax-rules ()
    #|    ((pairs (a b) ...) (list (list a b) ...))))
    #|(pairs (1 2) (3 4))
  )
  inspect(value_to_string(eval_program(program)), content="((1 2) (3 4))")
}
