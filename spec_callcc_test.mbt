///|
test "r6rs call/cc: simple escape" {
  let program =
    #|(call/cc (lambda (k) (k 1) 2))
  inspect(value_to_string(eval_program(program)), content="1")
}

///|
test "r6rs call/cc: nonlocal exit" {
  let program =
    #|(+ 1 (call/cc (lambda (k) (+ 2 (k 3)))))
  inspect(value_to_string(eval_program(program)), content="4")
}

///|
test "r6rs call/cc: stored continuation" {
  let program =
    #|(let ((escape #f))
    #|  (let ((result (call/cc (lambda (k) (set! escape k) 5))))
    #|    (if (= result 5) (escape 42) result)))
  inspect(value_to_string(eval_program(program)), content="42")
}

///|
test "r6rs call/cc: procedure?" {
  let program =
    #|(procedure? (call/cc (lambda (k) k)))
  inspect(value_to_string(eval_program(program)), content="#t")
}
