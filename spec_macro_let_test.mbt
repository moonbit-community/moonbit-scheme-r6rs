///|
test "r6rs macro: let-syntax" {
  let program =
    #|(let-syntax ((inc (syntax-rules () ((inc x) (+ x 1)))))
    #|  (inc 2))
  inspect(value_to_string(eval_program(program)), content="3")
}

///|
test "r6rs macro: let-syntax literals respect bindings" {
  let program =
    #|(let ((x 'outer))
    #|  (let-syntax ((pick (syntax-rules (x)
    #|                       ((_ x) 'literal)
    #|                       ((_ y) 'other))))
    #|    (list (pick x) (let ((x 'inner)) (pick x)))))
  inspect(value_to_string(eval_program(program)), content="(literal other)")
}

///|
test "r6rs macro: letrec-syntax" {
  let program =
    #|(letrec-syntax ((even (syntax-rules ()
    #|                      ((even n) (if (= n 0) #t (odd (- n 1))))))
    #|                  (odd (syntax-rules ()
    #|                     ((odd n) (if (= n 0) #f (even (- n 1)))))))
    #|  (even 4))
  inspect(value_to_string(eval_program(program)), content="#t")
  let program2 =
    #|(letrec-syntax ((even (syntax-rules ()
    #|                      ((even n) (if (= n 0) #t (odd (- n 1))))))
    #|                  (odd (syntax-rules ()
    #|                     ((odd n) (if (= n 0) #f (even (- n 1)))))))
    #|  (even 5))
  inspect(value_to_string(eval_program(program2)), content="#f")
}

///|
test "r6rs macro: letrec-syntax literals respect bindings" {
  let program =
    #|(let ((x 'outer))
    #|  (letrec-syntax ((pick (syntax-rules (x)
    #|                          ((_ x) 'literal)
    #|                          ((_ y) 'other))))
    #|    (list (pick x) (let ((x 'inner)) (pick x)))))
  inspect(value_to_string(eval_program(program)), content="(literal other)")
}
