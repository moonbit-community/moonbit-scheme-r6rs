///|
test "r6rs hard: letrec and recursion" {
  let program =
    #|(letrec ((fact (lambda (n)
    #|                  (if (= n 0)
    #|                    1
    #|                    (* n (fact (- n 1)))))))
    #|  (fact 5))
  inspect(value_to_string(eval_program(program)), content="120")
}

///|
test "r6rs hard: and/or short-circuit" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(and #t 1 2)
        ),
      ),
    ),
    content="2",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(and #f unknown)
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(or #t unknown)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(or #f 42)
        ),
      ),
    ),
    content="42",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(not #f)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(not 1)
        ),
      ),
    ),
    content="#f",
  )
}

///|
test "r6rs hard: cond and apply" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(cond ((< 1 0) 1)
          #|      ((< 1 2) 2)
          #|      (else 3))
        ),
      ),
    ),
    content="2",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(apply + '(1 2 3))
        ),
      ),
    ),
    content="6",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(apply (lambda (x y) (* x y)) (list 2 3))
        ),
      ),
    ),
    content="6",
  )
}

///|
test "r6rs hard: eval_program_all" {
  let results = eval_program_all(
    (
      #|(+ 1 2)
      #|(* 3 4)
    ),
  )
  inspect(value_to_string(results[0]), content="3")
  inspect(value_to_string(results[1]), content="12")
}

///|
test "r6rs large: tak" {
  let program =
    #|(letrec ((tak (lambda (x y z)
    #|                (if (<= x y)
    #|                  z
    #|                  (tak (tak (- x 1) y z)
    #|                       (tak (- y 1) z x)
    #|                       (tak (- z 1) x y))))))
    #|  (tak 12 6 0))
  inspect(value_to_string(eval_program(program)), content="1")
}

///|
test "r6rs large: sieve" {
  let program =
    #|(letrec ((remove-multiples (lambda (n lst)
    #|                              (cond ((null? lst) '())
    #|                                    ((= (modulo (car lst) n) 0)
    #|                                     (remove-multiples n (cdr lst)))
    #|                                    (else
    #|                                     (cons (car lst)
    #|                                           (remove-multiples n (cdr lst)))))))
    #|         (sieve (lambda (lst)
    #|                  (if (null? lst)
    #|                      '()
    #|                      (cons (car lst)
    #|                            (sieve (remove-multiples (car lst) (cdr lst))))))))
    #|  (sieve '(2 3 4 5 6 7 8 9 10 11)))
  inspect(value_to_string(eval_program(program)), content="(2 3 5 7 11)")
}

///|
test "r6rs hard: errors" {
  let unbound = try? eval_program(
    (
      #|x
    ),
  )
  match unbound {
    Err(err) => inspect(err, content="EvalError(\"unbound variable: x\")")
    _ => fail("expected EvalError for unbound variable")
  }
  let arity = try? eval_program(
    (
      #|((lambda (x) x) 1 2)
    ),
  )
  match arity {
    Err(err) =>
      inspect(err, content="EvalError(\"arity mismatch: expected 1 got 2\")")
    _ => fail("expected EvalError for arity mismatch")
  }
  let type_err = try? eval_program(
    (
      #|(car 1)
    ),
  )
  match type_err {
    Err(err) => inspect(err, content="EvalError(\"type error: pair expected\")")
    _ => fail("expected EvalError for type error")
  }
}
