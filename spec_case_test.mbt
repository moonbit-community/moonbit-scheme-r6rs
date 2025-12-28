///|
test "r6rs case: datum matching" {
  let program =
    #|(case 2 ((1) 10) ((2 3) 20) (else 30))
  inspect(value_to_string(eval_program(program)), content="20")
  let program2 =
    #|(case 'b ((a) 1) ((b c) 2) (else 3))
  inspect(value_to_string(eval_program(program2)), content="2")
}

///|
test "r6rs case: => clause" {
  let program =
    #|(case 4
    #|  ((1 2 3) 10)
    #|  ((4) => (lambda (x) (+ x 1)))
    #|  (else 0))
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs cond: => clause" {
  let program =
    #|(cond
    #|  ((+ 1 2) => (lambda (x) (+ x 1)))
    #|  (else 0))
  inspect(value_to_string(eval_program(program)), content="4")
}

///|
test "r6rs let: named let" {
  let program =
    #|(let loop ((k 0) (acc 0))
    #|  (if (< k 5)
    #|    (loop (+ k 1) (+ acc k))
    #|    acc))
  inspect(value_to_string(eval_program(program)), content="10")
}

///|
test "r6rs letrec*: sequential init" {
  let program =
    #|(letrec* ((x 1) (y (+ x 2))) y)
  inspect(value_to_string(eval_program(program)), content="3")
}

///|
test "r6rs letrec: mutual recursion" {
  let program =
    #|(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
    #|         (odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))))
    #|  (even? 4))
  inspect(value_to_string(eval_program(program)), content="#t")
}

///|
test "r6rs letrec: uninitialized use" {
  let bad = try? eval_program(
    (
      #|(letrec ((x 1) (y (+ x 2))) y)
    ),
  )
  match bad {
    Err(err) =>
      inspect(err, content="EvalError(\"type error: number expected\")")
    _ => fail("expected EvalError for letrec uninitialized access")
  }
}
