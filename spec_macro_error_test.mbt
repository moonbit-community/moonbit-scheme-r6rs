///|
test "r6rs macro: non-procedure transformer" {
  try eval_program("(define-syntax bad (+ 1 2))") catch {
    err => inspect(err, content="EvalError(\"type error: procedure expected\")")
  } noraise {
    _ => fail("expected transformer type error")
  }
}

///|
test "r6rs macro: invalid identifier-syntax rule" {
  try eval_program("(define-syntax bad (identifier-syntax (x) (y)))") catch {
    err => inspect(err, content="EvalError(\"invalid identifier-syntax\")")
  } noraise {
    _ => fail("expected identifier-syntax error")
  }
}

///|
test "r6rs macro: invalid with-syntax bindings" {
  let program =
    #|(begin
    #|  (define-syntax bad
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_ x) (with-syntax ((x)) #'x)))))
    #|  (bad 1))
  try eval_program(program) catch {
    err => inspect(err, content="EvalError(\"invalid with-syntax\")")
  } noraise {
    _ => fail("expected with-syntax error")
  }
}

///|
test "r6rs macro: with-syntax requires datum" {
  let program =
    #|(begin
    #|  (define-syntax bad
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_)
    #|         (with-syntax ((x (lambda (y) y)))
    #|           #'x)))))
    #|  (bad))
  try eval_program(program) catch {
    err => inspect(err, content="EvalError(\"type error: datum expected\")")
  } noraise {
    _ => fail("expected datum error")
  }
}

///|
test "r6rs macro: invalid syntax-case clause" {
  let program =
    #|(begin
    #|  (define-syntax bad
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        (()))))
    #|  (bad 1))
  try eval_program(program) catch {
    err => inspect(err, content="EvalError(\"invalid syntax-case clause\")")
  } noraise {
    _ => fail("expected syntax-case clause error")
  }
}

///|
test "r6rs macro: invalid syntax-rules literals" {
  try eval_program("(define-syntax bad (syntax-rules (1) ((bad) 1)))") catch {
    err => inspect(err, content="EvalError(\"invalid syntax-rules\")")
  } noraise {
    _ => fail("expected syntax-rules error")
  }
}
