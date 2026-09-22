///|
test "r6rs exceptions: raise-continuable resumes" {
  let program =
    #|(with-exception-handler
    #|  (lambda (e) 10)
    #|  (lambda () (+ 1 (raise-continuable 'boom))))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="11",
  )
}

///|
test "r6rs exceptions: nested handlers" {
  let program =
    #|(with-exception-handler
    #|  (lambda (e) 'outer)
    #|  (lambda ()
    #|    (with-exception-handler
    #|      (lambda (e) 'inner)
    #|      (lambda () (raise-continuable 'boom)))))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="inner",
  )
}

///|
test "r6rs exceptions: raise non-continuable" {
  let program =
    #|(with-exception-handler
    #|  (lambda (e) 'handled)
    #|  (lambda () (raise 'boom)))
  try @scheme-r6rs.eval_program(program) catch {
    err => inspect(err, content="EvalError(\"non-continuable exception\")")
  } noraise {
    _ => fail("expected EvalError for non-continuable raise")
  }
}

///|
test "r6rs exceptions: uncaught raise" {
  let program =
    #|(raise 'boom)
  try @scheme-r6rs.eval_program(program) catch {
    err => inspect(err, content="EvalError(\"uncaught exception\")")
  } noraise {
    _ => fail("expected EvalError for uncaught raise")
  }
}
