///|
test "r6rs guard: basic handling" {
  let program =
    #|(guard (ex ((eq? ex 'boom) 'handled)
    #|           (else 'other))
    #|  (raise 'boom))
  inspect(value_to_string(eval_program(program)), content="handled")
}

///|
test "r6rs guard: => clause" {
  let program =
    #|(guard (ex ((begin ex) => (lambda (v) v))
    #|           (else 'no))
    #|  (raise 'ok))
  inspect(value_to_string(eval_program(program)), content="ok")
}

///|
test "r6rs guard: no exception" {
  let program =
    #|(guard (ex (else 'bad)) 42)
  inspect(value_to_string(eval_program(program)), content="42")
}

///|
test "r6rs guard: re-raise" {
  let program =
    #|(guard (ex ((eq? ex 'a) 1))
    #|  (raise 'b))
  let result = try? eval_program(program)
  match result {
    Err(err) => inspect(err, content="EvalError(\"uncaught exception\")")
    _ => fail("expected uncaught exception")
  }
}
