///|
test "r6rs vectors" {
  inspect(value_to_string(eval_program("#(1 2 3)")), content="#(1 2 3)")
  inspect(value_to_string(eval_program("(vector 1 2 3)")), content="#(1 2 3)")
  inspect(value_to_string(eval_program("(make-vector 3 'a)")), content="#(a a a)")
  inspect(value_to_string(eval_program("(vector-length (make-vector 4))")), content="4")
  inspect(value_to_string(eval_program("(vector? #(1 2))")), content="#t")
  inspect(value_to_string(eval_program("(vector-length #(1 2 3))")), content="3")
  inspect(value_to_string(eval_program("(vector-ref #(1 2 3) 1)")), content="2")
  let program = "(let ((v (vector 1 2 3))) (vector-set! v 1 9) v)"
  inspect(value_to_string(eval_program(program)), content="#(1 9 3)")
  let fill_program = "(let ((v (vector 1 2 3))) (vector-fill! v 0) v)"
  inspect(value_to_string(eval_program(fill_program)), content="#(0 0 0)")
  let fill_range_program =
    "(let ((v (vector 1 2 3 4))) (vector-fill! v 9 1 3) v)"
  inspect(value_to_string(eval_program(fill_range_program)), content="#(1 9 9 4)")
  inspect(value_to_string(eval_program("(vector-copy #(1 2 3))")), content="#(1 2 3)")
  inspect(value_to_string(eval_program("(vector-copy #(1 2 3) 1)")), content="#(2 3)")
  inspect(value_to_string(eval_program("(vector-copy #(1 2 3) 1 2)")), content="#(2)")
  inspect(value_to_string(eval_program("(vector->list #(1 2 3))")), content="(1 2 3)")
  inspect(value_to_string(eval_program("(list->vector '(1 2 3))")), content="#(1 2 3)")
}

///|
test "r6rs bytevectors" {
  inspect(value_to_string(eval_program("#vu8(1 2 3)")), content="#vu8(1 2 3)")
  inspect(value_to_string(eval_program("(bytevector 1 2 3)")), content="#vu8(1 2 3)")
  inspect(value_to_string(eval_program("(bytevector? #vu8(1 2))")), content="#t")
  inspect(value_to_string(eval_program("(bytevector-length #vu8(1 2 3))")), content="3")
  inspect(value_to_string(eval_program("(bytevector-u8-ref #vu8(1 2 3) 2)")), content="3")
  let program = "(let ((b (bytevector 1 2))) (bytevector-u8-set! b 1 9) b)"
  inspect(value_to_string(eval_program(program)), content="#vu8(1 9)")
}

///|
test "r6rs vectors/bytevectors: errors" {
  let out_of_range = try? eval_program("(vector-ref #(1 2) 2)")
  match out_of_range {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for out of range")
  }

  let bad_make = try? eval_program("(make-vector -1)")
  match bad_make {
    Err(err) => inspect(err, content="EvalError(\"type error: exact nonnegative integer expected\")")
    _ => fail("expected EvalError for negative length")
  }

  let bad_copy = try? eval_program("(vector-copy #(1 2 3) 2 1)")
  match bad_copy {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for invalid range")
  }

  let bad_fill = try? eval_program("(vector-fill! #(1 2 3) 0 3 2)")
  match bad_fill {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for invalid fill range")
  }

  let bad_list = try? eval_program("(list->vector '(1 . 2))")
  match bad_list {
    Err(err) => inspect(err, content="EvalError(\"type error: proper list expected\")")
    _ => fail("expected EvalError for improper list")
  }

  let bad_byte = try? eval_program("(bytevector 256)")
  match bad_byte {
    Err(err) => inspect(err, content="EvalError(\"type error: byte expected\")")
    _ => fail("expected EvalError for invalid byte")
  }
}
