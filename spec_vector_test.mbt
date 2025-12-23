///|
test "r6rs vectors" {
  inspect(value_to_string(eval_program("#(1 2 3)")), content="#(1 2 3)")
  inspect(value_to_string(eval_program("(vector 1 2 3)")), content="#(1 2 3)")
  inspect(value_to_string(eval_program("(vector? #(1 2))")), content="#t")
  inspect(value_to_string(eval_program("(vector-length #(1 2 3))")), content="3")
  inspect(value_to_string(eval_program("(vector-ref #(1 2 3) 1)")), content="2")
  let program = "(let ((v (vector 1 2 3))) (vector-set! v 1 9) v)"
  inspect(value_to_string(eval_program(program)), content="#(1 9 3)")
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

  let bad_byte = try? eval_program("(bytevector 256)")
  match bad_byte {
    Err(err) => inspect(err, content="EvalError(\"type error: byte expected\")")
    _ => fail("expected EvalError for invalid byte")
  }
}
