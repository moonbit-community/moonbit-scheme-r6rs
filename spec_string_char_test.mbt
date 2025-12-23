///|
test "r6rs char and string primitives" {
  inspect(value_to_string(eval_program("#\\a")), content="#\\a")
  inspect(value_to_string(eval_program("#\\space")), content="#\\space")
  inspect(value_to_string(eval_program("#\\newline")), content="#\\newline")
  inspect(value_to_string(eval_program("(char? #\\a)")), content="#t")
  inspect(value_to_string(eval_program("(char? \"a\")")), content="#f")
  inspect(value_to_string(eval_program("(string? \"hi\")")), content="#t")
  inspect(value_to_string(eval_program("(string-length \"hi\")")), content="2")
  inspect(value_to_string(eval_program("(string-append \"a\" \"b\" \"c\")")), content="\"abc\"")
  inspect(value_to_string(eval_program("(string-append)")), content="\"\"")
  inspect(value_to_string(eval_program("(string-ref \"hi\" 0)")), content="#\\h")
  inspect(value_to_string(eval_program("(string-ref \"hi\" 1)")), content="#\\i")
}

///|
test "r6rs string-ref: errors" {
  let out_of_range = try? eval_program("(string-ref \"a\" 1)")
  match out_of_range {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for out of range")
  }
}
