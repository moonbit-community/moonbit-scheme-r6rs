///|
test "r6rs equality predicates" {
  inspect(value_to_string(eval_program("(eq? 1 1)")), content="#t")
  inspect(value_to_string(eval_program("(eqv? #t #t)")), content="#t")
  inspect(value_to_string(eval_program("(equal? '(1 2) '(1 2))")), content="#t")
  inspect(value_to_string(eval_program("(eq? \"hi\" \"hi\")")), content="#t")
  inspect(value_to_string(eval_program("(eq? 'a 'b)")), content="#f")
  inspect(value_to_string(eval_program("(eq? '(1 2) '(1 3))")), content="#f")
}

///|
test "r6rs list utilities" {
  inspect(value_to_string(eval_program("(list? '())")), content="#t")
  inspect(value_to_string(eval_program("(list? '(1 2 3))")), content="#t")
  inspect(value_to_string(eval_program("(list? '(1 . 2))")), content="#f")
  inspect(value_to_string(eval_program("(length '())")), content="0")
  inspect(value_to_string(eval_program("(length '(1 2 3))")), content="3")
  inspect(value_to_string(eval_program("(append)")), content="()")
  inspect(value_to_string(eval_program("(append '(1 2) '(3 4))")), content="(1 2 3 4)")
  inspect(value_to_string(eval_program("(append '(1 2) '(3 . 4))")), content="(1 2 3 . 4)")
  inspect(value_to_string(eval_program("(reverse '(1 2 3))")), content="(3 2 1)")
}

///|
test "r6rs list utilities: errors" {
  let improper = try? eval_program("(length '(1 . 2))")
  match improper {
    Err(err) => inspect(err, content="EvalError(\"type error: proper list expected\")")
    _ => fail("expected EvalError for improper list")
  }
}
