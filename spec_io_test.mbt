///|
test "r6rs io: display and newline" {
  let program =
    "(let ((p (open-output-string))) (display \"hi\" p) (newline p) (display 123 p) (get-output-string p))"
  inspect(value_to_string(eval_program(program)), content="\"hi\\n123\"")
}

///|
test "r6rs io: write to output string" {
  let program = "(let ((p (open-output-string))) (write 12 p) (get-output-string p))"
  inspect(value_to_string(eval_program(program)), content="\"12\"")
}

///|
test "r6rs io: current-output-port" {
  let program = "(begin (display \"ok\") (get-output-string (current-output-port)))"
  inspect(value_to_string(eval_program(program)), content="\"ok\"")
}
