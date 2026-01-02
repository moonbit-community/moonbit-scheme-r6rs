# eval

Evaluator and macro expander for Scheme R6RS.

## Entry points

- eval_program and eval_program_all evaluate full source strings
- register_include_source supplies in-memory (include ...) content

## Example

```mbt
///|
let program = "(define x 1) (+ x 2)"

///|
let result = @eval.eval_program(program)
```

```mbt check
///|
test "evaluate program" {
  let value = eval_program("(+ 1 2)")
  inspect(@runtime.value_to_string(value), content="3")
  let values = eval_program_all("(define x 1) (+ x 2)")
  inspect(values.length(), content="2")
  inspect(@runtime.value_to_string(values[1]), content="3")
}

///|
test "evaluate program returns last value" {
  let value = eval_program("(begin (define x 1) (+ x 4))")
  inspect(@runtime.value_to_string(value), content="5")
}

///|
test "empty program" {
  let values = eval_program_all("")
  inspect(values.length(), content="0")
}

///|
test "empty program value" {
  let value = eval_program("")
  inspect(@runtime.value_to_string(value), content="#<void>")
}

///|
test "eval error" {
  let result = try? eval_program("(car 1)")
  inspect(result is Err(_), content="true")
}

///|
test "numeric comparisons" {
  let value = eval_program("(and (= 1 1 1) (< 1 2 3) (>= 3 2 1))")
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "fixnum comparisons" {
  let value = eval_program("(and (fx=? 1 1 1) (fx<? 1 2 3) (fx>=? 3 2 1))")
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "bitwise reverse bit field" {
  let value = eval_program("(bitwise-reverse-bit-field #b1011 0 4)")
  inspect(@runtime.value_to_string(value), content="13")
}

///|
test "bitwise folds" {
  let value = eval_program("(bitwise-and #b1111 #b1100 #b1010)")
  inspect(@runtime.value_to_string(value), content="8")
}

///|
test "map over lists" {
  let value = eval_program("(map + '(1 2) '(3 4))")
  inspect(@runtime.value_to_string(value), content="(4 6)")
}

///|
test "let bindings" {
  let value = eval_program("(let ((x 2) (y 3)) (+ x y))")
  inspect(@runtime.value_to_string(value), content="5")
}

///|
test "flonum min/max" {
  let value = eval_program("(list (flmax 1.0 2.5 2.0) (flmin 1.0 -1.0 0.0))")
  inspect(@runtime.value_to_string(value), content="(2.5 -1)")
}

///|
test "bytevector copy" {
  let value = eval_program("(bytevector-copy #vu8(1 2 3) 1 3)")
  inspect(@runtime.value_to_string(value), content="#vu8(2 3)")
}

///|
test "syntax-rules macro" {
  let program =
    "(begin (define-syntax add2 (syntax-rules () ((add2 x) (+ x 2)))) (add2 3))"
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="5")
}

///|
test "include source" {
  register_include_source("mem.scm", "(+ 1 2)")
  let value = eval_program("(include \"mem.scm\")")
  inspect(@runtime.value_to_string(value), content="3")
}

///|
test "primitive output helpers" {
  let value = eval_program(
    "(begin (write 1) (newline) (get-output-string (current-output-port)))",
  )
  inspect(@runtime.value_to_string(value), content="\"1\\n\"")
}

///|
test "primitive arity errors" {
  let display_err = try? eval_program("(display 1 2 3)")
  inspect(display_err is Err(_), content="true")
  let write_err = try? eval_program("(write)")
  inspect(write_err is Err(_), content="true")
  let newline_err = try? eval_program("(newline 1 2)")
  inspect(newline_err is Err(_), content="true")
  let open_out_err = try? eval_program("(open-output-string 1)")
  inspect(open_out_err is Err(_), content="true")
  let get_out_err = try? eval_program("(get-output-string)")
  inspect(get_out_err is Err(_), content="true")
  let current_err = try? eval_program("(current-output-port 1)")
  inspect(current_err is Err(_), content="true")
  let not_err = try? eval_program("(not)")
  inspect(not_err is Err(_), content="true")
}

///|
test "syntax helper errors" {
  let var_err = try? eval_program("(make-variable-transformer 1)")
  inspect(var_err is Err(_), content="true")
  let temp_err = try? eval_program("(generate-temporaries '(1))")
  inspect(temp_err is Err(_), content="true")
}
```
