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
test "include source" {
  register_include_source("mem.scm", "(+ 1 2)")
  let value = eval_program("(include \"mem.scm\")")
  inspect(@runtime.value_to_string(value), content="3")
}
```
