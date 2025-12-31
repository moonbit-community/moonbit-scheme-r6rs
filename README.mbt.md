# Scheme R6RS in MoonBit

This module implements a Scheme R6RS interpreter in MoonBit. It includes a
lexer, parser, macro expander, evaluator, and runtime helpers.

## Layout

- core: runtime types plus Unicode helpers
- lexer: reader and token utilities
- parser: datum parsing and number parsing
- eval: macro expansion and evaluation
- runtime: environment, records, ports, and value helpers

## Usage

```mbt
let program = "(+ 1 2)"
let value = @dii/scheme-r6rs.eval_program(program)
let text = @dii/scheme-r6rs.value_to_string(value)
```

## Tests

```mbt check
///|
test "eval program" {
  let value = eval_program("(+ 1 2)")
  inspect(value_to_string(value), content="3")
  let list_value = eval_program("(list 1 2 3)")
  inspect(value_to_string(list_value), content="(1 2 3)")
  let forms = parse_program("(+ 1 2)")
  inspect(forms.length(), content="1")
  let values = eval_program_all("(define x 1) (+ x 2)")
  inspect(values.length(), content="2")
  register_include_source("mem.scm", "(+ 2 3)")
  let included = eval_program("(include \"mem.scm\")")
  inspect(value_to_string(included), content="5")
  match parse_number_token("10") {
    Some(@core.Datum::Int(10)) => ()
    _ => fail("expected int")
  }
  match parse_number_token_with_radix("ff", 16) {
    Some(@core.Datum::Int(255)) => ()
    _ => fail("expected 255")
  }
}
```

## Development

Use `moon check` to type check, `moon test` to run the spec tests, and `moon fmt`
to format the code.
