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
}
```

## Development

Use `moon check` to type check, `moon test` to run the spec tests, and `moon fmt`
to format the code.
