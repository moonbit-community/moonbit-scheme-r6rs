# parser

Datum parser for Scheme source text, built on the lexer reader.

## Entry points

- parse_program parses all top-level forms
- parse_number_token parses number literals

## Example

```mbt
///|
let forms = @parser.parse_program("(+ 1 2)")
```

```mbt check
///|
test "parse basics" {
  let forms = parse_program("(+ 1 2)")
  inspect(forms.length(), content="1")
  match parse_number_token("10") {
    Some(_) => ()
    None => fail("expected number")
  }
  match parse_number_token("3/4") {
    Some(@core.Datum::Rat(3, 4)) => ()
    _ => fail("expected 3/4")
  }
  match parse_number_token("1+2i") {
    Some(@core.Datum::Complex(real, imag)) =>
      match (real.val, imag.val) {
        (@core.Datum::Int(1), @core.Datum::Int(2)) => ()
        _ => fail("expected 1+2i")
      }
    _ => fail("expected complex")
  }
  match parse_number_token("1e2") {
    Some(@core.Datum::Float(f)) => inspect(f == 100.0, content="true")
    _ => fail("expected 100.0")
  }
  match parse_number_token_with_radix("ff", 16) {
    Some(@core.Datum::Int(255)) => ()
    _ => fail("expected 255")
  }
  match parse_program("#\\x41 #\\space") {
    [@core.Datum::Char('A'), @core.Datum::Char(' '), ..] => ()
    _ => fail("expected char literals")
  }
  match parse_program("#vu8(1 2)") {
    [@core.Datum::ByteVector(items), ..] =>
      inspect(items.length(), content="2")
    _ => fail("expected bytevector")
  }
  match parse_program_with_fold_case("ABC", true) {
    [@core.Datum::Symbol("abc"), ..] => ()
    _ => fail("expected folded symbol")
  }
}
```
