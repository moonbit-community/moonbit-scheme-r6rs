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
  match parse_number_token("-10") {
    Some(@core.Datum::Int(-10)) => ()
    _ => fail("expected -10")
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
  match parse_number_token("1.5") {
    Some(@core.Datum::Float(f)) => inspect(f == 1.5, content="true")
    _ => fail("expected 1.5")
  }
  match parse_number_token("ff", radix=16) {
    Some(@core.Datum::Int(255)) => ()
    _ => fail("expected 255")
  }
  match parse_program("#\\x41 #\\space") {
    [@core.Datum::Char('A'), @core.Datum::Char(' '), ..] => ()
    _ => fail("expected char literals")
  }
  match parse_program("#t #f") {
    [@core.Datum::Bool(true), @core.Datum::Bool(false), ..] => ()
    _ => fail("expected booleans")
  }
  match parse_program("\"hi\"") {
    [@core.Datum::String(text), ..] => inspect(text.val, content="hi")
    _ => fail("expected string")
  }
  match parse_program("#vu8(1 2)") {
    [@core.Datum::ByteVector(items), ..] =>
      inspect(items.length(), content="2")
    _ => fail("expected bytevector")
  }
  match parse_program("#(1 2)") {
    [@core.Datum::Vector(items), ..] => inspect(items.length(), content="2")
    _ => fail("expected vector")
  }
  match parse_program("(a . b)") {
    [@core.Datum::Pair(car, cdr), ..] =>
      match (car.val, cdr.val) {
        (@core.Datum::Symbol("a"), @core.Datum::Symbol("b")) => ()
        _ => fail("expected dotted pair")
      }
    _ => fail("expected dotted pair")
  }
  match parse_program("(a ;comment\n b)") {
    [@core.Datum::Pair(car, cdr), ..] =>
      match (car.val, cdr.val) {
        (@core.Datum::Symbol("a"), @core.Datum::Pair(next, tail)) =>
          match (next.val, tail.val) {
            (@core.Datum::Symbol("b"), @core.Datum::Nil) => ()
            _ => fail("expected (a b)")
          }
        _ => fail("expected (a b)")
      }
    _ => fail("expected list")
  }
  match parse_program("ABC", fold_case=true) {
    [@core.Datum::Symbol("abc"), ..] => ()
    _ => fail("expected folded symbol")
  }
}

///|
test "parse quote" {
  match parse_program("'a") {
    [@core.Datum::Pair(car, cdr), ..] =>
      match (car.val, cdr.val) {
        (@core.Datum::Symbol("quote"), @core.Datum::Pair(expr, tail)) =>
          match (expr.val, tail.val) {
            (@core.Datum::Symbol("a"), @core.Datum::Nil) => ()
            _ => fail("expected (quote a)")
          }
        _ => fail("expected (quote a)")
      }
    _ => fail("expected quote form")
  }
}

///|
test "parse labels" {
  let forms = parse_program("#1=(a) #1#")
  match forms {
    [@core.Datum::Label(label1, cell1), @core.Datum::Label(label2, cell2), ..] => {
      inspect(label1 == label2, content="true")
      cell1.val = @core.Datum::Symbol("z")
      match cell2.val {
        @core.Datum::Symbol("z") => ()
        _ => fail("expected shared label")
      }
    }
    _ => fail("expected labels")
  }
}

///|
test "parse block comment" {
  match parse_program("#| comment |# 1") {
    [@core.Datum::Int(1), ..] => ()
    _ => fail("expected int after comment")
  }
}

///|
test "parse errors" {
  let result = try? parse_program("#| unterminated")
  inspect(result is Err(_), content="true")
}
```
