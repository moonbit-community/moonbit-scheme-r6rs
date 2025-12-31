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
    Some(Int(-10)) => ()
    _ => fail("expected -10")
  }
  match parse_number_token("3/4") {
    Some(Rat(3, 4)) => ()
    _ => fail("expected 3/4")
  }
  match parse_number_token("1+2i") {
    Some(Complex(real, imag)) =>
      match (real.val, imag.val) {
        (Int(1), Int(2)) => ()
        _ => fail("expected 1+2i")
      }
    _ => fail("expected complex")
  }
  match parse_number_token("1e2") {
    Some(Float(f)) => inspect(f == 100.0, content="true")
    _ => fail("expected 100.0")
  }
  match parse_number_token("1.5") {
    Some(Float(f)) => inspect(f == 1.5, content="true")
    _ => fail("expected 1.5")
  }
  match parse_number_token_with_radix("ff", 16) {
    Some(Int(255)) => ()
    _ => fail("expected 255")
  }
  match parse_program("#\\x41 #\\space") {
    [Char('A'), Char(' '), ..] => ()
    _ => fail("expected char literals")
  }
  match parse_program("#t #f") {
    [Bool(true), Bool(false), ..] => ()
    _ => fail("expected booleans")
  }
  match parse_program("\"hi\"") {
    [String(text), ..] => inspect(text.val, content="hi")
    _ => fail("expected string")
  }
  match parse_program("#vu8(1 2)") {
    [ByteVector(items), ..] =>
      inspect(items.length(), content="2")
    _ => fail("expected bytevector")
  }
  match parse_program("#(1 2)") {
    [Vector(items), ..] => inspect(items.length(), content="2")
    _ => fail("expected vector")
  }
  match parse_program("(a . b)") {
    [Pair(car, cdr), ..] =>
      match (car.val, cdr.val) {
        (Symbol("a"), Symbol("b")) => ()
        _ => fail("expected dotted pair")
      }
    _ => fail("expected dotted pair")
  }
  match parse_program("(a ;comment\n b)") {
    [Pair(car, cdr), ..] =>
      match (car.val, cdr.val) {
        (Symbol("a"), Pair(next, tail)) =>
          match (next.val, tail.val) {
            (Symbol("b"), Nil) => ()
            _ => fail("expected (a b)")
          }
        _ => fail("expected (a b)")
      }
    _ => fail("expected list")
  }
  match parse_program_with_fold_case("ABC", true) {
    [Symbol("abc"), ..] => ()
    _ => fail("expected folded symbol")
  }
}

///|
test "parse labels" {
  let forms = parse_program("#1=(a) #1#")
  match forms {
    [Label(label1, cell1), Label(label2, cell2), ..] => {
      inspect(label1 == label2, content="true")
      cell1.val = Symbol("z")
      match cell2.val {
        Symbol("z") => ()
        _ => fail("expected shared label")
      }
    }
    _ => fail("expected labels")
  }
}

///|
test "parse block comment" {
  match parse_program("#| comment |# 1") {
    [Int(1), ..] => ()
    _ => fail("expected int after comment")
  }
}
```
