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
  match parse_number_token("ff", radix=16) {
    Some(Int(255)) => ()
    _ => fail("expected 255")
  }
  match parse_number_token("not-a-number") {
    None => ()
    _ => fail("expected None")
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
  match parse_program("ABC", fold_case=true) {
    [Symbol("abc"), ..] => ()
    _ => fail("expected folded symbol")
  }
}

///|
test "parse quote" {
  match parse_program("'a") {
    [Pair(car, cdr), ..] =>
      match (car.val, cdr.val) {
        (Symbol("quote"), Pair(expr, tail)) =>
          match (expr.val, tail.val) {
            (Symbol("a"), Nil) => ()
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
    [Label(label1, cell1), Label(label2, cell2), ..] => {
      inspect(label1 == label2, content="true")
      cell1.val = @core.Datum::Symbol("z")
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

///|
test "parse errors" {
  let result = try? parse_program("#| unterminated")
  inspect(result is Err(_), content="true")
}

///|
test "parse number edge cases" {
  match parse_number_token("#x10") {
    Some(Int(16)) => ()
    _ => fail("expected 16")
  }
  match parse_number_token("#o10") {
    Some(Int(8)) => ()
    _ => fail("expected 8")
  }
  match parse_number_token("#d10") {
    Some(Int(10)) => ()
    _ => fail("expected 10")
  }
  match parse_number_token("#x") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("#b#x10") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("#e#e1") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("#i#e1") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("#q10") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("#b101", radix=10) {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("#xA/B") {
    Some(Rat(10, 11)) => ()
    _ => fail("expected hex rational")
  }
  match parse_number_token("1.5", radix=16) {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1/2", radix=16) {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("0/5") {
    Some(Int(0)) => ()
    _ => fail("expected 0/5 -> 0")
  }
  match parse_number_token("4/2") {
    Some(Int(2)) => ()
    _ => fail("expected 4/2 -> 2")
  }
  match parse_number_token("1/0") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1/-2") {
    Some(Rat(-1, 2)) => ()
    _ => fail("expected -1/2")
  }
  match parse_number_token("1e") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1.2.3") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("#i10") {
    Some(Float(f)) if f == 10.0 => ()
    _ => fail("expected inexact 10.0")
  }
  match parse_number_token("#i1/2") {
    Some(Float(f)) if f == 0.5 => ()
    _ => fail("expected inexact 0.5")
  }
  match parse_number_token("#ii") {
    Some(Complex(real, imag)) =>
      match (real.val, imag.val) {
        (Int(0), Float(f)) if f == 1.0 => ()
        _ => fail("expected 0+1.0i")
      }
    _ => fail("expected complex")
  }
  match parse_number_token("#i+i") {
    Some(Complex(real, imag)) =>
      match (real.val, imag.val) {
        (Int(0), Float(f)) if f == 1.0 => ()
        _ => fail("expected 0+1.0i")
      }
    _ => fail("expected complex")
  }
  match parse_number_token("#i-i") {
    Some(Complex(real, imag)) =>
      match (real.val, imag.val) {
        (Int(0), Float(f)) if f == -1.0 => ()
        _ => fail("expected 0-1.0i")
      }
    _ => fail("expected complex")
  }
  match parse_number_token("#o10", radix=10) {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1/2", radix=10) {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1", radix=1) {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1000000000000000000000000000000/2") {
    Some(BigInt(_)) => ()
    _ => fail("expected big int")
  }
  match parse_number_token("0/1000000000000000000000000000000") {
    Some(Int(0)) => ()
    _ => fail("expected 0")
  }
  match parse_number_token("1000000000000000000000000000000/0") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1@@2") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("@1") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1@") {
    None => ()
    _ => fail("expected None")
  }
  match parse_number_token("1@0") {
    Some(Float(f)) if f == 1.0 => ()
    _ => fail("expected 1.0")
  }
  match parse_number_token("i") {
    Some(Complex(real, imag)) =>
      match (real.val, imag.val) {
        (Int(0), Int(1)) => ()
        _ => fail("expected 0+1i")
      }
    _ => fail("expected complex")
  }
  match parse_number_token("+i") {
    Some(Complex(real, imag)) =>
      match (real.val, imag.val) {
        (Int(0), Int(1)) => ()
        _ => fail("expected 0+1i")
      }
    _ => fail("expected complex")
  }
  match parse_number_token("-i") {
    Some(Complex(real, imag)) =>
      match (real.val, imag.val) {
        (Int(0), Int(-1)) => ()
        _ => fail("expected 0-1i")
      }
    _ => fail("expected complex")
  }
}

///|
test "parse char edge cases" {
  match parse_program("#\\linefeed #\\backspace #\\tab") {
    [Char('\n'), Char('\u{8}'), Char('\t'), ..] => ()
    _ => fail("expected named chars")
  }
  match parse_program("#\\unknown #\\xZZ #\\") {
    [Symbol("#\\unknown"), Symbol("#\\xZZ"), Symbol("#\\"), ..] => ()
    _ => fail("expected invalid char tokens as symbols")
  }
  match parse_program("#1x") {
    [Symbol("#1x"), ..] => ()
    _ => fail("expected invalid label token as symbol")
  }
}

///|
test "parse structural errors" {
  let unexpected_eof = try? parse_program("(")
  inspect(unexpected_eof is Err(_), content="true")
  let unexpected_dot = try? parse_program("(.)")
  inspect(unexpected_dot is Err(_), content="true")
  let dotted_tail = try? parse_program("(1 . 2 3)")
  inspect(dotted_tail is Err(_), content="true")
  let improper_vector = try? parse_program("#(1 . 2)")
  inspect(improper_vector is Err(_), content="true")
  let bad_byte = try? parse_program("#vu8(256)")
  inspect(bad_byte is Err(_), content="true")
  let bad_byte_type = try? parse_program("#vu8(a)")
  inspect(bad_byte_type is Err(_), content="true")
  let unexpected_close = try? parse_program(")")
  inspect(unexpected_close is Err(_), content="true")
  let duplicate_label = try? parse_program("#1= #1= 1")
  inspect(duplicate_label is Err(_), content="true")
  let undefined_label = try? parse_program("#1#")
  inspect(undefined_label is Err(_), content="true")
  let comment_eof = try? parse_program("#;")
  inspect(comment_eof is Err(_), content="true")
}
```
