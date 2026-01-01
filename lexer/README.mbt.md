# lexer

Reader and token utilities used by the parser.

## Entry points

- Reader::new to create a reader
- Reader::read_token to scan the next token
- Reader::read_string to parse string literals

## Example

```mbt
///|
let reader = @lexer.Reader::new("(+ 1 2)")

///|
let tok = reader.read_token()
```

```mbt check
///|
test "reader helpers" {
  let peek = Reader::new("ab")
  guard peek.peek_next() is Some('b') else { fail("expected b") }
  ignore(peek.next())
  guard peek.peek() is Some('b') else { fail("expected b") }
  let labeled = Reader::new("x")
  let cell = Ref::new(@core.Datum::Nil)
  labeled.label_set(1, cell)
  guard labeled.label_get(1) is Some(_) else { fail("expected label") }
  guard labeled.label_get(2) is None else { fail("expected missing label") }
  let r = Reader::new("#(1 2)")
  inspect(r.is_vector_start(), content="true")
  let ellipsis = Reader::new("...")
  inspect(ellipsis.is_ellipsis_start(), content="true")
  let r_fold = Reader::new("ABC")
  r_fold.set_fold_case(true)
  inspect(r_fold.read_token(), content="abc")
  let bytevector = Reader::new("#vu8(1)")
  inspect(bytevector.is_bytevector_start(), content="true")
  let r2 = Reader::new("  ; comment\nfoo")
  r2.skip_ws_and_comments()
  guard r2.peek() is Some('f') else { fail("expected f") }
  let r4 = Reader::new("#| block |#foo")
  r4.skip_ws_and_comments()
  inspect(r4.read_token(), content="foo")
  let r5 = Reader::new("#| outer #| inner |# |#bar")
  r5.skip_ws_and_comments()
  inspect(r5.read_token(), content="bar")
  let r3 = Reader::new("\"hi\"")
  ignore(r3.next())
  inspect(r3.read_string(), content="hi")
  let escaped = Reader::new("\\x41;bc")
  inspect(escaped.read_token(), content="Abc")
}

///|
test "reader chaining" {
  let token =
    Reader::new("  ; comment\nfoo")
      ..skip_ws_and_comments()
      .read_token()
  inspect(token, content="foo")
}

///|
test "token stops at delimiter" {
  let r = Reader::new("foo)")
  inspect(r.read_token(), content="foo")
  guard r.peek() is Some(')') else { fail("expected delimiter") }
}

///|
test "bar identifiers" {
  let r = Reader::new("|a b|")
  inspect(r.read_token(), content="a b")
}

///|
test "bar identifiers with escape" {
  let r = Reader::new("|a\\|b|")
  inspect(r.read_token(), content="a|b")
}

///|
test "string escapes" {
  let r = Reader::new("\"a\\n\"")
  ignore(r.next())
  inspect(r.read_string(), content="a\n")
  let r2 = Reader::new("\"\\x41;\"")
  ignore(r2.next())
  inspect(r2.read_string(), content="A")
}

///|
test "string line continuation" {
  let r = Reader::new("\"a\\\n   b\"")
  ignore(r.next())
  inspect(r.read_string(), content="ab")
}
```
