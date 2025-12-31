# lexer

Reader and token utilities used by the parser.

## Entry points

- make_reader to create a reader
- Reader::read_token to scan the next token
- Reader::read_string to parse string literals

## Example

```mbt
///|
let reader = @lexer.make_reader("(+ 1 2)")

///|
let tok = reader.read_token()
```

```mbt check
///|
test "reader helpers" {
  let peek = make_reader("ab")
  guard peek.peek_next() is Some('b') else { fail("expected b") }
  peek.advance(1)
  guard peek.peek() is Some('b') else { fail("expected b") }
  let labeled = make_reader("x")
  let cell = Ref::new(@core.Datum::Nil)
  labeled.label_set(1, cell)
  guard labeled.label_get(1) is Some(_) else { fail("expected label") }
  guard labeled.label_get(2) is None else { fail("expected missing label") }
  let r = make_reader("#(1 2)")
  inspect(r.is_vector_start(), content="true")
  let ellipsis = make_reader("...")
  inspect(ellipsis.is_ellipsis_start(), content="true")
  let r_fold = make_reader("ABC")
  r_fold.set_fold_case(true)
  inspect(r_fold.read_token(), content="abc")
  let bytevector = make_reader("#vu8(1)")
  inspect(bytevector.is_bytevector_start(), content="true")
  let r2 = make_reader("  ; comment\nfoo")
  r2.skip_ws_and_comments()
  guard r2.peek() is Some('f') else { fail("expected f") }
  let r4 = make_reader("#| block |#foo")
  r4.skip_ws_and_comments()
  inspect(r4.read_token(), content="foo")
  let r5 = make_reader("#| outer #| inner |# |#bar")
  r5.skip_ws_and_comments()
  inspect(r5.read_token(), content="bar")
  let r3 = make_reader("\"hi\"")
  ignore(r3.next())
  inspect(r3.read_string(), content="hi")
  let escaped = make_reader("\\x41;bc")
  inspect(escaped.read_token(), content="Abc")
}

///|
test "reader chaining" {
  let token =
    make_reader("  ; comment\nfoo")
      ..skip_ws_and_comments()
      .read_token()
  inspect(token, content="foo")
}

///|
test "bar identifiers" {
  let r = make_reader("|a b|")
  inspect(r.read_token(), content="a b")
}

///|
test "string escapes" {
  let r = make_reader("\"a\\n\"")
  ignore(r.next())
  inspect(r.read_string(), content="a\n")
  let r2 = make_reader("\"\\x41;\"")
  ignore(r2.next())
  inspect(r2.read_string(), content="A")
}

///|
test "string line continuation" {
  let r = make_reader("\"a\\\n   b\"")
  ignore(r.next())
  inspect(r.read_string(), content="ab")
}
```
