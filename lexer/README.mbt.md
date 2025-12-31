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
  inspect(digit_value('9'), content="Some(9)")
  let r = make_reader("#(1 2)")
  inspect(r.is_vector_start(), content="true")
  let ellipsis = make_reader("...")
  inspect(ellipsis.is_ellipsis_start(), content="true")
  let bytevector = make_reader("#vu8(1)")
  inspect(bytevector.is_bytevector_start(), content="true")
  let r2 = make_reader("  ; comment\nfoo")
  r2.skip_ws_and_comments()
  guard r2.peek() is Some('f') else { fail("expected f") }
  let r3 = make_reader("\"hi\"")
  ignore(r3.next())
  inspect(r3.read_string(), content="hi")
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
test "string escapes" {
  let r = make_reader("\"a\\n\"")
  ignore(r.next())
  inspect(r.read_string(), content="a\n")
  let r2 = make_reader("\"\\x41;\"")
  ignore(r2.next())
  inspect(r2.read_string(), content="A")
}
```
