# core

Core runtime data types and Unicode utilities used across the interpreter.

## Highlights

- Datum, Value, and Primitive definitions
- Runtime type records and continuations
- Unicode category and normalization helpers

## Examples

```mbt
///|
let folded = unicode_string("StraSSE").foldcase().into_string()

///|
let nfc = unicode_string("e\u{301}").normalize_nfc().into_string()
```

```mbt check
///|
test "unicode helpers" {
  inspect(unicode_char('A').general_category(), content="Lu")
  inspect(unicode_char('A').is_uppercase(), content="true")
  inspect(unicode_string("ABC").foldcase().into_string(), content="abc")
  inspect(unicode_string("AbC").downcase().into_string(), content="abc")
  inspect(unicode_string("abc").upcase().into_string(), content="ABC")
  inspect(
    unicode_string("e\u{301}").normalize_nfc().into_string(),
    content="\u{00e9}",
  )
  inspect(
    unicode_string("\u{00e9}").normalize_nfd().into_string(),
    content="e\u{301}",
  )
  inspect(
    unicode_string("e\u{301}").normalize_nfc().foldcase().into_string(),
    content="\u{00e9}",
  )
}
```
