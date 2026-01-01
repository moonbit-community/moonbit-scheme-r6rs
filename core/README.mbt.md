# core

Core runtime data types and Unicode utilities used across the interpreter.

## Highlights

- Datum, Value, and Primitive definitions
- Runtime type records and continuations
- Unicode category and normalization helpers

## Examples

```mbt
///|
let folded = UnicodeString::new("StraSSE").foldcase().into_string()

///|
let nfc = UnicodeString::new("e\u{301}").normalize_nfc().into_string()
```

```mbt check
///|
test "unicode helpers" {
  inspect(UnicodeChar::new('A').general_category(), content="Lu")
  inspect(UnicodeChar::new('A').is_uppercase(), content="true")
  inspect(UnicodeString::new("ABC").foldcase().into_string(), content="abc")
  inspect(UnicodeString::new("AbC").downcase().into_string(), content="abc")
  inspect(UnicodeString::new("abc").upcase().into_string(), content="ABC")
  inspect(
    UnicodeString::new("e\u{301}").normalize_nfc().into_string(),
    content="\u{00e9}",
  )
  inspect(
    UnicodeString::new("\u{00e9}").normalize_nfd().into_string(),
    content="e\u{301}",
  )
  inspect(
    UnicodeString::new("e\u{301}").normalize_nfc().foldcase().into_string(),
    content="\u{00e9}",
  )
  inspect(
    UnicodeString::new("e\u{301}").normalize_nfkd().into_string(),
    content="e\u{301}",
  )
  inspect(
    UnicodeString::new("\u{212b}").normalize_nfkc().into_string(),
    content="\u{00c5}",
  )
  inspect(UnicodeString::new("Hi").into_string(), content="Hi")
}

///|
test "unicode empty normalization" {
  inspect(UnicodeString::new("").normalize_nfkc().into_string(), content="")
}

///|
test "unicode char case" {
  inspect(UnicodeChar::new('a').is_lowercase(), content="true")
  inspect(UnicodeChar::new('0').is_alphabetic(), content="false")
  match UnicodeChar::new('a').upcase() {
    'A' => ()
    _ => fail("expected A")
  }
  match UnicodeChar::new('A').downcase() {
    'a' => ()
    _ => fail("expected a")
  }
}

///|
test "binding helpers" {
  let binding = Binding::new(1, Value::Void)
  inspect(binding.id(), content="1")
  match binding.value() {
    Value::Void => ()
    _ => fail("expected void")
  }
}

///|
test "record field binding helpers" {
  let binding = RecordFieldBinding::new("get", 0, None)
  inspect(binding.accessor(), content="get")
  inspect(binding.index(), content="0")
  inspect(binding.mutator(), content="None")
}

///|
test "datum constructors" {
  match Datum::Int(42) {
    Int(42) => ()
    _ => fail("expected int datum")
  }
  let pair = Datum::Pair(
    Ref::new(Datum::Symbol("a")),
    Ref::new(Datum::Nil),
  )
  match pair {
    Pair(car, cdr) =>
      match (car.val, cdr.val) {
        (Symbol("a"), Nil) => ()
        _ => fail("expected (a)")
      }
    _ => fail("expected pair")
  }
}
```
