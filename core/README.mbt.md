# core

Core runtime data types and Unicode utilities used across the interpreter.

## Highlights

- Datum, Value, and Primitive definitions
- Runtime type records and continuations
- Unicode category and normalization helpers

## Examples

```mbt nocheck
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
test "unicode titlecase" {
  match UnicodeChar::new('\u{01C5}').general_category() {
    "Lt" => ()
    _ => fail("expected titlecase category")
  }
  match UnicodeChar::new('\u{01C5}').upcase() {
    '\u{01C4}' | '\u{01C5}' => ()
    _ => fail("expected titlecase upcase")
  }
  match UnicodeChar::new('\u{01C5}').downcase() {
    '\u{01C6}' | '\u{01C5}' => ()
    _ => fail("expected titlecase downcase")
  }
  match UnicodeChar::new('1').downcase() {
    '1' => ()
    _ => fail("expected unchanged")
  }
}

///|
test "unicode hangul normalization" {
  inspect(
    UnicodeString::new("\u{AC01}").normalize_nfd().into_string(),
    content="\u{1100}\u{1161}\u{11A8}",
  )
  inspect(
    UnicodeString::new("\u{1100}\u{1161}\u{11A8}").normalize_nfc().into_string(),
    content="\u{AC01}",
  )
  inspect(
    UnicodeString::new("\u{0301}A").normalize_nfd().into_string(),
    content="\u{0301}A",
  )
  inspect(
    UnicodeString::new("B\u{0301}").normalize_nfc().into_string(),
    content="B\u{0301}",
  )
  inspect(
    UnicodeString::new("\u{212B}").normalize_nfkd().into_string(),
    content="A\u{030A}",
  )
  inspect(
    UnicodeString::new("\u{2460}").normalize_nfkd().into_string(),
    content="1",
  )
}

///|
test "binding helpers" {
  let binding = Binding::new(1, Void)
  inspect(binding.id(), content="1")
  match binding.value() {
    Void => ()
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
  let pair = Datum::Pair(Ref::new(Symbol("a")), Ref::new(Nil))
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
