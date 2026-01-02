///|
test "parser internals" {
  match parse_hex_digits(""[:]) {
    None => ()
    Some(_) => fail("expected None")
  }
}
