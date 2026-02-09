///|
test "printer internals" {
  inspect(datum_is_negative(Bool(true)), content="false")
  match datum_abs(BigInt(@bigint.BigInt::from_int(1))) {
    BigInt(_) => ()
    _ => fail("expected bigint")
  }
  match
    datum_abs(BigRat(@bigint.BigInt::from_int(1), @bigint.BigInt::from_int(2))) {
    BigRat(_, _) => ()
    _ => fail("expected bigrat")
  }
  match datum_abs(Bool(true)) {
    Bool(true) => ()
    _ => fail("expected bool")
  }
}
