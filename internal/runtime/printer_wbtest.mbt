///|
test "printer internals" {
  inspect(datum_is_negative(@core.Datum::Bool(true)), content="false")
  match datum_abs(@core.Datum::BigInt(@bigint.BigInt::from_int(1))) {
    BigInt(_) => ()
    _ => fail("expected bigint")
  }
  match datum_abs(
    @core.Datum::BigRat(
      @bigint.BigInt::from_int(1),
      @bigint.BigInt::from_int(2),
    ),
  ) {
    BigRat(_, _) => ()
    _ => fail("expected bigrat")
  }
  match datum_abs(@core.Datum::Bool(true)) {
    Bool(true) => ()
    _ => fail("expected bool")
  }
}
