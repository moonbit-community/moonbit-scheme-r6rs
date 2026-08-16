///|
test "bytevector primitive branches" {
  match apply_bytevector_primitive(MakeByteVector, [Datum(Int(3))]) {
    Some(Datum(ByteVector(items))) => inspect(items.length(), content="3")
    _ => fail("expected bytevector")
  }
  expect_error(() => apply_bytevector_primitive(MakeByteVector, []))
  match apply_bytevector_primitive(ByteVectorP, [Datum(Int(1))]) {
    Some(Datum(Bool(false))) => ()
    _ => fail("expected false")
  }
  expect_error(() => apply_bytevector_primitive(ByteVectorLength, []))
  expect_error(() => apply_bytevector_primitive(ByteVectorEq, []))
  expect_error(() => apply_bytevector_primitive(ByteVectorU8Ref, []))
  expect_error(() => apply_bytevector_primitive(ByteVectorU8Set, []))
}
