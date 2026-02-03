///|
test "bytevector primitive branches" {
  match
    apply_bytevector_primitive(MakeByteVector, [
      Datum(Int(3)),
    ]) {
    Some(Datum(ByteVector(items))) => inspect(items.length(), content="3")
    _ => fail("expected bytevector")
  }
  let make_err = try? apply_bytevector_primitive(
    MakeByteVector,
    [],
  )
  inspect(make_err is Err(_), content="true")
  match
    apply_bytevector_primitive(ByteVectorP, [
      Datum(Int(1)),
    ]) {
    Some(Datum(Bool(false))) => ()
    _ => fail("expected false")
  }
  let len_err = try? apply_bytevector_primitive(
    ByteVectorLength,
    [],
  )
  inspect(len_err is Err(_), content="true")
  let eq_err = try? apply_bytevector_primitive(ByteVectorEq, [])
  inspect(eq_err is Err(_), content="true")
  let ref_err = try? apply_bytevector_primitive(
    ByteVectorU8Ref,
    [],
  )
  inspect(ref_err is Err(_), content="true")
  let set_err = try? apply_bytevector_primitive(
    ByteVectorU8Set,
    [],
  )
  inspect(set_err is Err(_), content="true")
}
