///|
fn[T] expect_error(f : () -> T raise @core.EvalError) -> Unit raise {
  try f() catch {
    _ => ()
  } noraise {
    _ => fail("expected expression to raise")
  }
}

///|
test "apply primitive core internal errors" {
  let internal : Array[@core.Primitive] = [
    WithExceptionHandler,
    Raise,
    RaiseContinuable,
    Error,
    AssertionViolation,
    ImplementationRestrictionViolation,
    UndefinedViolation,
    SyntaxViolation,
    Apply,
    CallCC,
    Map,
    ForEach,
    VectorMap,
    VectorForEach,
    StringMap,
    StringForEach,
    CallWithValues,
    MakeParameter,
    DynamicWind,
    Eval,
    Environment,
    MakePromise,
    Force,
  ]
  for prim in internal {
    expect_error(() => apply_primitive_core(prim, []))
  }
  expect_error(() => apply_primitive_core(Add, []))
}

///|
test "apply primitive core type errors" {
  expect_error(() => apply_primitive_core(MakeVariableTransformer, [Void]))
  expect_error(() => apply_primitive_core(MakeVariableTransformer, []))
  expect_error(() => apply_primitive_core(GenerateTemporaries, [Datum(Int(1))]))
  expect_error(() => apply_primitive_core(GenerateTemporaries, []))
  expect_error(() => apply_primitive_core(PromiseP, []))
}
