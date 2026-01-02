///|
test "apply primitive core internal errors" {
  let internal = [
    @core.Primitive::WithExceptionHandler,
    @core.Primitive::Raise,
    @core.Primitive::RaiseContinuable,
    @core.Primitive::Error,
    @core.Primitive::AssertionViolation,
    @core.Primitive::ImplementationRestrictionViolation,
    @core.Primitive::UndefinedViolation,
    @core.Primitive::SyntaxViolation,
    @core.Primitive::Apply,
    @core.Primitive::CallCC,
    @core.Primitive::Map,
    @core.Primitive::ForEach,
    @core.Primitive::VectorMap,
    @core.Primitive::VectorForEach,
    @core.Primitive::StringMap,
    @core.Primitive::StringForEach,
    @core.Primitive::CallWithValues,
    @core.Primitive::MakeParameter,
    @core.Primitive::DynamicWind,
    @core.Primitive::Eval,
    @core.Primitive::Environment,
    @core.Primitive::MakePromise,
    @core.Primitive::Force,
  ]
  for prim in internal {
    let result = try? apply_primitive_core(prim, [])
    inspect(result is Err(_), content="true")
  }
  let dispatcher = try? apply_primitive_core(@core.Primitive::Add, [])
  inspect(dispatcher is Err(_), content="true")
}

///|
test "apply primitive core type errors" {
  let bad_transformer =
    try? apply_primitive_core(
      @core.Primitive::MakeVariableTransformer,
      [@core.Value::Void],
    )
  inspect(bad_transformer is Err(_), content="true")
  let transformer_arity =
    try? apply_primitive_core(@core.Primitive::MakeVariableTransformer, [])
  inspect(transformer_arity is Err(_), content="true")
  let bad_temporaries =
    try? apply_primitive_core(
      @core.Primitive::GenerateTemporaries,
      [@core.Value::Datum(@core.Datum::Int(1))],
    )
  inspect(bad_temporaries is Err(_), content="true")
  let temporaries_arity =
    try? apply_primitive_core(@core.Primitive::GenerateTemporaries, [])
  inspect(temporaries_arity is Err(_), content="true")
  let promise_arity =
    try? apply_primitive_core(@core.Primitive::PromiseP, [])
  inspect(promise_arity is Err(_), content="true")
}
