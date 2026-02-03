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
    let result = try? apply_primitive_core(prim, [])
    inspect(result is Err(_), content="true")
  }
  let dispatcher = try? apply_primitive_core(Add, [])
  inspect(dispatcher is Err(_), content="true")
}

///|
test "apply primitive core type errors" {
  let bad_transformer = try? apply_primitive_core(
    MakeVariableTransformer,
    [Void],
  )
  inspect(bad_transformer is Err(_), content="true")
  let transformer_arity = try? apply_primitive_core(
    MakeVariableTransformer,
    [],
  )
  inspect(transformer_arity is Err(_), content="true")
  let bad_temporaries = try? apply_primitive_core(
    GenerateTemporaries,
    [Datum(Int(1))],
  )
  inspect(bad_temporaries is Err(_), content="true")
  let temporaries_arity = try? apply_primitive_core(
    GenerateTemporaries,
    [],
  )
  inspect(temporaries_arity is Err(_), content="true")
  let promise_arity = try? apply_primitive_core(PromiseP, [])
  inspect(promise_arity is Err(_), content="true")
}
