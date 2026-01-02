///|
pub fn Closure::new(
  id : Int,
  params : Array[String],
  rest : String?,
  body : Array[Datum],
  env : Env,
) -> Closure {
  { id, params, rest, body, env }
}

///|
pub fn CaseClosure::new(
  id : Int,
  clauses : Array[CaseClause],
  env : Env,
) -> CaseClosure {
  { id, clauses, env }
}

///|
pub fn EvalEnv::new(id : Int, env : Env) -> EvalEnv {
  { id, env }
}

///|
pub fn Parameter::new(id : Int, value : Value, converter : Value?) -> Parameter {
  { id, value: Ref::new(value), converter }
}

///|
pub fn Promise::new(id : Int, state : PromiseState) -> Promise {
  { id, state: Ref::new(state) }
}

///|
pub fn SyntaxRule::new(
  pattern : Datum,
  template : Datum,
  fender : Datum?,
) -> SyntaxRule {
  { pattern, template, fender }
}
