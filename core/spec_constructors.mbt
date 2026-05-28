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
  { id, value: Ref(value), converter }
}

///|
pub fn Promise::new(id : Int, state : PromiseState) -> Promise {
  { id, state: Ref(state) }
}

///|
pub fn SyntaxRule::new(
  pattern : Datum,
  template : Datum,
  fender : Datum?,
) -> SyntaxRule {
  { pattern, template, fender }
}

///|
pub fn SyntaxRules::new(
  literals : Array[String],
  rules : Array[SyntaxRule],
  ellipsis : String,
  kind : SyntaxRulesKind,
  def_env : Env,
) -> SyntaxRules {
  { literals, rules, ellipsis, kind, def_env }
}

///|
pub fn SyntaxObject::new(
  datum : Datum,
  scopes : Array[Int],
  binding_id : Int?,
) -> SyntaxObject {
  { datum, scopes, binding_id }
}

///|
pub fn CaseClause::new(
  params : Array[String],
  rest : String?,
  body : Array[Datum],
) -> CaseClause {
  { params, rest, body }
}

///|
pub fn GuardHandler::new(
  id : Int,
  name : String,
  clauses : Array[Datum],
  env : Env,
  resume_value : Value,
  handlers : Array[Value],
) -> GuardHandler {
  { id, name, clauses, env, resume_value, handlers }
}
