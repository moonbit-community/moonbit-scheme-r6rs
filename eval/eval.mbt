///|
fn register_builtin_libraries(env : Env) -> Unit raise EvalError {
  let exports = env_export_map(env)
  for name in base_syntax_keywords() {
    // invariant : base_syntax_keywords().length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : base_syntax_keywords().length() >= 0
    exports[name] = Binding::{
      id: next_binding_id(),
      value: SyntaxKeyword(name),
    }
  }
  register_library_entry("rnrs.base", Some([6]), exports)
  let eval_exports : Map[String, Binding] = {}
  eval_exports["eval"] = Binding::{
    id: next_binding_id(),
    value: Primitive(Eval),
  }
  eval_exports["environment"] = Binding::{
    id: next_binding_id(),
    value: Primitive(Environment),
  }
  register_library_entry("rnrs.eval", Some([6]), eval_exports)
  let condition_exports : Map[String, Binding] = {}
  let condition_names : Array[String] = [
    "condition", "condition?", "simple-conditions", "condition-predicate", "condition-accessor",
    "&condition", "&message", "make-message-condition", "message-condition?", "condition-message",
    "&warning", "make-warning", "warning?", "&serious", "make-serious-condition",
    "serious-condition?", "&error", "make-error", "error?", "&violation", "make-violation",
    "violation?", "&assertion", "make-assertion-violation", "assertion-violation?",
    "&irritants", "make-irritants-condition", "irritants-condition?", "condition-irritants",
    "&who", "make-who-condition", "who-condition?", "condition-who", "&non-continuable",
    "make-non-continuable-violation", "non-continuable-violation?", "&implementation-restriction",
    "make-implementation-restriction-violation", "implementation-restriction-violation?",
    "&lexical", "make-lexical-violation", "lexical-violation?", "&syntax", "make-syntax-violation",
    "syntax-violation?", "syntax-violation-form", "syntax-violation-subform", "&undefined",
    "make-undefined-violation", "undefined-violation?",
  ]
  for name in condition_names {
    // invariant : condition_names.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : condition_names.length() >= 0
    condition_exports[name] = env_lookup_binding(env, name)
  }
  condition_exports["define-condition-type"] = Binding::{
    id: next_binding_id(),
    value: SyntaxKeyword("define-condition-type"),
  }
  register_library_entry("rnrs.conditions", Some([6]), condition_exports)
  let flonum_exports : Map[String, Binding] = {}
  let flonum_names : Array[String] = [
    "flonum?", "real->flonum", "fixnum->flonum", "fl=?", "fl<?", "fl>?", "fl<=?",
    "fl>=?", "flinteger?", "flzero?", "flpositive?", "flnegative?", "flodd?", "fleven?",
    "flfinite?", "flinfinite?", "flnan?", "flmax", "flmin", "fl+", "fl*", "fl-",
    "fl/", "flabs", "fldiv-and-mod", "fldiv", "flmod", "fldiv0-and-mod0", "fldiv0",
    "flmod0", "flnumerator", "fldenominator", "flfloor", "flceiling", "fltruncate",
    "flround", "flexp", "fllog", "flsin", "flcos", "fltan", "flasin", "flacos", "flatan",
    "flsqrt", "flexpt", "&no-infinities", "make-no-infinities-violation", "no-infinities-violation?",
    "&no-nans", "make-no-nans-violation", "no-nans-violation?",
  ]
  for name in flonum_names {
    // invariant : flonum_names.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : flonum_names.length() >= 0
    flonum_exports[name] = env_lookup_binding(env, name)
  }
  register_library_entry("rnrs.arithmetic.flonums", Some([6]), flonum_exports)
}

///|
priv struct Counter {
  cell : Ref[Int]
}

///|
fn Counter::new() -> Counter {
  { cell: Ref::new(0) }
}

///|
fn Counter::next(self : Counter) -> Int {
  let id = self.cell.val
  self.cell.val = id + 1
  id
}

///|
let closure_counter : Counter = Counter::new()

///|
fn next_closure_id() -> Int {
  closure_counter.next()
}

///|
fn make_closure(
  params : Array[String],
  rest : String?,
  body : Array[Datum],
  env : Env,
) -> Closure {
  { id: next_closure_id(), params, rest, body, env }
}

///|
let parameter_counter : Counter = Counter::new()

///|
fn next_parameter_id() -> Int {
  parameter_counter.next()
}

///|
fn make_parameter(value : Value) -> Parameter {
  { id: next_parameter_id(), value: Ref::new(value), converter: None }
}

///|
fn value_as_parameter(value : Value) -> Parameter raise EvalError {
  match value {
    Parameter(param) => param
    _ => raise @core.EvalError("type error: parameter expected")
  }
}

///|
fn parameter_get(param : Parameter) -> Value {
  param.value.val
}

///|
fn parameter_set(param : Parameter, value : Value) -> Unit {
  param.value.val = value
}

///|
fn parameter_with_converter(value : Value, converter : Value) -> Parameter {
  {
    id: next_parameter_id(),
    value: Ref::new(value),
    converter: Some(converter),
  }
}

///|
let promise_counter : Counter = Counter::new()

///|
fn next_promise_id() -> Int {
  promise_counter.next()
}

///|
fn make_promise(thunk : Value) -> Promise {
  { id: next_promise_id(), state: Ref::new(Thunk(thunk)) }
}

///|
let eval_env_counter : Counter = Counter::new()

///|
fn next_eval_env_id() -> Int {
  eval_env_counter.next()
}

///|
fn make_eval_env(env : Env) -> EvalEnv {
  { id: next_eval_env_id(), env }
}

///|
fn value_as_eval_env(value : Value) -> EvalEnv raise EvalError {
  match value {
    EvalEnv(env) => env
    _ => raise @core.EvalError("type error: environment expected")
  }
}

///|
fn make_case_closure(clauses : Array[CaseClause], env : Env) -> CaseClosure {
  { id: next_closure_id(), clauses, env }
}

///|
fn make_guard_handler(
  name : String,
  clauses : Array[Datum],
  env : Env,
  resume_value : Value,
  handlers : Array[Value],
) -> GuardHandler {
  GuardHandler::{
    id: next_guard_handler_id(),
    name,
    clauses,
    env,
    resume_value,
    handlers,
  }
}

///|
fn capture_continuation(kont : Kont, handlers : Array[Value]) -> Continuation {
  let winds_copy = wind_stack.val.copy()
  Continuation::{
    id: next_continuation_id(),
    kont,
    handlers,
    winds: winds_copy,
  }
}

///|
priv enum MachineState {
  Eval(Datum, Env, Kont, Array[Value])
  Apply(Value, Kont, Array[Value])
}

///|
let winder_counter : Counter = Counter::new()

///|
fn next_winder_id() -> Int {
  winder_counter.next()
}

///|
fn make_proc_winder(before : Value, after : Value) -> Winder {
  Winder::{ id: next_winder_id(), kind: Proc(before, after) }
}

///|
fn make_param_winder(bindings : Array[ParamBinding]) -> Winder {
  Winder::{ id: next_winder_id(), kind: Params(bindings) }
}

///|
let wind_stack : Ref[Array[Winder]] = Ref::new([])

///|
let continuation_counter : Counter = Counter::new()

///|
let guard_handler_counter : Counter = Counter::new()

///|
fn next_continuation_id() -> Int {
  continuation_counter.next()
}

///|
fn next_guard_handler_id() -> Int {
  guard_handler_counter.next()
}

///|
let do_counter : Counter = Counter::new()

///|
fn next_do_name() -> String {
  let id = do_counter.next()
  "__do_loop__\{id}"
}

///|
/// Evaluate a Scheme program and return the final value.
///
/// # Example
/// ```mbt check
/// test "eval program" {
///   let value = eval_program("(+ 1 2)")
///   inspect(@runtime.value_to_string(value), content="3")
/// }
/// ```
pub fn eval_program(src : String) -> Value raise {
  let results = eval_program_all(src)
  if results.is_empty() {
    Void
  } else {
    results[results.length() - 1]
  }
}

///|
/// Evaluate a Scheme program and return all top-level results.
///
/// # Example
/// ```mbt check
/// test "eval program all" {
///   let values = eval_program_all("(define x 1) (+ x 2)")
///   inspect(values.length(), content="2")
///   inspect(@runtime.value_to_string(values[1]), content="3")
/// }
/// ```
pub fn eval_program_all(src : String) -> Array[Value] raise {
  let forms = parse_program(src)
  reset_library_registry()
  reset_library_version_registry()
  reset_record_type_registry()
  set_current_output_port(new_output_string_port())
  wind_stack.val = []
  let env = init_env()
  load_stdlib(env)
  register_builtin_libraries(env)
  let results : Array[Value] = []
  for form in forms {
    // invariant : forms.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : forms.length() >= 0
    let value = eval_expr(form, env)
    results.push(value)
  }
  results
}

///|
fn env_with_captures(env : Env, captures : Map[String, Value]) -> Env {
  if captures.is_empty() {
    return env
  }
  let next_env = env_extend(env)
  for name in captures.keys() {
    // invariant : captures.keys().length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : captures.keys().length() >= 0
    match captures.get(name) {
      Some(value) => env_define(next_env, name, value)
      None => ()
    }
  }
  next_env
}

///|
fn values_with_push(values : Array[Value], extra : Value) -> Array[Value] {
  let next = values.copy()
  next.push(extra)
  next
}

///|
fn eval_expr_impl(
  expr : Datum,
  env : Env,
  unwrap_syntax : Bool,
  strip_literals : Bool,
) -> Value raise EvalError {
  let handlers : Array[Value] = []
  let mut state = Eval(expr, env, Halt, handlers)
  while true {
    match state {
      Eval(current_expr, current_env, kont, handlers) =>
        match current_expr {
          Label(_, cell) =>
            state = Eval(cell.val, current_env, kont, handlers)
          Value(SyntaxObject(obj)) =>
            if unwrap_syntax {
              state = Eval(obj.datum, current_env, kont, handlers)
            } else {
              let literal = if strip_literals {
                strip_syntax_datum(current_expr)
              } else {
                current_expr
              }
              state = Apply(
                value_from_datum(literal),
                kont,
                handlers,
              )
            }
          Nil
          | Bool(_)
          | Int(_)
          | BigInt(_)
          | Rat(_, _)
          | BigRat(_, _)
          | Float(_)
          | Complex(_, _)
          | Char(_)
          | String(_)
          | Vector(_)
          | ByteVector(_)
          | Record(_)
          | Condition(_)
          | Value(_) => {
            let literal = if strip_literals {
              strip_syntax_datum(current_expr)
            } else {
              current_expr
            }
            state = Apply(
              value_from_datum(literal),
              kont,
              handlers,
            )
          }
          Symbol(name) =>
            match env_get_macro(current_env, name) {
              Some(transformer) => {
                let expansion = expand_macro(
                  transformer,
                  Symbol(name),
                  current_env,
                )
                let next_env = env_with_captures(
                  current_env,
                  expansion.captures,
                )
                state = Eval(
                  expansion.datum,
                  next_env,
                  kont,
                  handlers,
                )
              }
              None => {
                let value = env_lookup(current_env, name)
                state = Apply(value, kont, handlers)
              }
            }
          Pair(_, _) =>
            state = eval_pair(current_expr, current_env, kont, handlers)
        }
      Apply(value, kont, handlers) =>
        match kont {
          Halt => return value
          If(then_expr, else_expr, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = Eval(else_expr, env, next, handlers)
            } else {
              state = Eval(then_expr, env, next, handlers)
            }
          }
          Begin(rest, env, next) =>
            if rest.is_empty() {
              state = Apply(value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = Eval(
                rest[0],
                env,
                Begin(tail, env, next),
                handlers,
              )
            }
          Set(name, env, next) => {
            let single = expect_single(value)
            env_set(env, name, single)
            state = Apply(Void, next, handlers)
          }
          Define(name, env, next) => {
            let single = expect_single(value)
            env_define(env, name, single)
            state = Apply(Void, next, handlers)
          }
          AppFun(args, env, next) => {
            let proc_value = expect_single(value)
            if args.is_empty() {
              state = apply_proc(proc_value, [], next, handlers)
            } else {
              let rest = args.sub(start=1).to_array()
              state = Eval(
                args[0],
                env,
                AppArgs(proc_value, [], rest, env, next),
                handlers,
              )
            }
          }
          AppArgs(proc, evaluated, remaining, env, next) => {
            let arg_value = expect_single(value)
            let next_values = values_with_push(evaluated, arg_value)
            if remaining.is_empty() {
              state = apply_proc(proc, next_values, next, handlers)
            } else {
              let rest = remaining.sub(start=1).to_array()
              state = Eval(
                remaining[0],
                env,
                AppArgs(proc, next_values, rest, env, next),
                handlers,
              )
            }
          }
          And(rest, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = Apply(test_value, next, handlers)
            } else if rest.is_empty() {
              state = Apply(test_value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = Eval(
                rest[0],
                env,
                And(tail, env, next),
                handlers,
              )
            }
          }
          Or(rest, env, next) => {
            let test_value = expect_single(value)
            if !is_false(test_value) {
              state = Apply(test_value, next, handlers)
            } else if rest.is_empty() {
              state = Apply(test_value, next, handlers)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = Eval(
                rest[0],
                env,
                Or(tail, env, next),
                handlers,
              )
            }
          }
          Cond(parts, rest, env, next) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = cond_start(rest, env, next, handlers)
            } else if parts.length() == 1 {
              state = Apply(test_value, next, handlers)
            } else if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
              if parts.length() != 3 {
                raise @core.EvalError("invalid cond")
              }
              state = Eval(
                parts[2],
                env,
                CondArrow(test_value, next),
                handlers,
              )
            } else {
              let body = parts.sub(start=1).to_array()
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          CondArrow(test_value, next) => {
            let proc_value = expect_single(value)
            state = apply_proc(proc_value, [test_value], next, handlers)
          }
          Let(bindings, index, values, env, body, next) => {
            let single = expect_single(value)
            let next_values = values_with_push(values, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = Eval(
                expr,
                env,
                Let(bindings, next_index, next_values, env, body, next),
                handlers,
              )
            } else {
              let new_env = env_extend(env)
              for i = 0; i < bindings.length(); {
                // invariant : i >= 0 && i <= bindings.length()
                // decreases : bindings.length() - i
                // assert : i <= bindings.length()
                let (name, _) = bindings[i]
                env_define(new_env, name, next_values[i])
                continue i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          LetStar(bindings, index, env, body, next) => {
            let single = expect_single(value)
            let (name, _) = bindings[index]
            let new_env = env_extend(env)
            env_define(new_env, name, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = Eval(
                expr,
                new_env,
                LetStar(bindings, next_index, new_env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          LetRec(bindings, index, env, body, next) => {
            let single = expect_single(value)
            let (name, _) = bindings[index]
            env_set(env, name, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = Eval(
                expr,
                env,
                LetRec(bindings, next_index, env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          LetRecInit(bindings, index, collected, env, body, next) => {
            let single = expect_single(value)
            let next_values = values_with_push(collected, single)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = Eval(
                expr,
                env,
                LetRecInit(
                  bindings, next_index, next_values, env, body, next,
                ),
                handlers,
              )
            } else {
              for i = 0; i < bindings.length(); {
                // invariant : i >= 0 && i <= bindings.length()
                // decreases : bindings.length() - i
                // assert : i <= bindings.length()
                let (name, _) = bindings[i]
                env_set(env, name, next_values[i])
                continue i + 1
              }
              state = eval_sequence_state(body, env, next, handlers)
            }
          }
          LetValues(bindings, index, collected, env, body, next) => {
            let values = value_to_values(value)
            let (formals, _) = bindings[index]
            let bound = formals_bindings(formals, values)
            let next_collected : Array[Array[(String, Value)]] = []
            for item in collected {
              // invariant : collected.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : collected.length() >= 0
              next_collected.push(item)
            }
            next_collected.push(bound)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = Eval(
                expr,
                env,
                LetValues(
                  bindings, next_index, next_collected, env, body, next,
                ),
                handlers,
              )
            } else {
              let new_env = env_extend(env)
              for i = 0; i < bindings.length(); {
                // invariant : i >= 0 && i <= bindings.length()
                // decreases : bindings.length() - i
                // assert : i <= bindings.length()
                let binding_values = next_collected[i]
                for binding in binding_values {
                  // invariant : binding_values.length() >= 0
                  // TODO(decreases) : loop index not exposed; possible bug
                  // assert : binding_values.length() >= 0
                  let (name, bound_value) = binding
                  env_define(new_env, name, bound_value)
                }
                continue i + 1
              }
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          LetStarValues(bindings, index, env, body, next) => {
            let values = value_to_values(value)
            let (formals, _) = bindings[index]
            let binding_values = formals_bindings(formals, values)
            let new_env = env_extend(env)
            for binding in binding_values {
              // invariant : binding_values.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : binding_values.length() >= 0
              let (name, bound_value) = binding
              env_define(new_env, name, bound_value)
            }
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = Eval(
                expr,
                new_env,
                LetStarValues(bindings, next_index, new_env, body, next),
                handlers,
              )
            } else {
              state = eval_sequence_state(body, new_env, next, handlers)
            }
          }
          DefineValues(formals, env, next) => {
            let values = value_to_values(value)
            let binding_values = formals_bindings(formals, values)
            for binding in binding_values {
              // invariant : binding_values.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : binding_values.length() >= 0
              let (name, bound_value) = binding
              env_define(env, name, bound_value)
            }
            state = Apply(Void, next, handlers)
          }
          SyntaxCase(rest, env, next) => {
            if rest.length() < 2 {
              raise @core.EvalError("invalid syntax-case")
            }
            let input = expect_single(value)
            let literals_expr = rest[0]
            let clauses = rest.sub(start=1).to_array()
            let rules = parse_syntax_case_rules(literals_expr, clauses, env)
            let call = value_to_syntax_datum(input)
            let expansion = expand_macro(
              MacroTransformer::Rules(rules),
              call,
              env,
            )
            let result = match expansion.datum {
              Value(SyntaxObject(obj)) => Value::SyntaxObject(obj)
              _ => Value::Datum(expansion.datum)
            }
            state = Apply(result, next, handlers)
          }
          ParameterizeParam(
            bindings,
            index,
            params,
            values,
            env,
            body,
            next
          ) => {
            let param_value = expect_single(value)
            let param = value_as_parameter(param_value)
            let next_params : Array[Parameter] = []
            for item in params {
              // invariant : params.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : params.length() >= 0
              next_params.push(item)
            }
            next_params.push(param)
            let (_, expr) = bindings[index]
            state = Eval(
              expr,
              env,
              ParameterizeValue(
                bindings, index, next_params, values, env, body, next,
              ),
              handlers,
            )
          }
          ParameterizeValue(
            bindings,
            index,
            params,
            values,
            env,
            body,
            next
          ) => {
            let new_value = expect_single(value)
            let param = params[index]
            match param.converter {
              Some(converter) =>
                state = apply_proc(
                  converter,
                  [new_value],
                  ParameterizeConvert(
                    bindings, index, params, values, env, body, next,
                  ),
                  handlers,
                )
              None => {
                let next_values = values_with_push(values, new_value)
                let next_index = index + 1
                if next_index < bindings.length() {
                  let (param_expr, _) = bindings[next_index]
                  state = Eval(
                    param_expr,
                    env,
                    ParameterizeParam(
                      bindings, next_index, params, next_values, env, body, next,
                    ),
                    handlers,
                  )
                } else {
                  let bindings_info : Array[ParamBinding] = []
                  for i = 0; i < params.length(); {
                    // invariant : i >= 0 && i <= params.length()
                    // decreases : params.length() - i
                    // assert : i <= params.length()
                    let current = params[i]
                    let old_value = parameter_get(current)
                    let new_val = next_values[i]
                    bindings_info.push(ParamBinding::{
                      param: current,
                      old_value,
                      new_value: new_val,
                    })
                    parameter_set(current, new_val)
                    continue i + 1
                  }
                  let winder = make_param_winder(bindings_info)
                  wind_push(winder)
                  state = eval_sequence_state(
                    body,
                    env,
                    WindExit(winder, next),
                    handlers,
                  )
                }
              }
            }
          }
          ParameterizeConvert(
            bindings,
            index,
            params,
            values,
            env,
            body,
            next
          ) => {
            let converted = expect_single(value)
            let next_values = values_with_push(values, converted)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (param_expr, _) = bindings[next_index]
              state = Eval(
                param_expr,
                env,
                ParameterizeParam(
                  bindings, next_index, params, next_values, env, body, next,
                ),
                handlers,
              )
            } else {
              let bindings_info : Array[ParamBinding] = []
              for i = 0; i < params.length(); {
                // invariant : i >= 0 && i <= params.length()
                // decreases : params.length() - i
                // assert : i <= params.length()
                let current = params[i]
                let old_value = parameter_get(current)
                let new_val = next_values[i]
                bindings_info.push(ParamBinding::{
                  param: current,
                  old_value,
                  new_value: new_val,
                })
                parameter_set(current, new_val)
                continue i + 1
              }
              let winder = make_param_winder(bindings_info)
              wind_push(winder)
              state = eval_sequence_state(
                body,
                env,
                WindExit(winder, next),
                handlers,
              )
            }
          }
          MakeParameter(converter, next) => {
            let converted = expect_single(value)
            let param = parameter_with_converter(converted, converter)
            state = Apply(Parameter(param), next, handlers)
          }
          SetParameter(param, next) => {
            let converted = expect_single(value)
            parameter_set(param, converted)
            state = Apply(Void, next, handlers)
          }
          GuardResult(info) => {
            let guard_value = expect_single(value)
            state = apply_proc(
              info.resume_value,
              [guard_value],
              kont,
              info.handlers,
            )
          }
          GuardCond(parts, rest, env, info) => {
            let test_value = expect_single(value)
            if is_false(test_value) {
              state = guard_start(rest, env, info, handlers)
            } else if parts.length() == 1 {
              state = apply_proc(
                info.resume_value,
                [test_value],
                kont,
                info.handlers,
              )
            } else if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
              if parts.length() != 3 {
                raise @core.EvalError("invalid guard")
              }
              state = Eval(
                parts[2],
                env,
                GuardCondArrow(test_value, info),
                handlers,
              )
            } else {
              let body = parts.sub(start=1).to_array()
              state = eval_sequence_state(
                body,
                env,
                GuardResult(info),
                handlers,
              )
            }
          }
          GuardCondArrow(test_value, info) => {
            let proc_value = expect_single(value)
            state = apply_proc(
              proc_value,
              [test_value],
              GuardResult(info),
              handlers,
            )
          }
          Case(clauses, env, next) => {
            let key_value = expect_single(value)
            state = case_dispatch(key_value, clauses, env, next, handlers)
          }
          CaseArrow(test_value, next) => {
            let proc_value = expect_single(value)
            state = apply_proc(proc_value, [test_value], next, handlers)
          }
          CallWithValues(consumer, next) => {
            let values = value_to_values(value)
            state = apply_proc(consumer, values, next, handlers)
          }
          RecordConstructorDone(next) => {
            let ctor_value = expect_single(value)
            if !is_procedure_value(ctor_value) {
              raise @core.EvalError("type error: procedure expected")
            }
            state = Apply(ctor_value, next, handlers)
          }
          RecordProtocolNApply(record_type, parent_args, next) => {
            let ctor_value = expect_single(value)
            state = apply_proc(
              ctor_value,
              parent_args,
              RecordProtocolNResult(record_type, next),
              handlers,
            )
          }
          RecordProtocolNResult(record_type, next) => {
            let record_value = expect_single(value)
            let record = value_as_record(record_value)
            let parent_type = match record_type.parent {
              Some(parent) => parent
              None =>
                raise @core.EvalError("invalid record constructor descriptor")
            }
            if !record_type_is_a(record.record_type, parent_type) {
              raise @core.EvalError(
                "type error: record type mismatch: expected \{parent_type.name}",
              )
            }
            let parent_values : Array[Value] =
              record.fields.map((field) => field.val)
            let proc = Value::RecordProc(
              make_record_proc(
                RecordProcKind::ProtocolP(record_type, parent_values),
              ),
            )
            state = Apply(proc, next, handlers)
          }
          DefineRecordType(
            ctor_name,
            pred_name,
            record_type,
            bindings,
            env,
            next
          ) => {
            let ctor_value = expect_single(value)
            if !is_procedure_value(ctor_value) {
              raise @core.EvalError("type error: procedure expected")
            }
            env_define(env, ctor_name, ctor_value)
            env_define(
              env,
              pred_name,
              Value::RecordProc(
                make_record_proc(RecordProcKind::Predicate(record_type)),
              ),
            )
            for binding in bindings {
              // invariant : bindings.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : bindings.length() >= 0
              env_define(
                env,
                binding.accessor,
                Value::RecordProc(
                  make_record_proc(
                    RecordProcKind::Accessor(record_type, binding.index),
                  ),
                ),
              )
              match binding.mutator {
                Some(mut_name) =>
                  env_define(
                    env,
                    mut_name,
                    Value::RecordProc(
                      make_record_proc(
                        RecordProcKind::Mutator(record_type, binding.index),
                      ),
                    ),
                  )
                None => ()
              }
            }
            state = Apply(Void, next, handlers)
          }
          DefineConditionType(
            ctor_name,
            pred_name,
            record_type,
            bindings,
            env,
            next
          ) => {
            let ctor_value = expect_single(value)
            if !is_procedure_value(ctor_value) {
              raise @core.EvalError("type error: procedure expected")
            }
            env_define(env, ctor_name, ctor_value)
            env_define(
              env,
              pred_name,
              Value::ConditionProc(
                make_condition_proc(ConditionProcKind::Predicate(record_type)),
              ),
            )
            for binding in bindings {
              // invariant : bindings.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : bindings.length() >= 0
              env_define(
                env,
                binding.accessor,
                Value::ConditionProc(
                  make_condition_proc(
                    ConditionProcKind::Accessor(record_type, binding.index),
                  ),
                ),
              )
            }
            state = Apply(Void, next, handlers)
          }
          HashtableFindResult(table, key, idx, op, next) => {
            let result_value = expect_single(value)
            if is_false(result_value) {
              state = hashtable_find_state(
                table,
                key,
                idx + 1,
                op,
                next,
                handlers,
              )
            } else {
              state = hashtable_found_state(table, key, idx, op, next, handlers)
            }
          }
          HashtableUpdateApply(table, key, idx, found, next) => {
            let next_value = expect_single(value)
            if !table.mutable {
              raise @core.EvalError("hashtable is immutable")
            }
            if found {
              let entries = table.entries.val
              let entry = entries[idx]
              entry.value.val = next_value
            } else {
              let entries = table.entries.val
              entries.push(HashtableEntry::{ key, value: Ref::new(next_value) })
            }
            state = Apply(Void, next, handlers)
          }
          MapStep(proc, lists, results, for_each, next) => {
            let single = expect_single(value)
            let next_results = results
            if !for_each {
              next_results.push(value_to_datum_element(single))
            }
            state = map_step(
              proc, lists, next_results, for_each, next, handlers,
            )
          }
          VectorMapFinalize(next) => {
            let single = expect_single(value)
            let result_datum = value_as_datum(single)
            let items = datum_list_to_array(result_datum)
            state = Apply(
              Datum(Vector(items)),
              next,
              handlers,
            )
          }
          StringMapFinalize(next) => {
            let single = expect_single(value)
            let result_datum = value_as_datum(single)
            let string_value = apply_primitive(ListToString, [
              Value::Datum(result_datum),
            ])
            state = Apply(string_value, next, handlers)
          }
          WindEnter(thunk, winder, next) => {
            wind_push(winder)
            state = apply_proc(
              thunk,
              [],
              WindExit(winder, next),
              handlers,
            )
          }
          WindExit(winder, next) => {
            let actions = [WindAction::After(winder)]
            state = run_wind_actions(actions, value, next, handlers, handlers)
          }
          WindActions(actions, resume_value, final_kont, final_handlers) => {
            let _ = value
            state = run_wind_actions(
              actions, resume_value, final_kont, final_handlers, handlers,
            )
          }
          WindPush(
            winder,
            actions,
            resume_value,
            final_kont,
            final_handlers
          ) => {
            let _ = value
            wind_push(winder)
            state = run_wind_actions(
              actions, resume_value, final_kont, final_handlers, handlers,
            )
          }
          ForcePromise(promise, next) => {
            promise.state.val = Value(value)
            state = Apply(value, next, handlers)
          }
          RaiseNonCont =>
            raise @core.EvalError("non-continuable exception")
          RaiseCont(next) =>
            state = Apply(value, next, handlers)
          RestoreHandlers(prev_handlers, next) =>
            state = Apply(value, next, prev_handlers)
        }
    }
  }
  Void
}

///|
fn eval_expr(expr : Datum, env : Env) -> Value raise EvalError {
  eval_expr_impl(expr, env, true, true)
}

///|
fn eval_expr_transformer(expr : Datum, env : Env) -> Value raise EvalError {
  eval_expr_impl(expr, env, false, false)
}

///|

///|
fn eval_quote(args : Array[Datum]) -> Value raise EvalError {
  match args {
    [arg] => Datum(strip_syntax_datum(arg))
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_syntax(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args {
    [arg] => {
      let scope = next_syntax_context()
      let scopes = [scope]
      let binding_id = match datum_unlabel(arg) {
        Symbol(name) =>
          match env_lookup_optional(env, name) {
            Some(SyntaxObject(obj)) => obj.binding_id
            _ => env_binding_id_optional(env, name)
          }
        Value(SyntaxObject(obj)) => obj.binding_id
        _ => None
      }
      let datum = syntax_wrap_root(arg, scopes)
      SyntaxObject(SyntaxObject::{ datum, scopes, binding_id })
    }
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_quasiquote(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args {
    [arg] =>
      Datum(strip_syntax_datum(quasiquote_expand(arg, env, 1)))
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_quasisyntax(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args {
    [arg] => {
      let expanded = quasisyntax_expand(arg, env, 1)
      match expanded {
        Value(SyntaxObject(obj)) => SyntaxObject(obj)
        _ => {
          let scope = next_syntax_context()
          let scopes = [scope]
          let binding_id = match datum_unlabel(expanded) {
            Symbol(name) =>
              match env_lookup_optional(env, name) {
                Some(SyntaxObject(obj)) => obj.binding_id
                _ => env_binding_id_optional(env, name)
              }
            _ => None
          }
          let datum = syntax_wrap_root(expanded, scopes)
          SyntaxObject(SyntaxObject::{ datum, scopes, binding_id })
        }
      }
    }
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn eval_syntax_case_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 3 {
    raise arity_mismatch(3, args.length())
  } else {
    let rest = args.sub(start=1).to_array()
    Eval(
      args[0],
      env,
      SyntaxCase(rest, env, kont),
      handlers,
    )
  }
}

///|
fn eval_delay(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args {
    [arg] => {
      let thunk = Value::Closure(make_closure([], None, [arg], env))
      Value::Promise(make_promise(thunk))
    }
    _ => raise arity_mismatch(1, args.length())
  }
}

///|
fn unwrap_form(expr : Datum, name : String) -> Datum? {
  match datum_unwrap_syntax(expr) {
    Pair(head, tail) =>
      match datum_unwrap_syntax(head.val) {
        Symbol(head_name) if head_name == name =>
          match tail.val {
            Pair(arg, rest) =>
              match rest.val {
                Nil => Some(arg.val)
                _ => None
              }
            _ => None
          }
        _ => None
      }
    _ => None
  }
}

///|
fn quasiquote_expand(
  expr : Datum,
  env : Env,
  depth : Int,
) -> Datum raise EvalError {
  match unwrap_form(expr, "quasiquote") {
    Some(arg) =>
      return list_from_array([
        Symbol("quasiquote"),
        quasiquote_expand(arg, env, depth + 1),
      ])
    None => ()
  }
  match unwrap_form(expr, "unquote") {
    Some(arg) =>
      if depth == 1 {
        return value_to_datum_element(eval_expr(arg, env))
      } else {
        return list_from_array([
          Symbol("unquote"),
          quasiquote_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match unwrap_form(expr, "unquote-splicing") {
    Some(arg) =>
      if depth == 1 {
        raise @core.EvalError("unquote-splicing not in list context")
      } else {
        return list_from_array([
          Symbol("unquote-splicing"),
          quasiquote_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match expr {
    Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car.val, "unquote-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = value_as_datum(value)
            let items = datum_list_to_array(datum)
            let tail = quasiquote_expand(cdr.val, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      pair_new(
        quasiquote_expand(car.val, env, depth),
        quasiquote_expand(cdr.val, env, depth),
      )
    }
    Vector(items) => {
      let expanded : Array[Datum] = []
      if depth == 1 {
        for item in items {
          // invariant : items.length() >= 0
          // TODO(decreases) : loop index not exposed; possible bug
          // assert : items.length() >= 0
          match unwrap_form(item, "unquote-splicing") {
            Some(arg) => {
              let value = eval_expr(arg, env)
              let datum = value_as_datum(value)
              let list_items = datum_list_to_array(datum)
              for list_item in list_items {
                // invariant : list_items.length() >= 0
                // TODO(decreases) : loop index not exposed; possible bug
                // assert : list_items.length() >= 0
                expanded.push(list_item)
              }
            }
            None => expanded.push(quasiquote_expand(item, env, depth))
          }
        }
      } else {
        for item in items {
          // invariant : items.length() >= 0
          // TODO(decreases) : loop index not exposed; possible bug
          // assert : items.length() >= 0
          expanded.push(quasiquote_expand(item, env, depth))
        }
      }
      Vector(expanded)
    }
    _ => expr
  }
}

///|
fn quasisyntax_expand(
  expr : Datum,
  env : Env,
  depth : Int,
) -> Datum raise EvalError {
  match unwrap_form(expr, "quasisyntax") {
    Some(arg) =>
      return list_from_array([
        Symbol("quasisyntax"),
        quasisyntax_expand(arg, env, depth + 1),
      ])
    None => ()
  }
  match unwrap_form(expr, "unsyntax") {
    Some(arg) =>
      if depth == 1 {
        return value_to_datum_element(eval_expr(arg, env))
      } else {
        return list_from_array([
          Symbol("unsyntax"),
          quasisyntax_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match unwrap_form(expr, "unsyntax-splicing") {
    Some(arg) =>
      if depth == 1 {
        raise @core.EvalError("unsyntax-splicing not in list context")
      } else {
        return list_from_array([
          Symbol("unsyntax-splicing"),
          quasisyntax_expand(arg, env, depth - 1),
        ])
      }
    None => ()
  }
  match expr {
    Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car.val, "unsyntax-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = match value {
              SyntaxObject(obj) => obj.datum
              _ => value_as_datum(value)
            }
            let items = datum_list_to_array(datum)
            let tail = quasisyntax_expand(cdr.val, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      pair_new(
        quasisyntax_expand(car.val, env, depth),
        quasisyntax_expand(cdr.val, env, depth),
      )
    }
    Vector(items) => {
      let expanded : Array[Datum] = []
      if depth == 1 {
        for item in items {
          // invariant : items.length() >= 0
          // TODO(decreases) : loop index not exposed; possible bug
          // assert : items.length() >= 0
          match unwrap_form(item, "unsyntax-splicing") {
            Some(arg) => {
              let value = eval_expr(arg, env)
              let datum = match value {
                SyntaxObject(obj) => obj.datum
                _ => value_as_datum(value)
              }
              let list_items = datum_list_to_array(datum)
              for list_item in list_items {
                // invariant : list_items.length() >= 0
                // TODO(decreases) : loop index not exposed; possible bug
                // assert : list_items.length() >= 0
                expanded.push(list_item)
              }
            }
            None => expanded.push(quasisyntax_expand(item, env, depth))
          }
        }
      } else {
        for item in items {
          // invariant : items.length() >= 0
          // TODO(decreases) : loop index not exposed; possible bug
          // assert : items.length() >= 0
          expanded.push(quasisyntax_expand(item, env, depth))
        }
      }
      Vector(expanded)
    }
    _ => expr
  }
}

///|
fn list_with_tail(items : Array[Datum], tail : Datum) -> Datum {
  for i = items.length(), result = tail; i > 0; {
    // invariant : i >= 0 && i <= items.length()
    // decreases : i
    // assert : i <= items.length()
    let idx = i - 1
    continue i - 1, pair_new(items[idx], result)
  } else {
    result
  }
}

///|
fn parse_formals(expr : Datum) -> Formals raise EvalError {
  let names : Array[String] = []
  for cur = expr; true; {
    // invariant : names.length() >= 0
    // TODO(decreases) : list structure not explicit; possible bug
    // assert : names.length() >= 0
    match datum_unwrap_syntax(cur) {
      Nil => break Formals::{ params: names, rest: None }
      Symbol(name) => break Formals::{ params: names, rest: Some(name) }
      Pair(car, cdr) => {
        let name = parse_symbol(car.val)
        names.push(name)
        continue cdr.val
      }
      _ => raise @core.EvalError("type error: symbol expected")
    }
  } else {
    Formals::{ params: names, rest: None }
  }
}

///|
fn arity_mismatch_at_least(expected : Int, got : Int) -> EvalError {
  @core.EvalError("arity mismatch: expected at least \{expected} got \{got}")
}

///|
fn values_to_list_value(values : Array[Value]) -> Value {
  let items : Array[Datum] = []
  for value in values {
    // invariant : values.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : values.length() >= 0
    items.push(value_to_datum_element(value))
  }
  Datum(list_from_array(items))
}

///|
fn formals_bindings(
  formals : Formals,
  values : Array[Value],
) -> Array[(String, Value)] raise EvalError {
  let params = formals.params
  let min_len = params.length()
  match formals.rest {
    None =>
      if values.length() != min_len {
        raise arity_mismatch(min_len, values.length())
      }
    Some(_) =>
      if values.length() < min_len {
        raise arity_mismatch_at_least(min_len, values.length())
      }
  }
  let bindings : Array[(String, Value)] = []
  for i = 0; i < params.length(); {
    // invariant : i >= 0 && i <= params.length()
    // decreases : params.length() - i
    // assert : i <= params.length()
    bindings.push((params[i], values[i]))
    continue i + 1
  }
  match formals.rest {
    Some(name) => {
      let rest_values : Array[Value] = []
      for j = min_len; j < values.length(); {
        // invariant : j >= min_len && j <= values.length()
        // decreases : values.length() - j
        // assert : j <= values.length()
        rest_values.push(values[j])
        continue j + 1
      }
      bindings.push((name, values_to_list_value(rest_values)))
    }
    None => ()
  }
  bindings
}

///|
fn formals_accept_len(formals : Formals, arg_len : Int) -> Bool {
  match formals.rest {
    None => arg_len == formals.params.length()
    Some(_) => arg_len >= formals.params.length()
  }
}

///|
fn formals_expected_desc(formals : Formals) -> String {
  match formals.rest {
    None => formals.params.length().to_string()
    Some(_) => ">=\{formals.params.length()}"
  }
}

///|
fn eval_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let formals = parse_formals(args[0])
    let body = args.sub(start=1).to_array()
    Closure(make_closure(formals.params, formals.rest, body, env))
  }
}

///|
fn eval_case_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.is_empty() {
    raise arity_mismatch(1, 0)
  }
  let clauses : Array[CaseClause] = args.map((clause) => {
    let parts = datum_list_to_array(clause)
    if parts.length() < 2 {
      raise @core.EvalError("invalid case-lambda")
    }
    let formals = parse_formals(parts[0])
    let body = parts.sub(start=1).to_array()
    CaseClause::{
      params: formals.params,
      rest: formals.rest,
      body,
    }
  })
  CaseClosure(make_case_closure(clauses, env))
}

///|
fn eval_define_syntax(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args {
    [name_expr, transformer_expr] => {
      let name = parse_symbol(name_expr)
      let transformer = parse_transformer(transformer_expr, name, env)
      env_define(env, name, Macro(transformer))
      Void
    }
    _ => raise arity_mismatch(2, args.length())
  }
}

///|
fn parse_syntax_bindings(
  expr : Datum,
  env : Env,
) -> Array[(String, MacroTransformer)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, MacroTransformer)] = []
  for binding in bindings {
    // invariant : bindings.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : bindings.length() >= 0
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise @core.EvalError("type error: syntax binding expected")
    } else {
      let name = parse_symbol(parts[0])
      let transformer = parse_transformer(parts[1], name, env)
      parsed.push((name, transformer))
    }
  }
  parsed
}

///|
fn parse_bindings(expr : Datum) -> Array[(String, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, Datum)] = []
  for binding in bindings {
    // invariant : bindings.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : bindings.length() >= 0
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise @core.EvalError("type error: binding expected")
    } else {
      let name = parse_symbol(parts[0])
      parsed.push((name, parts[1]))
    }
  }
  parsed
}

///|
fn parse_value_bindings(
  expr : Datum,
) -> Array[(Formals, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(Formals, Datum)] = []
  for binding in bindings {
    // invariant : bindings.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : bindings.length() >= 0
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise @core.EvalError("invalid let-values")
    } else {
      let formals = parse_formals(parts[0])
      parsed.push((formals, parts[1]))
    }
  }
  parsed
}

///|
fn parse_parameterize_bindings(
  expr : Datum,
) -> Array[(Datum, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(Datum, Datum)] = []
  for binding in bindings {
    // invariant : bindings.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : bindings.length() >= 0
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise @core.EvalError("invalid parameterize")
    } else {
      parsed.push((parts[0], parts[1]))
    }
  }
  parsed
}

///|

///|
fn parse_symbol(expr : Datum) -> String raise EvalError {
  match symbol_name(datum_unwrap_syntax(expr)) {
    Some(name) => name
    None => raise @core.EvalError("type error: symbol expected")
  }
}

///|
fn parse_bool(expr : Datum) -> Bool raise EvalError {
  match expr {
    Bool(value) => value
    _ => raise @core.EvalError("invalid define-record-type")
  }
}

///|
fn parse_symbol_list(items : Array[Datum]) -> Array[String] raise EvalError {
  let names : Array[String] = []
  for item in items {
    // invariant : items.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : items.length() >= 0
    names.push(parse_symbol(item))
  }
  names
}

///|
fn expect_single(value : Value) -> Value raise EvalError {
  match value {
    Values(values) =>
      if values.length() == 1 {
        values[0]
      } else {
        raise @core.EvalError("multiple values in single-value context")
      }
    _ => value
  }
}

///|
fn value_to_values(value : Value) -> Array[Value] {
  match value {
    Values(values) => values
    _ => {
      let values : Array[Value] = []
      values.push(value)
      values
    }
  }
}

///|
fn raise_to_handlers(
  condition : Value,
  handlers : Array[Value],
  kont : Kont,
  continuable : Bool,
) -> MachineState raise EvalError {
  if handlers.is_empty() {
    raise @core.EvalError("uncaught exception")
  } else {
    let handler = handlers[handlers.length() - 1]
    if continuable {
      apply_proc(handler, [condition], RaiseCont(kont), handlers)
    } else {
      apply_proc(handler, [condition], RaiseNonCont, handlers)
    }
  }
}

///|
fn guard_start(
  clauses : Array[Datum],
  env : Env,
  info : GuardInfo,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i <= clauses.length()
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      continue i + 1
    } else {
      let head = symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, GuardResult(info), handlers)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return Eval(
          parts[0],
          env,
          GuardCond(parts, rest, env, info),
          handlers,
        )
      }
    }
  }
  raise_to_handlers(
    info.condition,
    info.handlers,
    info.raise_kont,
    info.continuable,
  )
}

///|
fn handle_guard(
  handler : GuardHandler,
  condition : Value,
  raise_kont : Kont,
  continuable : Bool,
) -> MachineState raise EvalError {
  let guard_env = env_extend(handler.env)
  env_define(guard_env, handler.name, condition)
  let info = GuardInfo::{
    condition,
    resume_value: handler.resume_value,
    handlers: handler.handlers,
    raise_kont,
    continuable,
  }
  guard_start(handler.clauses, guard_env, info, handler.handlers)
}

///|
fn wind_push(winder : Winder) -> Unit {
  let next : Array[Winder] = []
  for item in wind_stack.val {
    // invariant : wind_stack.val.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : wind_stack.val.length() >= 0
    next.push(item)
  }
  next.push(winder)
  wind_stack.val = next
}

///|
fn wind_pop() -> Winder {
  let current = wind_stack.val
  let last_index = current.length() - 1
  let winder = current[last_index]
  let next = current.sub(start=0, end=last_index).to_array()
  wind_stack.val = next
  winder
}

///|
fn apply_winder_before(
  winder : Winder,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match winder.kind {
    Proc(before, _) => apply_proc(before, [], kont, handlers)
    Params(bindings) => {
      for binding in bindings {
        // invariant : bindings.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : bindings.length() >= 0
        parameter_set(binding.param, binding.new_value)
      }
      Apply(Void, kont, handlers)
    }
  }
}

///|
fn apply_winder_after(
  winder : Winder,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match winder.kind {
    Proc(_, after) => apply_proc(after, [], kont, handlers)
    Params(bindings) => {
      for binding in bindings {
        // invariant : bindings.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : bindings.length() >= 0
        parameter_set(binding.param, binding.old_value)
      }
      Apply(Void, kont, handlers)
    }
  }
}

///|
fn common_wind_prefix(current : Array[Winder], target : Array[Winder]) -> Int {
  for i = 0; i < current.length() && i < target.length(); {
    // invariant : i >= 0 && i <= current.length()
    // invariant : i >= 0 && i <= target.length()
    // decreases : (if current.length() < target.length() { current.length() } else { target.length() }) - i
    // assert : i <= current.length() && i <= target.length()
    if current[i].id != target[i].id {
      break i
    }
    continue i + 1
  } else {
    i
  }
}

///|

///|
fn run_wind_actions(
  actions : Array[WindAction],
  resume_value : Value,
  final_kont : Kont,
  final_handlers : Array[Value],
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if actions.is_empty() {
    Apply(resume_value, final_kont, final_handlers)
  } else {
    let rest = actions.sub(start=1).to_array()
    match actions[0] {
      After(winder) => {
        let _ = wind_pop()
        apply_winder_after(
          winder,
          WindActions(rest, resume_value, final_kont, final_handlers),
          handlers,
        )
      }
      Before(winder) =>
        apply_winder_before(
          winder,
          WindPush(winder, rest, resume_value, final_kont, final_handlers),
          handlers,
        )
      SwitchHandlers(next_handlers) =>
        run_wind_actions(
          rest, resume_value, final_kont, final_handlers, next_handlers,
        )
    }
  }
}

///|
fn eval_sequence_state(
  exprs : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if exprs.is_empty() {
    Apply(Void, kont, handlers)
  } else {
    let rest = exprs.sub(start=1).to_array()
    Eval(exprs[0], env, Begin(rest, env, kont), handlers)
  }
}

///|
fn eval_begin_for_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let syntax_env = env_clone(env)
  for form in args {
    // invariant : args.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : args.length() >= 0
    let _ = eval_expr(form, syntax_env)

  }
  let frame = syntax_env[syntax_env.length() - 1]
  for key in frame.keys() {
    // invariant : frame.keys().length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : frame.keys().length() >= 0
    match frame.get(key) {
      Some(binding) =>
        match binding.value {
          Macro(rules) => env_define(env, key, Macro(rules))
          _ => ()
        }
      _ => ()
    }
  }
  Apply(Void, kont, handlers)
}

///|
fn eval_if_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match args {
    [test_expr, then_expr, else_expr] =>
      Eval(
        test_expr,
        env,
        If(then_expr, else_expr, env, kont),
        handlers,
      )
    _ => raise arity_mismatch(3, args.length())
  }
}

///|
fn eval_define_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match datum_unwrap_syntax(args[0]) {
      Symbol(_) => {
        let name = parse_symbol(args[0])
        match args {
          [_, value_expr] =>
            Eval(
              value_expr,
              env,
              Define(name, env, kont),
              handlers,
            )
          _ => raise arity_mismatch(2, args.length())
        }
      }
      Pair(head, tail) => {
        let name = parse_symbol(head.val)
        let formals = parse_formals(tail.val)
        let body = args.sub(start=1).to_array()
        env_define(
          env,
          name,
          Closure(make_closure(formals.params, formals.rest, body, env)),
        )
        Apply(Void, kont, handlers)
      }
      _ => raise @core.EvalError("type error: symbol expected")
    }
  }
}

///|
fn eval_define_values_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match args {
    [formals_expr, value_expr] => {
      let formals = parse_formals(formals_expr)
      Eval(
        value_expr,
        env,
        DefineValues(formals, env, kont),
        handlers,
      )
    }
    _ => raise arity_mismatch(2, args.length())
  }
}

///|
fn eval_guard_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let spec = datum_list_to_array(args[0])
    if spec.is_empty() {
      raise @core.EvalError("invalid guard")
    }
    let name = parse_symbol(spec[0])
    let clauses = spec.sub(start=1).to_array()
    let body = args.sub(start=1).to_array()
    let resume_value = Value::Continuation(
      capture_continuation(kont, handlers),
    )
    let guard_handlers : Array[Value] = []
    for item in handlers {
      // invariant : handlers.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : handlers.length() >= 0
      guard_handlers.push(item)
    }
    let handler = Value::GuardHandler(
      make_guard_handler(name, clauses, env, resume_value, guard_handlers),
    )
    let next_handlers : Array[Value] = []
    for item in handlers {
      // invariant : handlers.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : handlers.length() >= 0
      next_handlers.push(item)
    }
    next_handlers.push(handler)
    eval_sequence_state(
      body,
      env,
      Kont::RestoreHandlers(handlers, kont),
      next_handlers,
    )
  }
}

///|
fn eval_parameterize_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_parameterize_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (param_expr, _) = bindings[0]
      Eval(
        param_expr,
        env,
        ParameterizeParam(bindings, 0, [], [], env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_set_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match args {
    [name_expr, value_expr] => {
      let name = parse_symbol(name_expr)
      match env_get_macro(env, name) {
        Some(transformer) => {
          let expansion = expand_macro(
            transformer,
            list_from_array([
              Symbol("set!"),
              Symbol(name),
              value_expr,
            ]),
            env,
          )
          let next_env = env_with_captures(env, expansion.captures)
          Eval(expansion.datum, next_env, kont, handlers)
        }
        None =>
          Eval(
            value_expr,
            env,
            Set(name, env, kont),
            handlers,
          )
      }
    }
    _ => raise arity_mismatch(2, args.length())
  }
}

///|
fn eval_let_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Symbol(name) => {
        if args.length() < 3 {
          raise arity_mismatch(3, args.length())
        }
        let bindings = parse_bindings(args[1])
        let body = args.sub(start=2).to_array()
        let expanded = expand_named_let(name, bindings, body)
        Eval(expanded, env, kont, handlers)
      }
      _ => {
        let bindings = parse_bindings(args[0])
        let body = args.sub(start=1).to_array()
        if bindings.is_empty() {
          eval_sequence_state(body, env, kont, handlers)
        } else {
          let (_, expr) = bindings[0]
          Eval(
            expr,
            env,
            Let(bindings, 0, [], env, body, kont),
            handlers,
          )
        }
      }
    }
  }
}

///|

///|
fn eval_let_values_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_value_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      Eval(
        expr,
        env,
        LetValues(bindings, 0, [], env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_let_star_values_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_value_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      Eval(
        expr,
        env,
        LetStarValues(bindings, 0, env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_let_star_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      Eval(
        expr,
        env,
        LetStar(bindings, 0, env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_letrec_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      // invariant : bindings.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : bindings.length() >= 0
      let (name, _) = binding
      env_define(new_env, name, Void)
    }
    if bindings.is_empty() {
      eval_sequence_state(body, new_env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      Eval(
        expr,
        new_env,
        LetRecInit(bindings, 0, [], new_env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn eval_letrec_star_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      // invariant : bindings.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : bindings.length() >= 0
      let (name, _) = binding
      env_define(new_env, name, Void)
    }
    if bindings.is_empty() {
      eval_sequence_state(body, new_env, kont, handlers)
    } else {
      let (_, expr) = bindings[0]
      Eval(
        expr,
        new_env,
        LetRec(bindings, 0, new_env, body, kont),
        handlers,
      )
    }
  }
}

///|
fn expand_named_let(
  name : String,
  bindings : Array[(String, Datum)],
  body : Array[Datum],
) -> Datum {
  let param_items : Array[Datum] = []
  let arg_items : Array[Datum] = []
  for binding in bindings {
    // invariant : bindings.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : bindings.length() >= 0
    let (param, expr) = binding
    param_items.push(Symbol(param))
    arg_items.push(expr)
  }
  let lambda_items : Array[Datum] = []
  lambda_items.push(Symbol("lambda"))
  lambda_items.push(list_from_array(param_items))
  for expr in body {
    // invariant : body.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : body.length() >= 0
    lambda_items.push(expr)
  }
  let lambda_expr = list_from_array(lambda_items)
  let binding_expr = list_from_array([Symbol(name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let call_items : Array[Datum] = []
  call_items.push(Symbol(name))
  for arg in arg_items {
    // invariant : arg_items.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : arg_items.length() >= 0
    call_items.push(arg)
  }
  let call_expr = list_from_array(call_items)
  list_from_array([Symbol("letrec"), bindings_expr, call_expr])
}

///|
fn eval_let_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0], env)
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      // invariant : bindings.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : bindings.length() >= 0
      let (name, transformer) = binding
      env_define(new_env, name, Macro(transformer))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn eval_letrec_syntax_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let new_env = env_extend(env)
    let binding_specs = datum_list_to_array(args[0])
    for i = 0; i < binding_specs.length(); {
      // invariant : i >= 0 && i <= binding_specs.length()
      // decreases : binding_specs.length() - i
      // assert : i <= binding_specs.length()
      let parts = datum_list_to_array(binding_specs[i])
      if parts.length() != 2 {
        raise @core.EvalError("type error: syntax binding expected")
      }
      let name = parse_symbol(parts[0])
      let placeholder = MacroTransformer::Rules(SyntaxRules::{
        literals: [],
        rules: [],
        ellipsis: "...",
        kind: SyntaxRulesKind::SyntaxRules,
        def_env: new_env,
      })
      env_define(new_env, name, Value::Macro(placeholder))
      continue i + 1
    }
    let bindings = parse_syntax_bindings(args[0], new_env)
    let body = args.sub(start=1).to_array()
    for binding in bindings {
      // invariant : bindings.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : bindings.length() >= 0
      let (name, transformer) = binding
      env_define(new_env, name, Value::Macro(transformer))
    }
    eval_sequence_state(body, new_env, kont, handlers)
  }
}

///|
fn build_begin(exprs : Array[Datum]) -> Datum {
  let items : Array[Datum] = []
  items.push(Symbol("begin"))
  for expr in exprs {
    // invariant : exprs.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : exprs.length() >= 0
    items.push(expr)
  }
  list_from_array(items)
}

///|
fn parse_do_bindings(
  expr : Datum,
) -> (Array[String], Array[Datum], Array[Datum]) raise EvalError {
  let bindings = datum_list_to_array(expr)
  let names : Array[String] = []
  let inits : Array[Datum] = []
  let steps : Array[Datum] = []
  for binding in bindings {
    // invariant : bindings.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : bindings.length() >= 0
    let parts = datum_list_to_array(binding)
    if parts.length() < 2 || parts.length() > 3 {
      raise @core.EvalError("invalid do")
    }
    let name = parse_symbol(parts[0])
    names.push(name)
    inits.push(parts[1])
    if parts.length() == 3 {
      steps.push(parts[2])
    } else {
      steps.push(Symbol(name))
    }
  }
  (names, inits, steps)
}

///|
fn eval_do_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let (names, inits, steps) = parse_do_bindings(args[0])
  let test_parts = datum_list_to_array(args[1])
  if test_parts.is_empty() {
    raise @core.EvalError("invalid do")
  }
  let test_expr = test_parts[0]
  let result_exprs = test_parts.sub(start=1).to_array()
  let command_exprs = args.sub(start=2).to_array()
  let loop_name = next_do_name()
  let params : Array[Datum] = []
  for name in names {
    // invariant : names.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : names.length() >= 0
    params.push(Symbol(name))
  }
  let call_items : Array[Datum] = []
  call_items.push(Symbol(loop_name))
  for step in steps {
    // invariant : steps.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : steps.length() >= 0
    call_items.push(step)
  }
  let call_expr = list_from_array(call_items)
  let else_body : Array[Datum] = []
  for expr in command_exprs {
    // invariant : command_exprs.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : command_exprs.length() >= 0
    else_body.push(expr)
  }
  else_body.push(call_expr)
  let then_expr = build_begin(result_exprs)
  let else_expr = build_begin(else_body)
  let if_expr = list_from_array([
    Symbol("if"),
    test_expr,
    then_expr,
    else_expr,
  ])
  let lambda_expr = list_from_array([
    Symbol("lambda"),
    list_from_array(params),
    if_expr,
  ])
  let binding_expr = list_from_array([Symbol(loop_name), lambda_expr])
  let bindings_expr = list_from_array([binding_expr])
  let init_call_items : Array[Datum] = []
  init_call_items.push(Symbol(loop_name))
  for init in inits {
    // invariant : inits.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : inits.length() >= 0
    init_call_items.push(init)
  }
  let init_call_expr = list_from_array(init_call_items)
  let expanded = list_from_array([
    Symbol("letrec"),
    bindings_expr,
    init_call_expr,
  ])
  Eval(expanded, env, kont, handlers)
}

///|
fn eval_define_record_type_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 3 {
    raise @core.EvalError("invalid define-record-type")
  }
  let record_name = parse_symbol(args[0])
  let ctor_parts = datum_list_to_array(args[1])
  if ctor_parts.is_empty() {
    raise @core.EvalError("invalid define-record-type")
  }
  let ctor_name = parse_symbol(ctor_parts[0])
  let ctor_fields = parse_symbol_list(ctor_parts.sub(start=1).to_array())
  let pred_name = parse_symbol(args[2])
  let raw_specs = args.sub(start=3).to_array()
  let field_specs : Array[Datum] = []
  let (parent_name, protocol_expr, is_sealed, is_opaque, uid) =
    for i = 0,
      parent_name = None,
      protocol_expr = None,
      is_sealed = false,
      is_opaque = false,
      uid = None;
      i < raw_specs.length(); {
      // invariant : i >= 0 && i <= raw_specs.length()
      // decreases : raw_specs.length() - i
      // assert : i <= raw_specs.length()
      let spec = raw_specs[i]
      let parts = datum_list_to_array(spec)
      if parts.length() >= 1 && parts.length() <= 2 {
        match symbol_name(parts[0]) {
          Some("parent") if parts.length() == 2 => {
            if parent_name is Some(_) {
              raise @core.EvalError("invalid define-record-type")
            }
            let parent_name = Some(parse_symbol(parts[1]))
            continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
          }
          Some("protocol") if parts.length() == 2 => {
            if protocol_expr is Some(_) {
              raise @core.EvalError("invalid define-record-type")
            }
            let protocol_expr = Some(parts[1])
            continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
          }
          Some("sealed") => {
            let is_sealed = if parts.length() == 1 {
              true
            } else {
              parse_bool(parts[1])
            }
            continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
          }
          Some("opaque") => {
            let is_opaque = if parts.length() == 1 {
              true
            } else {
              parse_bool(parts[1])
            }
            continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
          }
          Some("nongenerative") => {
            if uid is Some(_) {
              raise @core.EvalError("invalid define-record-type")
            }
            let uid = if parts.length() == 1 {
              Some(record_name)
            } else {
              Some(parse_symbol(parts[1]))
            }
            continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
          }
          _ => ()
        }
      }
      field_specs.push(spec)
      continue i + 1, parent_name, protocol_expr, is_sealed, is_opaque, uid
    } else {
      (parent_name, protocol_expr, is_sealed, is_opaque, uid)
    }
  let parent_desc = match parent_name {
    Some(name) =>
      match lookup_record_type_descriptor(name) {
        Some(desc) => Some(desc)
        None => raise @core.EvalError("invalid define-record-type")
      }
    None => None
  }
  let parent_type = match parent_desc {
    Some(desc) => Some(desc.record_type)
    None => None
  }
  match parent_type {
    Some(parent) =>
      if parent.is_sealed {
        raise @core.EvalError("invalid define-record-type")
      }
    None => ()
  }
  let parent_fields = match parent_type {
    Some(record_type) => record_type.fields
    None => []
  }
  let parent_count = parent_fields.length()
  let parent_field_names : Array[String] = []
  for field in parent_fields {
    // invariant : parent_fields.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : parent_fields.length() >= 0
    parent_field_names.push(field.name)
  }
  let accessors : Map[String, String] = {}
  let mutators : Map[String, String] = {}
  let mutable : Map[String, Bool] = {}
  let new_field_names : Array[String] = []
  for spec in field_specs {
    // invariant : field_specs.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : field_specs.length() >= 0
    let parts = datum_list_to_array(spec)
    if parts.length() < 2 || parts.length() > 3 {
      raise @core.EvalError("invalid define-record-type")
    }
    let field_name = parse_symbol(parts[0])
    if array_contains_name(parent_field_names, field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    if accessors.contains(field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    let accessor = parse_symbol(parts[1])
    accessors[field_name] = accessor
    if parts.length() == 3 {
      let mutator = parse_symbol(parts[2])
      mutators[field_name] = mutator
      mutable[field_name] = true
    } else {
      mutable[field_name] = false
    }
    new_field_names.push(field_name)
  }
  if parent_count == 0 {
    if ctor_fields.length() != new_field_names.length() {
      raise @core.EvalError("invalid define-record-type")
    }
  } else if ctor_fields.length() != parent_count + new_field_names.length() {
    raise @core.EvalError("invalid define-record-type")
  }
  let fields : Array[RecordField] = []
  let field_index : Map[String, Int] = {}
  for i = 0; i < ctor_fields.length(); {
    // invariant : i >= 0 && i <= ctor_fields.length()
    // decreases : ctor_fields.length() - i
    // assert : i <= ctor_fields.length()
    let field_name = ctor_fields[i]
    if field_index.contains(field_name) {
      raise @core.EvalError("invalid define-record-type")
    }
    if i < parent_count {
      let parent_field = parent_fields[i]
      if parent_field.name != field_name {
        raise @core.EvalError("invalid define-record-type")
      }
      fields.push(parent_field)
    } else {
      let is_mutable = match mutable.get(field_name) {
        Some(flag) => flag
        None => raise @core.EvalError("invalid define-record-type")
      }
      fields.push(RecordField::{ name: field_name, mutable: is_mutable })
    }
    field_index[field_name] = i
    continue i + 1
  }
  let bindings : Array[RecordFieldBinding] = []
  for field_name in new_field_names {
    // invariant : new_field_names.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : new_field_names.length() >= 0
    let idx = match field_index.get(field_name) {
      Some(value) => value
      None => raise @core.EvalError("invalid define-record-type")
    }
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise @core.EvalError("invalid define-record-type")
    }
    let mutator_name = mutators.get(field_name)
    bindings.push(RecordFieldBinding::{
      accessor: accessor_name,
      index: idx,
      mutator: mutator_name,
    })
  }
  let existing_desc = match uid {
    Some(uid_name) => lookup_record_type_descriptor_by_uid(uid_name)
    None => None
  }
  let (record_type, ctor_desc) = match existing_desc {
    Some(desc) => {
      if !record_type_matches(
          desc.record_type,
          parent_type,
          is_sealed,
          is_opaque,
          fields,
        ) {
        raise @core.EvalError("invalid define-record-type")
      }
      if protocol_expr is Some(_) {
        raise @core.EvalError("invalid define-record-type")
      }
      register_record_type_alias(record_name, desc)
      (desc.record_type, desc.constructor_desc)
    }
    None => {
      let effective_opaque = match parent_type {
        Some(parent) => parent.is_opaque || is_opaque
        None => is_opaque
      }
      let record_type = make_record_type(
        record_name, parent_type, is_sealed, effective_opaque, uid, fields,
      )
      let protocol_value = match protocol_expr {
        Some(expr) => {
          let proc = eval_expr(expr, env)
          if !is_procedure_value(proc) {
            raise @core.EvalError("type error: procedure expected")
          }
          Some(proc)
        }
        None => None
      }
      let parent_ctor_desc = match parent_desc {
        Some(desc) => Some(desc.constructor_desc)
        None => None
      }
      let ctor_desc = make_record_constructor_descriptor(
        record_type, parent_ctor_desc, protocol_value,
      )
      let rtd = make_record_type_descriptor(record_type, ctor_desc)
      let _ = register_record_type(record_name, rtd)
      (record_type, ctor_desc)
    }
  }
  let next_kont = Kont::DefineRecordType(
    ctor_name, pred_name, record_type, bindings, env, kont,
  )
  record_constructor_from_descriptor(ctor_desc, next_kont, handlers)
}

///|
fn eval_define_condition_type_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 4 {
    raise @core.EvalError("invalid define-condition-type")
  }
  let record_name = parse_symbol(args[0])
  let parent_name = parse_symbol(args[1])
  let ctor_name = parse_symbol(args[2])
  let pred_name = parse_symbol(args[3])
  let field_specs = args.sub(start=4).to_array()
  match lookup_record_type_descriptor(record_name) {
    Some(_) => raise @core.EvalError("invalid define-condition-type")
    None => ()
  }
  let parent_desc = match lookup_record_type_descriptor(parent_name) {
    Some(desc) => desc
    None => raise @core.EvalError("invalid define-condition-type")
  }
  let parent_type = parent_desc.record_type
  if parent_type.is_sealed || parent_type.is_opaque {
    raise @core.EvalError("invalid define-condition-type")
  }
  let base_type = condition_base_type()
  if !record_type_is_a(parent_type, base_type) {
    raise @core.EvalError("invalid define-condition-type")
  }
  let parent_fields = parent_type.fields
  let parent_field_names : Array[String] = []
  for field in parent_fields {
    // invariant : parent_fields.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : parent_fields.length() >= 0
    parent_field_names.push(field.name)
  }
  let accessors : Map[String, String] = {}
  let new_field_names : Array[String] = []
  for spec in field_specs {
    // invariant : field_specs.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : field_specs.length() >= 0
    let parts = datum_list_to_array(spec)
    if parts.length() != 2 {
      raise @core.EvalError("invalid define-condition-type")
    }
    let field_name = parse_symbol(parts[0])
    if array_contains_name(parent_field_names, field_name) {
      raise @core.EvalError("invalid define-condition-type")
    }
    if accessors.contains(field_name) {
      raise @core.EvalError("invalid define-condition-type")
    }
    let accessor = parse_symbol(parts[1])
    accessors[field_name] = accessor
    new_field_names.push(field_name)
  }
  let fields : Array[RecordField] = []
  for field in parent_fields {
    // invariant : parent_fields.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : parent_fields.length() >= 0
    fields.push(field)
  }
  for field_name in new_field_names {
    // invariant : new_field_names.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : new_field_names.length() >= 0
    fields.push(RecordField::{ name: field_name, mutable: false })
  }
  let bindings : Array[RecordFieldBinding] = []
  let parent_count = parent_fields.length()
  for i = 0; i < new_field_names.length(); {
    // invariant : i >= 0 && i <= new_field_names.length()
    // decreases : new_field_names.length() - i
    // assert : i <= new_field_names.length()
    let field_name = new_field_names[i]
    let accessor_name = match accessors.get(field_name) {
      Some(name) => name
      None => raise @core.EvalError("invalid define-condition-type")
    }
    bindings.push(RecordFieldBinding::{
      accessor: accessor_name,
      index: parent_count + i,
      mutator: None,
    })
    continue i + 1
  }
  let record_type = make_record_type(
    record_name,
    Some(parent_type),
    false,
    false,
    None,
    fields,
  )
  let ctor_desc = make_record_constructor_descriptor(
    record_type,
    Some(parent_desc.constructor_desc),
    None,
  )
  let rtd = make_record_type_descriptor(record_type, ctor_desc)
  let _ = register_record_type(record_name, rtd)
  env_define(env, record_name, RecordTypeDescriptor(rtd))
  let next_kont = Kont::DefineConditionType(
    ctor_name, pred_name, record_type, bindings, env, kont,
  )
  record_constructor_from_descriptor(ctor_desc, next_kont, handlers)
}

///|
fn eval_define_enumeration_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise @core.EvalError("invalid define-enumeration")
  }
  let type_name = parse_symbol(args[0])
  let symbols = parse_symbol_list(datum_list_to_array(args[1]))
  let ctor_name = parse_symbol(args[2])
  let seen : Map[String, Bool] = {}
  for name in symbols {
    // invariant : symbols.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : symbols.length() >= 0
    if seen.contains(name) {
      raise @core.EvalError("invalid define-enumeration")
    }
    seen[name] = true
  }
  let members = enum_set_members_all_true(symbols)
  let universe = make_enum_set(symbols, members)
  env_define(env, type_name, Value::EnumSet(universe))
  env_define(
    env,
    ctor_name,
    Value::EnumSetProc(
      make_enum_set_proc(EnumSetProcKind::Constructor(universe)),
    ),
  )
  Apply(Value::Void, kont, handlers)
}

///|

///|

///|
fn array_contains_name(items : Array[String], name : String) -> Bool {
  for item in items {
    // invariant : items.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : items.length() >= 0
    if item == name {
      return true
    }
  }
  false
}

///|
fn value_as_record(value : Value) -> Record raise EvalError {
  match value {
    Record(record) => record
    _ => raise @core.EvalError("type error: record expected")
  }
}

///|
fn record_constructor_from_descriptor(
  desc : RecordConstructorDescriptor,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match desc.protocol {
    None => {
      let ctor = Value::RecordProc(
        make_record_proc(
          RecordProcKind::Constructor(desc.record_type),
        ),
      )
      Apply(ctor, kont, handlers)
    }
    Some(protocol) => {
      if !is_procedure_value(protocol) {
        raise @core.EvalError("type error: procedure expected")
      }
      let arg = match desc.record_type.parent {
        None =>
          Value::RecordProc(
            make_record_proc(
              RecordProcKind::Constructor(desc.record_type),
            ),
          )
        Some(parent) => {
          let parent_desc = match desc.parent_desc {
            Some(p) => p
            None => default_constructor_descriptor(parent)
          }
          Value::RecordProc(
            make_record_proc(
              RecordProcKind::ProtocolN(desc.record_type, parent_desc),
            ),
          )
        }
      }
      apply_proc(
        protocol,
        [arg],
        Kont::RecordConstructorDone(kont),
        handlers,
      )
    }
  }
}

///|
fn apply_record_proc(
  proc : RecordProc,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc.kind {
    Constructor(record_type) => {
      let field_count = record_type.fields.length()
      if args.length() != field_count {
        raise arity_mismatch(field_count, args.length())
      } else {
        let record = make_record_instance(record_type, args)
        Apply(Record(record), kont, handlers)
      }
    }
    Predicate(record_type) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let result = match args[0] {
          Record(record) =>
            record_type_is_a(record.record_type, record_type)
          _ => false
        }
        Apply(bool_value(result), kont, handlers)
      }
    Accessor(record_type, idx) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let record = value_as_record(args[0])
        if !record_type_is_a(record.record_type, record_type) {
          raise @core.EvalError(
            "type error: record type mismatch: expected \{record_type.name}",
          )
        }
        Apply(record.fields[idx].val, kont, handlers)
      }
    Mutator(record_type, idx) =>
      if args.length() != 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let record = value_as_record(args[0])
        if !record_type_is_a(record.record_type, record_type) {
          raise @core.EvalError(
            "type error: record type mismatch: expected \{record_type.name}",
          )
        }
        let field = record.record_type.fields[idx]
        if !field.mutable {
          raise @core.EvalError("record field is immutable: \{field.name}")
        }
        record.fields[idx].val = args[1]
        Apply(Void, kont, handlers)
      }
    ProtocolN(record_type, parent_desc) => {
      let parent_type = match record_type.parent {
        Some(parent) => parent
        None => raise @core.EvalError("invalid record constructor descriptor")
      }
      match parent_desc.protocol {
        None => {
          if args.length() != parent_type.fields.length() {
            raise arity_mismatch(parent_type.fields.length(), args.length())
          }
          let parent_values = args.copy()
          let proc = Value::RecordProc(
            make_record_proc(
              RecordProcKind::ProtocolP(record_type, parent_values),
            ),
          )
          Apply(proc, kont, handlers)
        }
        Some(_) => {
          let parent_args = args.copy()
          record_constructor_from_descriptor(
            parent_desc,
            Kont::RecordProtocolNApply(record_type, parent_args, kont),
            handlers,
          )
        }
      }
    }
    ProtocolP(record_type, parent_values) => {
      let total_fields = record_type.fields.length()
      if parent_values.length() > total_fields {
        raise @core.EvalError("invalid record constructor descriptor")
      }
      let child_count = total_fields - parent_values.length()
      if args.length() != child_count {
        raise arity_mismatch(child_count, args.length())
      }
      let values = parent_values.copy()
      values.push_iter(args.iter())
      let record = make_record_instance(record_type, values)
      Apply(Record(record), kont, handlers)
    }
  }
}

///|
fn apply_condition_proc(
  proc : ConditionProc,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc.kind {
    Predicate(record_type) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let base = condition_base_type()
        let ok = match condition_components_opt(args[0], base) {
          Some(components) =>
            for i = 0; i < components.length(); {
              // invariant : i >= 0 && i <= components.length()
              // decreases : components.length() - i
              // assert : i <= components.length()
              if record_type_is_a(components[i].record_type, record_type) {
                break true
              }
              continue i + 1
            } else {
              false
            }
          None => false
        }
        Apply(bool_value(ok), kont, handlers)
      }
    Accessor(record_type, idx) =>
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let base = condition_base_type()
        let components = condition_components(args[0], base)
        let found =
          for i = 0; i < components.length(); {
            // invariant : i >= 0 && i <= components.length()
            // decreases : components.length() - i
            // assert : i <= components.length()
            let record = components[i]
            if record_type_is_a(record.record_type, record_type) {
              break Some(record)
            }
            continue i + 1
          } else {
            None
          }
        match found {
          Some(record) =>
            Apply(record.fields[idx].val, kont, handlers)
          None =>
            raise @core.EvalError(
              "type error: condition type mismatch: expected \{record_type.name}",
            )
        }
      }
  }
}

///|
fn apply_enum_set_proc(
  proc : EnumSetProc,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc.kind {
    Indexer(set) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      }
      let name = parse_symbol(value_as_datum(args[0]))
      match enum_set_index_of(set.universe, name) {
        Some(idx) =>
          Apply(Datum(Int(idx)), kont, handlers)
        None =>
          Apply(Datum(Bool(false)), kont, handlers)
      }
    }
    Constructor(set) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      }
      let names = parse_symbol_list(
        datum_list_to_array(value_as_datum(args[0])),
      )
      let subset = enum_set_from_names(set.universe, names)
      Apply(EnumSet(subset), kont, handlers)
    }
  }
}

///|
fn make_condition_value(components : Array[Record]) -> Value {
  if components.length() == 1 {
    Record(components[0])
  } else {
    Datum(Condition(make_condition(components)))
  }
}

///|
fn lookup_condition_record_type(name : String) -> RecordType raise EvalError {
  match lookup_record_type_descriptor(name) {
    Some(desc) => desc.record_type
    None => raise @core.EvalError("type error: condition type expected")
  }
}

///|
fn make_condition_record(
  name : String,
  values : Array[Value],
) -> Record raise EvalError {
  let record_type = lookup_condition_record_type(name)
  if record_type.fields.length() != values.length() {
    raise @core.EvalError("type error: condition field count mismatch")
  }
  make_record_instance(record_type, values)
}

///|
fn irritants_list_value(values : Array[Value]) -> Value {
  values_to_list_value(values)
}

///|
fn raise_condition(
  components : Array[Record],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let cond_value = make_condition_value(components)
  apply_proc(Primitive(Raise), [cond_value], kont, handlers)
}

///|
fn hashtable_key_equal(
  equiv : HashtableEquiv,
  left : Value,
  right : Value,
) -> Bool {
  match equiv {
    Eq => value_eqv(left, right)
    Eqv => value_eqv(left, right)
    Equal => value_equal(left, right)
    Proc(_) => false
  }
}

///|
fn hashtable_find_state(
  table : Hashtable,
  key : Value,
  idx : Int,
  op : HashtableOp,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let entries = table.entries.val
  if idx >= entries.length() {
    return hashtable_not_found_state(table, key, op, kont, handlers)
  }
  let entry = entries[idx]
  match table.equiv {
    Proc(proc) =>
      apply_proc(
        proc,
        [key, entry.key],
        HashtableFindResult(table, key, idx, op, kont),
        handlers,
      )
    _ =>
      if hashtable_key_equal(table.equiv, key, entry.key) {
        hashtable_found_state(table, key, idx, op, kont, handlers)
      } else {
        hashtable_find_state(table, key, idx + 1, op, kont, handlers)
      }
  }
}

///|
fn hashtable_found_state(
  table : Hashtable,
  key : Value,
  idx : Int,
  op : HashtableOp,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let entries = table.entries.val
  let entry = entries[idx]
  match op {
    Ref(_) => Apply(entry.value.val, kont, handlers)
    Contains =>
      Apply(bool_value(true), kont, handlers)
    Set(value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      entry.value.val = value
      Apply(Void, kont, handlers)
    }
    Delete => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      let next_entries : Array[HashtableEntry] = []
      for i = 0; i < entries.length(); {
        // invariant : i >= 0 && i <= entries.length()
        // decreases : entries.length() - i
        // assert : i <= entries.length()
        if i != idx {
          next_entries.push(entries[i])
        }
        continue i + 1
      }
      table.entries.val = next_entries
      Apply(Void, kont, handlers)
    }
    Update(proc, _) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      apply_proc(
        proc,
        [entry.value.val],
        HashtableUpdateApply(table, key, idx, true, kont),
        handlers,
      )
    }
  }
}

///|
fn hashtable_not_found_state(
  table : Hashtable,
  key : Value,
  op : HashtableOp,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match op {
    Ref(default_value) =>
      Apply(default_value, kont, handlers)
    Contains =>
      Apply(bool_value(false), kont, handlers)
    Set(value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      let entries = table.entries.val
      entries.push(HashtableEntry::{ key, value: Ref::new(value) })
      Apply(Void, kont, handlers)
    }
    Delete => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      Apply(Void, kont, handlers)
    }
    Update(proc, default_value) => {
      if !table.mutable {
        raise @core.EvalError("hashtable is immutable")
      }
      apply_proc(
        proc,
        [default_value],
        HashtableUpdateApply(table, key, -1, false, kont),
        handlers,
      )
    }
  }
}

///|
fn apply_hashtable_ref(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let default_value = args[2]
  hashtable_find_state(
    table,
    key,
    0,
    Ref(default_value),
    kont,
    handlers,
  )
}

///|
fn apply_hashtable_set(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let value = args[2]
  hashtable_find_state(table, key, 0, Set(value), kont, handlers)
}

///|
fn apply_hashtable_delete(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 2 {
    raise arity_mismatch(2, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  hashtable_find_state(table, key, 0, Delete, kont, handlers)
}

///|
fn apply_hashtable_contains(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 2 {
    raise arity_mismatch(2, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  hashtable_find_state(table, key, 0, Contains, kont, handlers)
}

///|
fn apply_hashtable_update(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() != 4 {
    raise arity_mismatch(4, args.length())
  }
  let table = value_as_hashtable(args[0])
  let key = args[1]
  let proc = args[2]
  let default_value = args[3]
  if !is_procedure_value(proc) {
    raise @core.EvalError("type error: procedure expected")
  }
  hashtable_find_state(
    table,
    key,
    0,
    Update(proc, default_value),
    kont,
    handlers,
  )
}

///|
priv struct LibraryNameSpec {
  base : String
  version : Array[Int]?
}

///|
priv enum VersionRef {
  Exact(Array[Int])
  Gt(Array[Int])
  Ge(Array[Int])
  Lt(Array[Int])
  Le(Array[Int])
  And(Array[VersionRef])
  Or(Array[VersionRef])
  Not(Ref[VersionRef])
}

///|
priv struct LibraryRef {
  base : String
  version_ref : VersionRef?
}

///|
priv struct LibraryVersionInfo {
  name : String
  version : Array[Int]
  has_version : Bool
}

///|
let library_versions_registry : Ref[Map[String, Array[LibraryVersionInfo]]] = Ref::new({},
)

///|
fn reset_library_version_registry() -> Unit {
  library_versions_registry.val = {}
}

///|
fn format_version_suffix(version : Array[Int]) -> String {
  let versions : Array[String] = []
  for part in version {
    // invariant : version.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : version.length() >= 0
    versions.push(part.to_string())
  }
  let joined = versions.join(".")
  "v\{joined}"
}

///|
fn library_full_name(base : String, version : Array[Int]?) -> String {
  match version {
    Some(value) => "\{base}.\{format_version_suffix(value)}"
    None => base
  }
}

///|
fn register_library_entry(
  base : String,
  version : Array[Int]?,
  exports : Map[String, Binding],
) -> Unit {
  let name = library_full_name(base, version)
  register_library(name, exports)
  let entry = LibraryVersionInfo::{
    name,
    version: match version {
      Some(value) => value
      None => []
    },
    has_version: version is Some(_),
  }
  match library_versions_registry.val.get(base) {
    Some(existing) => {
      let updated = existing
      updated.push(entry)
      library_versions_registry.val[base] = updated
    }
    None => library_versions_registry.val[base] = [entry]
  }
}

///|
fn parse_version_items(items : Array[Datum]) -> Array[Int] raise EvalError {
  if items.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  let versions : Array[Int] = []
  for part in items {
    // invariant : items.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : items.length() >= 0
    match part {
      Int(n) =>
        if n < 0 {
          raise @core.EvalError("invalid library name")
        } else {
          versions.push(n)
        }
      _ => raise @core.EvalError("invalid library name")
    }
  }
  versions
}

///|
fn parse_version_list(expr : Datum) -> Array[Int] raise EvalError {
  parse_version_items(datum_list_to_array(expr))
}

///|
fn parse_version_ref_version(
  items : Array[Datum],
) -> Array[Int] raise EvalError {
  if items.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  if items.length() == 1 {
    match items[0] {
      Pair(_, _) | Nil => parse_version_list(items[0])
      _ => parse_version_items(items)
    }
  } else {
    parse_version_items(items)
  }
}

///|
fn parse_version_ref(expr : Datum) -> VersionRef raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  match items[0] {
    Symbol("and") => {
      if items.length() == 1 {
        raise @core.EvalError("invalid library name")
      }
      let refs : Array[VersionRef] = []
      for i = 1; i < items.length(); {
        // invariant : i >= 1 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        refs.push(parse_version_ref(items[i]))
        continue i + 1
      }
      And(refs)
    }
    Symbol("or") => {
      if items.length() == 1 {
        raise @core.EvalError("invalid library name")
      }
      let refs : Array[VersionRef] = []
      for i = 1; i < items.length(); {
        // invariant : i >= 1 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        refs.push(parse_version_ref(items[i]))
        continue i + 1
      }
      Or(refs)
    }
    Symbol("not") => {
      if items.length() != 2 {
        raise @core.EvalError("invalid library name")
      }
      Not(Ref::new(parse_version_ref(items[1])))
    }
    Symbol(">") =>
      Gt(parse_version_ref_version(items.sub(start=1).to_array()))
    Symbol(">=") =>
      Ge(parse_version_ref_version(items.sub(start=1).to_array()))
    Symbol("<") =>
      Lt(parse_version_ref_version(items.sub(start=1).to_array()))
    Symbol("<=") =>
      Le(parse_version_ref_version(items.sub(start=1).to_array()))
    Int(_) => Exact(parse_version_items(items))
    _ => raise @core.EvalError("invalid library name")
  }
}

///|
fn parse_library_name_spec(expr : Datum) -> LibraryNameSpec raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  let names : Array[String] = []
  let version : Array[Int]? =
    for i = 0, version = None; i < parts.length(); {
      // invariant : i >= 0 && i <= parts.length()
      // decreases : parts.length() - i
      // assert : i <= parts.length()
      match parts[i] {
        Symbol(name) => {
          if version is Some(_) {
            raise @core.EvalError("invalid library name")
          }
          names.push(name)
          continue i + 1, version
        }
        Pair(_, _) | Nil => {
          if i != parts.length() - 1 {
            raise @core.EvalError("invalid library name")
          }
          continue i + 1, Some(parse_version_list(parts[i]))
        }
        _ => raise @core.EvalError("invalid library name")
      }
    } else {
      version
    }
  if names.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  { base: names.join("."), version }
}

///|
fn parse_library_ref(expr : Datum) -> LibraryRef raise EvalError {
  let parts = datum_list_to_array(expr)
  if parts.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  let names : Array[String] = []
  let version_ref : VersionRef? =
    for i = 0, version_ref = None; i < parts.length(); {
      // invariant : i >= 0 && i <= parts.length()
      // decreases : parts.length() - i
      // assert : names.length() <= i
      match parts[i] {
        Symbol(name) => {
          if version_ref is Some(_) {
            raise @core.EvalError("invalid library name")
          }
          names.push(name)
          continue i + 1, version_ref
        }
        Pair(_, _) | Nil => {
          if i != parts.length() - 1 {
            raise @core.EvalError("invalid library name")
          }
          continue i + 1, Some(parse_version_ref(parts[i]))
        }
        _ => raise @core.EvalError("invalid library name")
      }
    } else {
      version_ref
    }
  if names.is_empty() {
    raise @core.EvalError("invalid library name")
  }
  { base: names.join("."), version_ref }
}

///|
fn compare_versions(left : Array[Int], right : Array[Int]) -> Int {
  let min_len = if left.length() < right.length() {
    left.length()
  } else {
    right.length()
  }
  let prefix_cmp = for i = 0; i < min_len; {
    // invariant : i >= 0 && i <= min_len
    // decreases : min_len - i
    // assert : i <= min_len
    if left[i] < right[i] {
      break -1
    }
    if left[i] > right[i] {
      break 1
    }
    continue i + 1
  } else {
    0
  }
  if prefix_cmp != 0 {
    prefix_cmp
  } else if left.length() < right.length() {
    -1
  } else if left.length() > right.length() {
    1
  } else {
    0
  }
}

///|
fn version_ref_matches(version : Array[Int], reference : VersionRef) -> Bool {
  match reference {
    Exact(expected) => compare_versions(version, expected) == 0
    Gt(expected) => compare_versions(version, expected) > 0
    Ge(expected) => compare_versions(version, expected) >= 0
    Lt(expected) => compare_versions(version, expected) < 0
    Le(expected) => compare_versions(version, expected) <= 0
    And(items) => {
      for i = 0; i < items.length(); {
        // invariant : i >= 0 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        if !version_ref_matches(version, items[i]) {
          break false
        }
        continue i + 1
      } else {
        true
      }
    }
    Or(items) => {
      for i = 0; i < items.length(); {
        // invariant : i >= 0 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        if version_ref_matches(version, items[i]) {
          break true
        }
        continue i + 1
      } else {
        false
      }
    }
    Not(item) => !version_ref_matches(version, item.val)
  }
}

///|
fn select_library_for_ref(reference : LibraryRef) -> LibraryVersionInfo? {
  match library_versions_registry.val.get(reference.base) {
    None => None
    Some(available) => {
      let version_ref = reference.version_ref
      let best = for i = 0, best = (None : LibraryVersionInfo?);
        i < available.length(); {
        // invariant : i >= 0 && i <= available.length()
        // decreases : available.length() - i
        // assert : i <= available.length()
        let entry = available[i]
        match version_ref {
          Some(ref_value) => {
            if !entry.has_version {
              continue i + 1, best
            }
            if !version_ref_matches(entry.version, ref_value) {
              continue i + 1, best
            }
          }
          None => ()
        }
        let next_best = match best {
          None => Some(entry)
          Some(current) =>
            if version_ref is None {
              if entry.has_version && !current.has_version {
                Some(entry)
              } else if entry.has_version && current.has_version {
                if compare_versions(entry.version, current.version) > 0 {
                  Some(entry)
                } else {
                  Some(current)
                }
              } else {
                Some(current)
              }
            } else if compare_versions(entry.version, current.version) > 0 {
              Some(entry)
            } else {
              Some(current)
            }
        }
        continue i + 1, next_best
      } else {
        best
      }
      best
    }
  }
}

///|
fn library_exports_from_ref(
  reference : LibraryRef,
) -> Map[String, Binding] raise EvalError {
  match select_library_for_ref(reference) {
    Some(entry) => library_exports(entry.name)
    None => raise @core.EvalError("unknown library: \{reference.base}")
  }
}

///|
fn parse_export_clause(expr : Datum) -> Array[(String, String)] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid library")
  }
  match items[0] {
    Symbol("export") => {
      let exports : Array[(String, String)] = []
      for i = 1; i < items.length(); {
        // invariant : i >= 1 && i <= items.length()
        // decreases : items.length() - i
        // assert : i <= items.length()
        match items[i] {
          Symbol(name) => exports.push((name, name))
          Pair(_, _) => {
            let spec_items = datum_list_to_array(items[i])
            if spec_items.is_empty() {
              raise @core.EvalError("invalid library")
            }
            match spec_items[0] {
              Symbol("rename") => {
                for j = 1; j < spec_items.length(); {
                  // invariant : j >= 1 && j <= spec_items.length()
                  // decreases : spec_items.length() - j
                  // assert : j <= spec_items.length()
                  let rename_parts = datum_list_to_array(spec_items[j])
                  if rename_parts.length() != 2 {
                    raise @core.EvalError("invalid library")
                  }
                  let internal = parse_symbol(rename_parts[0])
                  let external = parse_symbol(rename_parts[1])
                  exports.push((external, internal))
                  continue j + 1
                }
              }
              _ => raise @core.EvalError("invalid library")
            }
          }
          _ => raise @core.EvalError("invalid library")
        }
        continue i + 1
      }
      exports
    }
    _ => raise @core.EvalError("invalid library")
  }
}

///|
fn base_syntax_keywords() -> Array[String] {
  [
    "...", "_", "=>", "and", "begin", "case", "cond", "define", "define-syntax",
    "else", "identifier-syntax", "if", "lambda", "let", "let*", "let*-values", "let-syntax",
    "let-values", "letrec", "letrec*", "letrec-syntax", "or", "quasiquote", "quote",
    "set!", "syntax-rules", "unquote", "unquote-splicing",
  ]
}

///|
fn syntax_case_keywords() -> Array[String] {
  [
    "...", "_", "with-syntax", "syntax", "quasisyntax", "unsyntax", "unsyntax-splicing",
    "syntax-case",
  ]
}

///|
fn control_syntax_keywords() -> Array[String] {
  ["case-lambda", "do"]
}

///|
fn exceptions_syntax_keywords() -> Array[String] {
  ["guard"]
}

///|
fn enums_syntax_keywords() -> Array[String] {
  ["define-enumeration"]
}

///|
fn records_syntax_keywords() -> Array[String] {
  [
    "define-record-type", "fields", "immutable", "mutable", "nongenerative", "opaque",
    "parent", "parent-rtd", "protocol", "record-constructor-descriptor", "record-type-descriptor",
    "sealed",
  ]
}

///|
fn conditions_syntax_keywords() -> Array[String] {
  ["define-condition-type"]
}

///|
fn is_syntax_keyword(name : String) -> Bool {
  let lists : Array[Array[String]] = [
    base_syntax_keywords(),
    syntax_case_keywords(),
    control_syntax_keywords(),
    exceptions_syntax_keywords(),
    enums_syntax_keywords(),
    records_syntax_keywords(),
    conditions_syntax_keywords(),
  ]
  for keywords in lists {
    // invariant : lists.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : lists.length() >= 0
    for kw in keywords {
      // invariant : keywords.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : keywords.length() >= 0
      if kw == name {
        return true
      }
    }
  }
  false
}

///|
fn parse_import_clause(expr : Datum) -> Array[Datum] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid library")
  }
  match items[0] {
    Symbol("import") => items.sub(start=1).to_array()
    _ => raise @core.EvalError("invalid library")
  }
}

///|
fn library_exports(name : String) -> Map[String, Binding] raise EvalError {
  match lookup_library(name) {
    Some(exports_map) => {
      let exports : Map[String, Binding] = {}
      for key in exports_map.keys() {
        // invariant : exports_map.keys().length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : exports_map.keys().length() >= 0
        match exports_map.get(key) {
          Some(binding) => exports[key] = binding
          None => ()
        }
      }
      exports
    }
    None => raise @core.EvalError("unknown library: \{name}")
  }
}

///|
fn import_set_only(
  exports : Map[String, Binding],
  names : Array[String],
) -> Map[String, Binding] raise EvalError {
  let result : Map[String, Binding] = {}
  for name in names {
    // invariant : names.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : names.length() >= 0
    match exports.get(name) {
      Some(binding) => result[name] = binding
      None => raise @core.EvalError("unknown import: \{name}")
    }
  }
  result
}

///|
fn import_set_except(
  exports : Map[String, Binding],
  names : Array[String],
) -> Map[String, Binding] raise EvalError {
  let excluded : Map[String, Bool] = {}
  for name in names {
    // invariant : names.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : names.length() >= 0
    if !exports.contains(name) {
      raise @core.EvalError("unknown import: \{name}")
    }
    excluded[name] = true
  }
  let result : Map[String, Binding] = {}
  for key in exports.keys() {
    // invariant : exports.keys().length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : exports.keys().length() >= 0
    if !excluded.contains(key) {
      match exports.get(key) {
        Some(binding) => result[key] = binding
        None => ()
      }
    }
  }
  result
}

///|
fn import_set_rename(
  exports : Map[String, Binding],
  renames : Array[(String, String)],
) -> Map[String, Binding] raise EvalError {
  let rename_map : Map[String, String] = {}
  for rename in renames {
    // invariant : renames.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : renames.length() >= 0
    let (old_name, new_name) = rename
    if !exports.contains(old_name) {
      raise @core.EvalError("unknown import: \{old_name}")
    }
    if rename_map.contains(old_name) {
      raise @core.EvalError("invalid import set")
    }
    rename_map[old_name] = new_name
  }
  let result : Map[String, Binding] = {}
  for key in exports.keys() {
    // invariant : exports.keys().length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : exports.keys().length() >= 0
    let target = match rename_map.get(key) {
      Some(name) => name
      None => key
    }
    if result.contains(target) {
      raise @core.EvalError("invalid import set")
    }
    match exports.get(key) {
      Some(binding) => result[target] = binding
      None => ()
    }
  }
  result
}

///|
fn import_set_prefix(
  exports : Map[String, Binding],
  prefix : String,
) -> Map[String, Binding] {
  let result : Map[String, Binding] = {}
  for key in exports.keys() {
    // invariant : exports.keys().length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : exports.keys().length() >= 0
    match exports.get(key) {
      Some(binding) => result["\{prefix}\{key}"] = binding
      None => ()
    }
  }
  result
}

///|
fn parse_rename_specs(
  items : Array[Datum],
) -> Array[(String, String)] raise EvalError {
  let renames : Array[(String, String)] = []
  for item in items {
    // invariant : items.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : items.length() >= 0
    let parts = datum_list_to_array(item)
    if parts.length() != 2 {
      raise @core.EvalError("invalid import set")
    }
    let old_name = parse_symbol(parts[0])
    let new_name = parse_symbol(parts[1])
    renames.push((old_name, new_name))
  }
  renames
}

///|
priv struct ImportPhases {
  run : Bool
  expand : Bool
}

///|
fn parse_for_phases(items : Array[Datum]) -> ImportPhases raise EvalError {
  if items.is_empty() {
    raise @core.EvalError("invalid import set")
  }
  let (run, expand) = for i = 0, run = false, expand = false;
    i < items.length(); {
    // invariant : i >= 0 && i <= items.length()
    // decreases : items.length() - i
    // assert : i <= items.length()
    let item = items[i]
    match item {
      Symbol("run") => continue i + 1, true, expand
      Symbol("expand") => continue i + 1, run, true
      Pair(_, _) | Nil => {
        let parts = datum_list_to_array(item)
        if parts.length() != 2 {
          raise @core.EvalError("invalid import set")
        }
        match parts[0] {
          Symbol("meta") =>
            match parts[1] {
              Int(n) =>
                if n == 0 {
                  continue i + 1, true, expand
                } else if n > 0 {
                  continue i + 1, run, true
                } else {
                  raise @core.EvalError("invalid import set")
                }
              _ => raise @core.EvalError("invalid import set")
            }
          _ => raise @core.EvalError("invalid import set")
        }
      }
      _ => raise @core.EvalError("invalid import set")
    }
  } else {
    (run, expand)
  }
  if !run && !expand {
    raise @core.EvalError("invalid import set")
  }
  { run, expand }
}

///|
fn filter_exports_by_phase(
  exports : Map[String, Binding],
  phases : ImportPhases,
) -> Map[String, Binding] {
  if phases.run && phases.expand {
    return exports
  }
  let result : Map[String, Binding] = {}
  for key in exports.keys() {
    // invariant : true
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : true
    match exports.get(key) {
      Some(binding) =>
        match binding.value {
          Macro(_) => if phases.expand { result[key] = exports[key] }
          SyntaxKeyword(_) =>
            if phases.expand {
              result[key] = exports[key]
            }
          _ => if phases.run { result[key] = exports[key] }
        }
      None => ()
    }
  }
  result
}

///|
fn import_set_exports(expr : Datum) -> Map[String, Binding] raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    raise @core.EvalError("invalid import set")
  }
  match items[0] {
    Symbol("only") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let names = parse_symbol_list(items.sub(start=2).to_array())
      import_set_only(base, names)
    }
    Symbol("except") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let names = parse_symbol_list(items.sub(start=2).to_array())
      import_set_except(base, names)
    }
    Symbol("rename") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let renames = parse_rename_specs(items.sub(start=2).to_array())
      import_set_rename(base, renames)
    }
    Symbol("prefix") => {
      if items.length() != 3 {
        raise @core.EvalError("invalid import set")
      }
      let base = import_set_exports(items[1])
      let prefix = parse_symbol(items[2])
      import_set_prefix(base, prefix)
    }
    Symbol("for") => {
      if items.length() < 3 {
        raise @core.EvalError("invalid import set")
      }
      let phases = parse_for_phases(items.sub(start=2).to_array())
      let base = import_set_exports(items[1])
      filter_exports_by_phase(base, phases)
    }
    _ => {
      let reference = parse_library_ref(expr)
      library_exports_from_ref(reference)
    }
  }
}

///|
fn import_set_into_env(expr : Datum, env : Env) -> Unit raise EvalError {
  let exports = import_set_exports(expr)
  for key in exports.keys() {
    // invariant : exports.keys().length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : exports.keys().length() >= 0
    match exports.get(key) {
      Some(binding) => env_define_binding(env, key, binding)
      None => ()
    }
  }
}

///|
fn eval_library_state(
  args : Array[Datum],
  _env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 3 {
    raise @core.EvalError("invalid library")
  }
  let name_spec = parse_library_name_spec(args[0])
  let exports = parse_export_clause(args[1])
  let imports = parse_import_clause(args[2])
  let body = args.sub(start=3).to_array()
  let lib_env = init_env()
  for import_set in imports {
    // invariant : imports.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : imports.length() >= 0
    import_set_into_env(import_set, lib_env)
  }
  for form in body {
    // invariant : body.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : body.length() >= 0
    let _ = eval_expr(form, lib_env)

  }
  let export_map : Map[String, Binding] = {}
  for export_spec in exports {
    // invariant : exports.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : exports.length() >= 0
    let (external, internal) = export_spec
    if is_syntax_keyword(internal) {
      export_map[external] = Binding::{
        id: next_binding_id(),
        value: SyntaxKeyword(external),
      }
    } else {
      export_map[external] = env_lookup_binding(lib_env, internal)
    }
  }
  register_library_entry(name_spec.base, name_spec.version, export_map)
  Apply(Void, kont, handlers)
}

///|
fn eval_import_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  for spec in args {
    // invariant : args.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : args.length() >= 0
    import_set_into_env(spec, env)
  }
  Apply(Void, kont, handlers)
}

///|
fn eval_and_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if args.is_empty() {
    Apply(bool_value(true), kont, handlers)
  } else {
    let rest = args.sub(start=1).to_array()
    Eval(args[0], env, And(rest, env, kont), handlers)
  }
}

///|
fn eval_or_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState {
  if args.is_empty() {
    Apply(bool_value(false), kont, handlers)
  } else {
    let rest = args.sub(start=1).to_array()
    Eval(args[0], env, Or(rest, env, kont), handlers)
  }
}

///|
fn eval_case_state(
  args : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  }
  let key_expr = args[0]
  let clauses = args.sub(start=1).to_array()
  Eval(key_expr, env, Case(clauses, env, kont), handlers)
}

///|
fn case_dispatch(
  key : Value,
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i <= clauses.length()
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      continue i + 1
    } else {
      match symbol_name(parts[0]) {
        Some("else") => {
          let body = parts.sub(start=1).to_array()
          return eval_sequence_state(body, env, kont, handlers)
        }
        _ => ()
      }
      let datums = datum_list_to_array(parts[0])
      let matched = for j = 0; j < datums.length(); {
        // invariant : j >= 0 && j <= datums.length()
        // decreases : datums.length() - j
        // assert : j <= datums.length()
        if value_eqv(key, Datum(datums[j])) {
          break true
        }
        continue j + 1
      } else {
        false
      }
      if matched {
        if parts.length() >= 2 && symbol_name(parts[1]) is Some("=>") {
          if parts.length() != 3 {
            raise @core.EvalError("invalid case")
          }
          return Eval(
            parts[2],
            env,
            CaseArrow(key, kont),
            handlers,
          )
        }
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      }
      continue i + 1
    }
  }
  Apply(Void, kont, handlers)
}

///|
fn cond_start(
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i <= clauses.length()
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      continue i + 1
    } else {
      let head = symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return Eval(
          parts[0],
          env,
          Cond(parts, rest, env, kont),
          handlers,
        )
      }
    }
  }
  Apply(Void, kont, handlers)
}

///|
fn feature_enabled(name : String) -> Bool {
  match name {
    "r6rs" => true
    "ratios" => true
    "ieee-float" => true
    "full-unicode" => true
    "exact-closed" => false
    "exact-complex" => false
    _ => false
  }
}

///|
fn cond_expand_feature(expr : Datum) -> Bool raise EvalError {
  match expr {
    Symbol(name) => feature_enabled(name)
    Pair(_, _) => {
      let items = datum_list_to_array(expr)
      if items.is_empty() {
        raise @core.EvalError("invalid cond-expand")
      }
      match items[0] {
        Symbol("and") =>
          if items.length() == 1 {
            true
          } else {
            for i = 1; i < items.length(); {
              // invariant : i >= 1 && i <= items.length()
              // decreases : items.length() - i
              // assert : i <= items.length()
              if !cond_expand_feature(items[i]) {
                break false
              }
              continue i + 1
            } else {
              true
            }
          }
        Symbol("or") => {
          for i = 1; i < items.length(); {
            // invariant : i >= 1 && i <= items.length()
            // decreases : items.length() - i
            // assert : i <= items.length()
            if cond_expand_feature(items[i]) {
              break true
            }
            continue i + 1
          } else {
            false
          }
        }
        Symbol("not") => {
          if items.length() != 2 {
            raise @core.EvalError("invalid cond-expand")
          }
          !cond_expand_feature(items[1])
        }
        Symbol("library") => {
          if items.length() != 2 {
            raise @core.EvalError("invalid cond-expand")
          }
          let reference = parse_library_ref(items[1])
          select_library_for_ref(reference) is Some(_)
        }
        _ => raise @core.EvalError("invalid cond-expand")
      }
    }
    _ => raise @core.EvalError("invalid cond-expand")
  }
}

///|
fn eval_cond_expand_state(
  clauses : Array[Datum],
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  for i = 0; i < clauses.length(); {
    // invariant : i >= 0 && i <= clauses.length()
    // decreases : clauses.length() - i
    // assert : i <= clauses.length()
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      raise @core.EvalError("invalid cond-expand")
    }
    match parts[0] {
      Symbol("else") => {
        if i != clauses.length() - 1 {
          raise @core.EvalError("invalid cond-expand")
        }
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont, handlers)
      }
      _ =>
        if cond_expand_feature(parts[0]) {
          let body = parts.sub(start=1).to_array()
          return eval_sequence_state(body, env, kont, handlers)
        }
    }
    continue i + 1
  }
  Apply(Void, kont, handlers)
}

///|
fn eval_pair(
  expr : Datum,
  env : Env,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  let expr_unwrapped = datum_unwrap_syntax(expr)
  let items = datum_list_to_array(expr_unwrapped)
  if items.is_empty() {
    Apply(Datum(Nil), kont, handlers)
  } else {
    let head = items[0]
    let args = items.sub(start=1).to_array()
    match symbol_name(datum_unwrap_syntax(head)) {
      Some("define-syntax") =>
        Apply(eval_define_syntax(args, env), kont, handlers)
      Some("library") => eval_library_state(args, env, kont, handlers)
      Some("import") => eval_import_state(args, env, kont, handlers)
      Some("include") => eval_include_state(args, env, kont, handlers, false)
      Some("include-ci") => eval_include_state(args, env, kont, handlers, true)
      Some("include-library-declarations") =>
        eval_include_state(args, env, kont, handlers, false)
      Some("define-record-type") =>
        eval_define_record_type_state(args, env, kont, handlers)
      Some("define-condition-type") =>
        eval_define_condition_type_state(args, env, kont, handlers)
      Some("define-enumeration") =>
        eval_define_enumeration_state(args, env, kont, handlers)
      Some("quote") => Apply(eval_quote(args), kont, handlers)
      Some("quasiquote") =>
        Apply(eval_quasiquote(args, env), kont, handlers)
      Some("syntax") =>
        Apply(eval_syntax(args, env), kont, handlers)
      Some("quasisyntax") =>
        Apply(eval_quasisyntax(args, env), kont, handlers)
      Some("syntax-case") => eval_syntax_case_state(args, env, kont, handlers)
      Some("delay") =>
        Apply(eval_delay(args, env), kont, handlers)
      Some("if") => eval_if_state(args, env, kont, handlers)
      Some("begin") => eval_sequence_state(args, env, kont, handlers)
      Some("begin-for-syntax") =>
        eval_begin_for_syntax_state(args, env, kont, handlers)
      Some("lambda") =>
        Apply(eval_lambda(args, env), kont, handlers)
      Some("case-lambda") =>
        Apply(eval_case_lambda(args, env), kont, handlers)
      Some("define") => eval_define_state(args, env, kont, handlers)
      Some("define-values") =>
        eval_define_values_state(args, env, kont, handlers)
      Some("set!") => eval_set_state(args, env, kont, handlers)
      Some("let") => eval_let_state(args, env, kont, handlers)
      Some("let-values") => eval_let_values_state(args, env, kont, handlers)
      Some("let*-values") =>
        eval_let_star_values_state(args, env, kont, handlers)
      Some("let*") => eval_let_star_state(args, env, kont, handlers)
      Some("letrec") => eval_letrec_state(args, env, kont, handlers)
      Some("letrec*") => eval_letrec_star_state(args, env, kont, handlers)
      Some("parameterize") => eval_parameterize_state(args, env, kont, handlers)
      Some("let-syntax") => eval_let_syntax_state(args, env, kont, handlers)
      Some("letrec-syntax") =>
        eval_letrec_syntax_state(args, env, kont, handlers)
      Some("and") => eval_and_state(args, env, kont, handlers)
      Some("or") => eval_or_state(args, env, kont, handlers)
      Some("cond") => cond_start(args, env, kont, handlers)
      Some("cond-expand") => eval_cond_expand_state(args, env, kont, handlers)
      Some("guard") => eval_guard_state(args, env, kont, handlers)
      Some("do") => eval_do_state(args, env, kont, handlers)
      Some("case") => eval_case_state(args, env, kont, handlers)
      Some(name) =>
        match env_get_macro(env, name) {
          Some(transformer) => {
            let expansion = expand_macro(transformer, expr_unwrapped, env)
            let next_env = env_with_captures(env, expansion.captures)
            Eval(expansion.datum, next_env, kont, handlers)
          }
          None =>
            Eval(
              head,
              env,
              AppFun(args, env, kont),
              handlers,
            )
        }
      None =>
        Eval(head, env, AppFun(args, env, kont), handlers)
    }
  }
}

///|
fn map_step(
  proc : Value,
  lists : Array[Datum],
  results : Array[Datum],
  for_each : Bool,
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if lists.is_empty() {
    raise @core.EvalError("arity mismatch: expected at least 1 list")
  }
  let args : Array[Value] = []
  let next_lists : Array[Datum] = []
  for list in lists {
    // invariant : lists.length() >= 0
    // TODO(decreases) : loop index not exposed; possible bug
    // assert : lists.length() >= 0
    match list {
      Nil => {
        if for_each {
          return Apply(Void, kont, handlers)
        }
        return Apply(
          Datum(list_from_array(results)),
          kont,
          handlers,
        )
      }
      Pair(car, cdr) => {
        args.push(value_from_datum(car.val))
        next_lists.push(cdr.val)
      }
      _ => raise @core.EvalError("type error: proper list expected")
    }
  }
  apply_proc(
    proc,
    args,
    MapStep(proc, next_lists, results, for_each, kont),
    handlers,
  )
}

///|
fn string_to_char_list(s : String) -> Datum raise EvalError {
  let items : Array[Datum] = []
  for i = 0; i < s.length(); {
    // invariant : i >= 0 && i <= s.length()
    // decreases : s.length() - i
    // assert : i <= s.length()
    match s.get_char(i) {
      Some(ch) => items.push(Char(ch))
      None => raise @core.EvalError("index out of range")
    }
    continue i + 1
  }
  list_from_array(items)
}

///|
fn apply_proc(
  proc : Value,
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  match proc {
    Primitive(Apply) => apply_apply(args, kont, handlers)
    Primitive(CallWithValues) =>
      match args {
        [producer, consumer] =>
          apply_proc(
            producer,
            [],
            CallWithValues(consumer, kont),
            handlers,
          )
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(RecordConstructor) =>
      match args {
        [desc_value] => {
          let desc = value_as_record_constructor_descriptor(desc_value)
          record_constructor_from_descriptor(desc, kont, handlers)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(HashtableRef) =>
      apply_hashtable_ref(args, kont, handlers)
    Primitive(HashtableSet) =>
      apply_hashtable_set(args, kont, handlers)
    Primitive(HashtableDelete) =>
      apply_hashtable_delete(args, kont, handlers)
    Primitive(HashtableContainsP) =>
      apply_hashtable_contains(args, kont, handlers)
    Primitive(HashtableUpdate) =>
      apply_hashtable_update(args, kont, handlers)
    Primitive(MakeParameter) =>
      match args {
        [value] =>
          Apply(
            Parameter(make_parameter(value)),
            kont,
            handlers,
          )
        [value, converter] =>
          apply_proc(
            converter,
            [value],
            MakeParameter(converter, kont),
            handlers,
          )
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Primitive(DynamicWind) =>
      match args {
        [before, thunk, after] => {
          let winder = make_proc_winder(before, after)
          apply_winder_before(
            winder,
            WindEnter(thunk, winder, kont),
            handlers,
          )
        }
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive(Eval) =>
      match args {
        [expr_value, env_value] => {
          let expr = value_as_datum(expr_value)
          let eval_env = value_as_eval_env(env_value)
          Eval(expr, eval_env.env, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(Environment) => {
      match args {
        [] => raise arity_mismatch(1, 0)
        _ => {
          let env = env_new()
          for arg in args {
            // invariant : args.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : args.length() >= 0
            let spec = value_as_datum(arg)
            import_set_into_env(spec, env)
          }
          Apply(EvalEnv(make_eval_env(env)), kont, handlers)
        }
      }
    }
    Primitive(MakePromise) =>
      match args {
        [thunk] => {
          if !is_procedure_value(thunk) {
            raise @core.EvalError("type error: procedure expected")
          }
          Apply(Promise(make_promise(thunk)), kont, handlers)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(Force) =>
      match args {
        [promise_value] =>
          match promise_value {
            Promise(promise) =>
              match promise.state.val {
                Value(result) =>
                  Apply(result, kont, handlers)
                Thunk(thunk) =>
                  apply_proc(
                    thunk,
                    [],
                    ForcePromise(promise, kont),
                    handlers,
                  )
              }
            _ => raise @core.EvalError("type error: promise expected")
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(WithExceptionHandler) =>
      match args {
        [handler, thunk] => {
          let next_handlers : Array[Value] = []
          for item in handlers {
            // invariant : handlers.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : handlers.length() >= 0
            next_handlers.push(item)
          }
          next_handlers.push(handler)
          let restore = Kont::RestoreHandlers(handlers, kont)
          apply_proc(thunk, [], restore, next_handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(Error) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&error", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(AssertionViolation) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&assertion", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(ImplementationRestrictionViolation) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(
          make_condition_record("&implementation-restriction", []),
        )
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(UndefinedViolation) =>
      if args.length() < 2 {
        raise arity_mismatch(2, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let irritants = args.sub(start=2).to_array()
        let components : Array[Record] = []
        components.push(make_condition_record("&undefined", []))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(SyntaxViolation) =>
      if args.length() < 3 {
        raise arity_mismatch(3, args.length())
      } else {
        let who = args[0]
        let message = args[1]
        let form = args[2]
        let subform = if args.length() >= 4 {
          args[3]
        } else {
          Datum(Bool(false))
        }
        let irritants = if args.length() >= 4 {
          args.sub(start=4).to_array()
        } else {
          []
        }
        let components : Array[Record] = []
        components.push(make_condition_record("&syntax", [form, subform]))
        components.push(make_condition_record("&message", [message]))
        components.push(make_condition_record("&who", [who]))
        components.push(
          make_condition_record("&irritants", [irritants_list_value(irritants)]),
        )
        raise_condition(components, kont, handlers)
      }
    Primitive(Raise) =>
      match args {
        [condition] =>
          if handlers.is_empty() {
            raise @core.EvalError("uncaught exception")
          } else {
            let handler = handlers[handlers.length() - 1]
            match handler {
              GuardHandler(guard_handler) =>
                handle_guard(guard_handler, condition, kont, false)
              _ =>
                apply_proc(handler, [condition], RaiseNonCont, handlers)
            }
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(RaiseContinuable) =>
      match args {
        [condition] =>
          if handlers.is_empty() {
            raise @core.EvalError("uncaught exception")
          } else {
            let handler = handlers[handlers.length() - 1]
            match handler {
              GuardHandler(guard_handler) =>
                handle_guard(guard_handler, condition, kont, true)
              _ => apply_proc(handler, [condition], RaiseCont(kont), handlers)
            }
          }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(CallCC) =>
      match args {
        [proc_value] => {
          let cont = capture_continuation(kont, handlers)
          apply_proc(proc_value, [Continuation(cont)], kont, handlers)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive(Map) =>
      match args {
        [proc_value, first_list, ..rest] => {
          let lists : Array[Datum] = []
          lists.push(value_as_datum(first_list))
          for item in rest {
            // invariant : rest.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : rest.length() >= 0
            lists.push(value_as_datum(item))
          }
          map_step(proc_value, lists, [], false, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(ForEach) =>
      match args {
        [proc_value, first_list, ..rest] => {
          let lists : Array[Datum] = []
          lists.push(value_as_datum(first_list))
          for item in rest {
            // invariant : rest.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : rest.length() >= 0
            lists.push(value_as_datum(item))
          }
          map_step(proc_value, lists, [], true, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(VectorMap) =>
      match args {
        [proc_value, first_vector, ..rest] => {
          let vectors : Array[Array[Datum]] = []
          vectors.push(value_as_vector(first_vector))
          for item in rest {
            // invariant : rest.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : rest.length() >= 0
            vectors.push(value_as_vector(item))
          }
          let len = vectors[0].length()
          for j = 0; j < vectors.length(); {
            // invariant : j >= 0 && j <= vectors.length()
            // decreases : vectors.length() - j
            // assert : j <= vectors.length()
            if vectors[j].length() != len {
              raise @core.EvalError("vector length mismatch")
            }
            continue j + 1
          }
          let lists : Array[Datum] = []
          for k = 0; k < vectors.length(); {
            // invariant : k >= 0 && k <= vectors.length()
            // decreases : vectors.length() - k
            // assert : k <= vectors.length()
            lists.push(list_from_array(vectors[k]))
            continue k + 1
          }
          map_step(
            proc_value,
            lists,
            [],
            false,
            VectorMapFinalize(kont),
            handlers,
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(VectorForEach) =>
      match args {
        [proc_value, first_vector, ..rest] => {
          let vectors : Array[Array[Datum]] = []
          vectors.push(value_as_vector(first_vector))
          for item in rest {
            // invariant : rest.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : rest.length() >= 0
            vectors.push(value_as_vector(item))
          }
          let len = vectors[0].length()
          for j = 0; j < vectors.length(); {
            // invariant : j >= 0 && j <= vectors.length()
            // decreases : vectors.length() - j
            // assert : j <= vectors.length()
            if vectors[j].length() != len {
              raise @core.EvalError("vector length mismatch")
            }
            continue j + 1
          }
          let lists : Array[Datum] = []
          for k = 0; k < vectors.length(); {
            // invariant : k >= 0 && k <= vectors.length()
            // decreases : vectors.length() - k
            // assert : k <= vectors.length()
            lists.push(list_from_array(vectors[k]))
            continue k + 1
          }
          map_step(proc_value, lists, [], true, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(StringMap) =>
      match args {
        [proc_value, first_value, ..rest] => {
          let first = value_as_string(first_value)
          let len = first.length()
          let lists : Array[Datum] = []
          lists.push(string_to_char_list(first))
          for item in rest {
            // invariant : rest.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : rest.length() >= 0
            let s = value_as_string(item)
            if s.length() != len {
              raise @core.EvalError("string length mismatch")
            }
            lists.push(string_to_char_list(s))
          }
          map_step(
            proc_value,
            lists,
            [],
            false,
            StringMapFinalize(kont),
            handlers,
          )
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(StringForEach) =>
      match args {
        [proc_value, first_value, ..rest] => {
          let first = value_as_string(first_value)
          let len = first.length()
          let lists : Array[Datum] = []
          lists.push(string_to_char_list(first))
          for item in rest {
            // invariant : rest.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : rest.length() >= 0
            let s = value_as_string(item)
            if s.length() != len {
              raise @core.EvalError("string length mismatch")
            }
            lists.push(string_to_char_list(s))
          }
          map_step(proc_value, lists, [], true, kont, handlers)
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive(prim) =>
      Apply(apply_primitive(prim, args), kont, handlers)
    Closure(cl) => {
      let formals = Formals::{ params: cl.params, rest: cl.rest }
      let bindings = formals_bindings(formals, args)
      let new_env = env_extend(cl.env)
      for binding in bindings {
        // invariant : bindings.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : bindings.length() >= 0
        let (name, bound_value) = binding
        env_define(new_env, name, bound_value)
      }
      eval_sequence_state(cl.body, new_env, kont, handlers)
    }
    CaseClosure(cl) => {
      let matched =
        for i = 0; i < cl.clauses.length(); {
          // invariant : i >= 0 && i <= cl.clauses.length()
          // decreases : cl.clauses.length() - i
          // assert : i <= cl.clauses.length()
          let clause = cl.clauses[i]
          let clause_formals = Formals::{
            params: clause.params,
            rest: clause.rest,
          }
          if formals_accept_len(clause_formals, args.length()) {
            break Some((clause_formals, clause.body))
          }
          continue i + 1
        } else {
          None
        }
      match matched {
        None => {
          if cl.clauses.length() == 1 {
            let clause = cl.clauses[0]
            match clause.rest {
              None =>
                raise arity_mismatch(clause.params.length(), args.length())
              Some(_) =>
                raise arity_mismatch_at_least(
                  clause.params.length(),
                  args.length(),
                )
            }
          }
          let expected_parts : Array[String] = []
          for clause in cl.clauses {
            // invariant : cl.clauses.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : cl.clauses.length() >= 0
            let clause_formals = Formals::{
              params: clause.params,
              rest: clause.rest,
            }
            expected_parts.push(formals_expected_desc(clause_formals))
          }
          let expected = expected_parts.join(", ")
          raise @core.EvalError(
            "arity mismatch: expected one of \{expected} got \{args.length()}",
          )
        }
        Some((matched_formals, matched_body)) => {
          let new_env = env_extend(cl.env)
          let bindings = formals_bindings(matched_formals, args)
          for binding in bindings {
            // invariant : bindings.length() >= 0
            // TODO(decreases) : loop index not exposed; possible bug
            // assert : bindings.length() >= 0
            let (name, bound_value) = binding
            env_define(new_env, name, bound_value)
          }
          eval_sequence_state(matched_body, new_env, kont, handlers)
        }
      }
    }
    Parameter(param) =>
      match args {
        [] => Apply(parameter_get(param), kont, handlers)
        [value] =>
          match param.converter {
            Some(converter) =>
              apply_proc(
                converter,
                [value],
                SetParameter(param, kont),
                handlers,
              )
            None => {
              parameter_set(param, value)
              Apply(Void, kont, handlers)
            }
          }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 0 or 1 got \{args.length()}",
          )
      }
    Continuation(cont) => {
      let resume_value = if args.length() == 1 {
        args[0]
      } else {
        Values(args)
      }
      let current_winds = wind_stack.val
      let target_winds = cont.winds
      let actions : Array[WindAction] = []
      let prefix = common_wind_prefix(current_winds, target_winds)
      for i = current_winds.length(); i > prefix; {
        // invariant : i >= prefix && i <= current_winds.length()
        // decreases : i - prefix
        // assert : i <= current_winds.length()
        let idx = i - 1
        actions.push(After(current_winds[idx]))
        continue i - 1
      }
      actions.push(SwitchHandlers(cont.handlers))
      for i = prefix; i < target_winds.length(); {
        // invariant : i >= prefix && i <= target_winds.length()
        // decreases : target_winds.length() - i
        // assert : i <= target_winds.length()
        actions.push(Before(target_winds[i]))
        continue i + 1
      }
      run_wind_actions(
        actions,
        resume_value,
        cont.kont,
        cont.handlers,
        handlers,
      )
    }
    RecordProc(proc) => apply_record_proc(proc, args, kont, handlers)
    ConditionProc(proc) =>
      apply_condition_proc(proc, args, kont, handlers)
    EnumSetProc(proc) => apply_enum_set_proc(proc, args, kont, handlers)
    _ => raise @core.EvalError("type error: procedure expected")
  }
}

///|
fn apply_apply(
  args : Array[Value],
  kont : Kont,
  handlers : Array[Value],
) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let proc = args[0]
    let flat : Array[Value] = []
    for i = 1; i < args.length() - 1; {
      // invariant : i >= 1 && i <= args.length() - 1
      // decreases : args.length() - 1 - i
      // assert : i <= args.length() - 1
      flat.push(args[i])
      continue i + 1
    }
    let list_value = value_as_datum(args[args.length() - 1])
    let tail = datum_list_to_value_array(list_value)
    flat.append(tail[:])
    apply_proc(proc, flat, kont, handlers)
  }
}
