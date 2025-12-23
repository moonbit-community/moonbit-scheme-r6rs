///|
fn init_env() -> Env {
  let env = env_new()
  set_current_output_port(new_output_string_port())
  env_define(env, "+", Value::Primitive(Primitive::Add))
  env_define(env, "-", Value::Primitive(Primitive::Sub))
  env_define(env, "*", Value::Primitive(Primitive::Mul))
  env_define(env, "/", Value::Primitive(Primitive::Div))
  env_define(env, "=", Value::Primitive(Primitive::NumEq))
  env_define(env, "<", Value::Primitive(Primitive::Less))
  env_define(env, ">", Value::Primitive(Primitive::Greater))
  env_define(env, "<=", Value::Primitive(Primitive::LessEq))
  env_define(env, ">=", Value::Primitive(Primitive::GreaterEq))
  env_define(env, "cons", Value::Primitive(Primitive::Cons))
  env_define(env, "car", Value::Primitive(Primitive::Car))
  env_define(env, "cdr", Value::Primitive(Primitive::Cdr))
  env_define(env, "list", Value::Primitive(Primitive::List))
  env_define(env, "null?", Value::Primitive(Primitive::NullP))
  env_define(env, "pair?", Value::Primitive(Primitive::PairP))
  env_define(env, "symbol?", Value::Primitive(Primitive::SymbolP))
  env_define(env, "boolean?", Value::Primitive(Primitive::BooleanP))
  env_define(env, "number?", Value::Primitive(Primitive::NumberP))
  env_define(env, "procedure?", Value::Primitive(Primitive::ProcedureP))
  env_define(env, "not", Value::Primitive(Primitive::Not))
  env_define(env, "apply", Value::Primitive(Primitive::Apply))
  env_define(env, "eq?", Value::Primitive(Primitive::Eq))
  env_define(env, "eqv?", Value::Primitive(Primitive::Eqv))
  env_define(env, "equal?", Value::Primitive(Primitive::Equal))
  env_define(env, "list?", Value::Primitive(Primitive::ListP))
  env_define(env, "length", Value::Primitive(Primitive::Length))
  env_define(env, "append", Value::Primitive(Primitive::Append))
  env_define(env, "reverse", Value::Primitive(Primitive::Reverse))
  env_define(env, "char?", Value::Primitive(Primitive::CharP))
  env_define(env, "string?", Value::Primitive(Primitive::StringP))
  env_define(env, "string-length", Value::Primitive(Primitive::StringLength))
  env_define(env, "string-append", Value::Primitive(Primitive::StringAppend))
  env_define(env, "string-ref", Value::Primitive(Primitive::StringRef))
  env_define(env, "vector", Value::Primitive(Primitive::Vector))
  env_define(env, "vector?", Value::Primitive(Primitive::VectorP))
  env_define(env, "vector-length", Value::Primitive(Primitive::VectorLength))
  env_define(env, "vector-ref", Value::Primitive(Primitive::VectorRef))
  env_define(env, "vector-set!", Value::Primitive(Primitive::VectorSet))
  env_define(env, "bytevector", Value::Primitive(Primitive::ByteVector))
  env_define(env, "bytevector?", Value::Primitive(Primitive::ByteVectorP))
  env_define(env, "bytevector-length", Value::Primitive(Primitive::ByteVectorLength))
  env_define(env, "bytevector-u8-ref", Value::Primitive(Primitive::ByteVectorU8Ref))
  env_define(env, "bytevector-u8-set!", Value::Primitive(Primitive::ByteVectorU8Set))
  env_define(env, "display", Value::Primitive(Primitive::Display))
  env_define(env, "write", Value::Primitive(Primitive::Write))
  env_define(env, "newline", Value::Primitive(Primitive::Newline))
  env_define(env, "open-output-string", Value::Primitive(Primitive::OpenOutputString))
  env_define(env, "get-output-string", Value::Primitive(Primitive::GetOutputString))
  env_define(env, "current-output-port", Value::Primitive(Primitive::CurrentOutputPort))
  env_define(env, "call/cc", Value::Primitive(Primitive::CallCC))
  env_define(
    env,
    "call-with-current-continuation",
    Value::Primitive(Primitive::CallCC),
  )
  env
}

///|
let closure_counter : Ref[Int] = Ref::new(0)

///|
fn next_closure_id() -> Int {
  let id = closure_counter.val
  closure_counter.val = id + 1
  id
}

///|
fn make_closure(params : Array[String], body : Array[Datum], env : Env) -> Closure {
  { id: next_closure_id(), params, body, env }
}

///|
priv enum Kont {
  Halt
  If(Datum, Datum, Env, Kont)
  Begin(Array[Datum], Env, Kont)
  Set(String, Env, Kont)
  Define(String, Env, Kont)
  AppFun(Array[Datum], Env, Kont)
  AppArgs(Value, Array[Value], Array[Datum], Env, Kont)
  And(Array[Datum], Env, Kont)
  Or(Array[Datum], Env, Kont)
  Cond(Array[Datum], Array[Datum], Env, Kont)
  Let(Array[(String, Datum)], Int, Array[Value], Env, Array[Datum], Kont)
  LetStar(Array[(String, Datum)], Int, Env, Array[Datum], Kont)
  LetRec(Array[(String, Datum)], Int, Env, Array[Datum], Kont)
}

///|
struct Continuation {
  id : Int
  kont : Kont
}

///|
priv enum MachineState {
  Eval(Datum, Env, Kont)
  Apply(Value, Kont)
}

///|
let continuation_counter : Ref[Int] = Ref::new(0)

///|
fn next_continuation_id() -> Int {
  let id = continuation_counter.val
  continuation_counter.val = id + 1
  id
}

///|
pub fn eval_program(src : String) -> Value raise {
  let results = eval_program_all(src)
  if results.is_empty() {
    Value::Void
  } else {
    results[results.length() - 1]
  }
}

///|
pub fn eval_program_all(src : String) -> Array[Value] raise {
  let forms = parse_program(src)
  let env = init_env()
  let results : Array[Value] = []
  for form in forms {
    let value = eval_expr(form, env)
    results.push(value)
  }
  results
}

///|
fn eval_expr(expr : Datum, env : Env) -> Value raise EvalError {
  let mut state = MachineState::Eval(expr, env, Kont::Halt)
  while true {
    match state {
      MachineState::Eval(current_expr, current_env, kont) => {
        match current_expr {
          Datum::Nil
          | Datum::Bool(_)
          | Datum::Int(_)
          | Datum::Rat(_, _)
          | Datum::Float(_)
          | Datum::Char(_)
          | Datum::String(_)
          | Datum::Vector(_)
          | Datum::ByteVector(_) =>
            state = MachineState::Apply(Value::Datum(current_expr), kont)
          Datum::Symbol(name) => {
            match env_lookup(current_env, name) {
              Value::Macro(_) => raise EvalError("macro used as value")
              value => state = MachineState::Apply(value, kont)
            }
          }
          Datum::Pair(_, _) => state = eval_pair(current_expr, current_env, kont)
        }
      }
      MachineState::Apply(value, kont) => {
        match kont {
          Kont::Halt => return value
          Kont::If(then_expr, else_expr, env, next) => {
            if is_false(value) {
              state = MachineState::Eval(else_expr, env, next)
            } else {
              state = MachineState::Eval(then_expr, env, next)
            }
          }
          Kont::Begin(rest, env, next) => {
            if rest.is_empty() {
              state = MachineState::Apply(value, next)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::Begin(tail, env, next))
            }
          }
          Kont::Set(name, env, next) => {
            env_set(env, name, value)
            state = MachineState::Apply(Value::Void, next)
          }
          Kont::Define(name, env, next) => {
            env_define(env, name, value)
            state = MachineState::Apply(Value::Void, next)
          }
          Kont::AppFun(args, env, next) => {
            if args.is_empty() {
              state = apply_proc(value, [], next)
            } else {
              let rest = args.sub(start=1).to_array()
              state = MachineState::Eval(args[0], env, Kont::AppArgs(value, [], rest, env, next))
            }
          }
          Kont::AppArgs(proc, evaluated, remaining, env, next) => {
            let next_values : Array[Value] = []
            for item in evaluated {
              next_values.push(item)
            }
            next_values.push(value)
            if remaining.is_empty() {
              state = apply_proc(proc, next_values, next)
            } else {
              let rest = remaining.sub(start=1).to_array()
              state = MachineState::Eval(remaining[0], env, Kont::AppArgs(proc, next_values, rest, env, next))
            }
          }
          Kont::And(rest, env, next) => {
            if is_false(value) {
              state = MachineState::Apply(value, next)
            } else if rest.is_empty() {
              state = MachineState::Apply(value, next)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::And(tail, env, next))
            }
          }
          Kont::Or(rest, env, next) => {
            if !is_false(value) {
              state = MachineState::Apply(value, next)
            } else if rest.is_empty() {
              state = MachineState::Apply(value, next)
            } else {
              let tail = rest.sub(start=1).to_array()
              state = MachineState::Eval(rest[0], env, Kont::Or(tail, env, next))
            }
          }
          Kont::Cond(parts, rest, env, next) => {
            if is_false(value) {
              state = cond_start(rest, env, next)
            } else if parts.length() == 1 {
              state = MachineState::Apply(value, next)
            } else {
              let body = parts.sub(start=1).to_array()
              state = eval_sequence_state(body, env, next)
            }
          }
          Kont::Let(bindings, index, values, env, body, next) => {
            let next_values : Array[Value] = []
            for item in values {
              next_values.push(item)
            }
            next_values.push(value)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::Let(bindings, next_index, next_values, env, body, next),
              )
            } else {
              let new_env = env_extend(env)
              let mut i = 0
              while i < bindings.length() {
                let (name, _) = bindings[i]
                env_define(new_env, name, next_values[i])
                i = i + 1
              }
              state = eval_sequence_state(body, new_env, next)
            }
          }
          Kont::LetStar(bindings, index, env, body, next) => {
            let (name, _) = bindings[index]
            let new_env = env_extend(env)
            env_define(new_env, name, value)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                new_env,
                Kont::LetStar(bindings, next_index, new_env, body, next),
              )
            } else {
              state = eval_sequence_state(body, new_env, next)
            }
          }
          Kont::LetRec(bindings, index, env, body, next) => {
            let (name, _) = bindings[index]
            env_set(env, name, value)
            let next_index = index + 1
            if next_index < bindings.length() {
              let (_, expr) = bindings[next_index]
              state = MachineState::Eval(
                expr,
                env,
                Kont::LetRec(bindings, next_index, env, body, next),
              )
            } else {
              state = eval_sequence_state(body, env, next)
            }
          }
        }
      }
    }
  }
  Value::Void
}

///|
///|
fn eval_quote(args : Array[Datum]) -> Value raise EvalError {
  match args.length() {
    1 => Value::Datum(args[0])
    n => raise arity_mismatch(1, n)
  }
}

///|
fn eval_quasiquote(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    1 => Value::Datum(quasiquote_expand(args[0], env, 1))
    n => raise arity_mismatch(1, n)
  }
}

///|
fn unwrap_form(expr : Datum, name : String) -> Datum? {
  match expr {
    Datum::Pair(Datum::Symbol(head), Datum::Pair(arg, Datum::Nil)) if head == name => Some(arg)
    _ => None
  }
}

///|
fn quasiquote_expand(expr : Datum, env : Env, depth : Int) -> Datum raise EvalError {
  match unwrap_form(expr, "quasiquote") {
    Some(arg) =>
      return list_from_array([Datum::Symbol("quasiquote"), quasiquote_expand(arg, env, depth + 1)])
    None => ()
  }
  match unwrap_form(expr, "unquote") {
    Some(arg) => {
      if depth == 1 {
        return value_as_datum(eval_expr(arg, env))
      } else {
        return list_from_array([Datum::Symbol("unquote"), quasiquote_expand(arg, env, depth - 1)])
      }
    }
    None => ()
  }
  match unwrap_form(expr, "unquote-splicing") {
    Some(arg) => {
      if depth == 1 {
        raise EvalError("unquote-splicing not in list context")
      } else {
        return list_from_array([
          Datum::Symbol("unquote-splicing"),
          quasiquote_expand(arg, env, depth - 1),
        ])
      }
    }
    None => ()
  }
  match expr {
    Datum::Pair(car, cdr) => {
      if depth == 1 {
        match unwrap_form(car, "unquote-splicing") {
          Some(arg) => {
            let value = eval_expr(arg, env)
            let datum = value_as_datum(value)
            let items = datum_list_to_array(datum)
            let tail = quasiquote_expand(cdr, env, depth)
            return list_with_tail(items, tail)
          }
          None => ()
        }
      }
      Datum::Pair(
        quasiquote_expand(car, env, depth),
        quasiquote_expand(cdr, env, depth),
      )
    }
    _ => expr
  }
}

///|
fn list_with_tail(items : Array[Datum], tail : Datum) -> Datum {
  let mut result = tail
  for item in items.rev_iter() {
    result = Datum::Pair(item, result)
  }
  result
}
///|
fn parse_params(expr : Datum) -> Array[String] raise EvalError {
  let params = datum_list_to_array(expr)
  let names : Array[String] = []
  for param in params {
    match param {
      Datum::Symbol(name) => names.push(name)
      _ => raise EvalError("type error: symbol expected")
    }
  }
  names
}

///|
fn eval_lambda(args : Array[Datum], env : Env) -> Value raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let params = parse_params(args[0])
    let body = args.sub(start=1).to_array()
    Value::Closure(make_closure(params, body, env))
  }
}

///|
fn eval_define_syntax(args : Array[Datum], env : Env) -> Value raise EvalError {
  match args.length() {
    2 => {
      match args[0] {
        Datum::Symbol(name) => {
          let rules = parse_syntax_rules(args[1])
          env_define(env, name, Value::Macro(rules))
          Value::Void
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
    n => raise arity_mismatch(2, n)
  }
}

///|
fn parse_syntax_bindings(expr : Datum) -> Array[(String, SyntaxRules)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, SyntaxRules)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("type error: syntax binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => {
          let rules = parse_syntax_rules(parts[1])
          parsed.push((name, rules))
        }
        _ => raise EvalError("type error: symbol expected")
      }
    }
  }
  parsed
}

///|
fn parse_bindings(expr : Datum) -> Array[(String, Datum)] raise EvalError {
  let bindings = datum_list_to_array(expr)
  let parsed : Array[(String, Datum)] = []
  for binding in bindings {
    let parts = datum_list_to_array(binding)
    if parts.length() != 2 {
      raise EvalError("type error: binding expected")
    } else {
      match parts[0] {
        Datum::Symbol(name) => parsed.push((name, parts[1]))
        _ => raise EvalError("type error: symbol expected")
      }
    }
  }
  parsed
}

///|
fn eval_sequence_state(exprs : Array[Datum], env : Env, kont : Kont) -> MachineState {
  if exprs.is_empty() {
    MachineState::Apply(Value::Void, kont)
  } else {
    let rest = exprs.sub(start=1).to_array()
    MachineState::Eval(exprs[0], env, Kont::Begin(rest, env, kont))
  }
}

///|
fn eval_if_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  if args.length() != 3 {
    raise arity_mismatch(3, args.length())
  } else {
    MachineState::Eval(args[0], env, Kont::If(args[1], args[2], env, kont))
  }
}

///|
fn eval_define_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    match args[0] {
      Datum::Symbol(name) => {
        match args.length() {
          2 => MachineState::Eval(args[1], env, Kont::Define(name, env, kont))
          n => raise arity_mismatch(2, n)
        }
      }
      Datum::Pair(_, _) => {
        let sig = datum_list_to_array(args[0])
        if sig.is_empty() {
          raise EvalError("type error: symbol expected")
        } else {
          match sig[0] {
            Datum::Symbol(name) => {
              let params = parse_params(list_from_array(sig.sub(start=1).to_array()))
              let body = args.sub(start=1).to_array()
              env_define(env, name, Value::Closure(make_closure(params, body, env)))
              MachineState::Apply(Value::Void, kont)
            }
            _ => raise EvalError("type error: symbol expected")
          }
        }
      }
      _ => raise EvalError("type error: symbol expected")
    }
  }
}

///|
fn eval_set_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  match args.length() {
    2 => {
      match args[0] {
        Datum::Symbol(name) => MachineState::Eval(args[1], env, Kont::Set(name, env, kont))
        _ => raise EvalError("type error: symbol expected")
      }
    }
    n => raise arity_mismatch(2, n)
  }
}

///|
fn eval_let_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(expr, env, Kont::Let(bindings, 0, [], env, body, kont))
    }
  }
}

///|
fn eval_let_star_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    if bindings.is_empty() {
      eval_sequence_state(body, env, kont)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(expr, env, Kont::LetStar(bindings, 0, env, body, kont))
    }
  }
}

///|
fn eval_letrec_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, _) = binding
      env_define(new_env, name, Value::Void)
    }
    if bindings.is_empty() {
      eval_sequence_state(body, new_env, kont)
    } else {
      let (_, expr) = bindings[0]
      MachineState::Eval(expr, new_env, Kont::LetRec(bindings, 0, new_env, body, kont))
    }
  }
}

///|
fn eval_let_syntax_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, rules) = binding
      env_define(new_env, name, Value::Macro(rules))
    }
    eval_sequence_state(body, new_env, kont)
  }
}

///|
fn eval_letrec_syntax_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let bindings = parse_syntax_bindings(args[0])
    let body = args.sub(start=1).to_array()
    let new_env = env_extend(env)
    for binding in bindings {
      let (name, rules) = binding
      env_define(new_env, name, Value::Macro(rules))
    }
    eval_sequence_state(body, new_env, kont)
  }
}

///|
fn eval_and_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState {
  if args.is_empty() {
    MachineState::Apply(bool_value(true), kont)
  } else {
    let rest = args.sub(start=1).to_array()
    MachineState::Eval(args[0], env, Kont::And(rest, env, kont))
  }
}

///|
fn eval_or_state(args : Array[Datum], env : Env, kont : Kont) -> MachineState {
  if args.is_empty() {
    MachineState::Apply(bool_value(false), kont)
  } else {
    let rest = args.sub(start=1).to_array()
    MachineState::Eval(args[0], env, Kont::Or(rest, env, kont))
  }
}

///|
fn cond_start(clauses : Array[Datum], env : Env, kont : Kont) -> MachineState raise EvalError {
  let mut i = 0
  while i < clauses.length() {
    let parts = datum_list_to_array(clauses[i])
    if parts.is_empty() {
      i = i + 1
    } else {
      let head = symbol_name(parts[0])
      if head is Some("else") {
        let body = parts.sub(start=1).to_array()
        return eval_sequence_state(body, env, kont)
      } else {
        let rest = clauses.sub(start=i + 1).to_array()
        return MachineState::Eval(parts[0], env, Kont::Cond(parts, rest, env, kont))
      }
    }
  }
  MachineState::Apply(Value::Void, kont)
}

///|
fn eval_pair(expr : Datum, env : Env, kont : Kont) -> MachineState raise EvalError {
  let items = datum_list_to_array(expr)
  if items.is_empty() {
    MachineState::Apply(Value::Datum(Datum::Nil), kont)
  } else {
    let head = items[0]
    let args = items.sub(start=1).to_array()
    match symbol_name(head) {
      Some("define-syntax") => MachineState::Apply(eval_define_syntax(args, env), kont)
      Some("quote") => MachineState::Apply(eval_quote(args), kont)
      Some("quasiquote") => MachineState::Apply(eval_quasiquote(args, env), kont)
      Some("if") => eval_if_state(args, env, kont)
      Some("begin") => eval_sequence_state(args, env, kont)
      Some("lambda") => MachineState::Apply(eval_lambda(args, env), kont)
      Some("define") => eval_define_state(args, env, kont)
      Some("set!") => eval_set_state(args, env, kont)
      Some("let") => eval_let_state(args, env, kont)
      Some("let*") => eval_let_star_state(args, env, kont)
      Some("letrec") => eval_letrec_state(args, env, kont)
      Some("let-syntax") => eval_let_syntax_state(args, env, kont)
      Some("letrec-syntax") => eval_letrec_syntax_state(args, env, kont)
      Some("and") => eval_and_state(args, env, kont)
      Some("or") => eval_or_state(args, env, kont)
      Some("cond") => cond_start(args, env, kont)
      Some(name) => {
        match env_get_macro(env, name) {
          Some(rules) => {
            let expanded = expand_macro(rules, expr, env)
            MachineState::Eval(expanded, env, kont)
          }
          None => MachineState::Eval(head, env, Kont::AppFun(args, env, kont))
        }
      }
      None => MachineState::Eval(head, env, Kont::AppFun(args, env, kont))
    }
  }
}

///|
fn apply_proc(proc : Value, args : Array[Value], kont : Kont) -> MachineState raise EvalError {
  match proc {
    Value::Primitive(Primitive::Apply) => apply_apply(args, kont)
    Value::Primitive(Primitive::CallCC) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        let cont = Continuation::{ id: next_continuation_id(), kont }
        apply_proc(args[0], [Value::Continuation(cont)], kont)
      }
    }
    Value::Primitive(prim) => MachineState::Apply(apply_primitive(prim, args), kont)
    Value::Closure(cl) => {
      if args.length() != cl.params.length() {
        raise arity_mismatch(cl.params.length(), args.length())
      } else {
        let new_env = env_extend(cl.env)
        let mut i = 0
        while i < cl.params.length() {
          env_define(new_env, cl.params[i], args[i])
          i = i + 1
        }
        eval_sequence_state(cl.body, new_env, kont)
      }
    }
    Value::Continuation(cont) => {
      if args.length() != 1 {
        raise arity_mismatch(1, args.length())
      } else {
        MachineState::Apply(args[0], cont.kont)
      }
    }
    _ => raise EvalError("type error: procedure expected")
  }
}

///|
fn apply_apply(args : Array[Value], kont : Kont) -> MachineState raise EvalError {
  if args.length() < 2 {
    raise arity_mismatch(2, args.length())
  } else {
    let proc = args[0]
    let flat : Array[Value] = []
    let mut i = 1
    while i < args.length() - 1 {
      flat.push(args[i])
      i = i + 1
    }
    let list_value = value_as_datum(args[args.length() - 1])
    let tail = datum_list_to_value_array(list_value)
    flat.append(tail[:])
    apply_proc(proc, flat, kont)
  }
}
