///|
fn env_new() -> Env {
  let env : Env = []
  env.push({})
  env
}

///|
fn env_extend(env : Env) -> Env {
  let new_env = env.copy()
  new_env.push({})
  new_env
}

///|
fn env_clone(env : Env) -> Env {
  let new_env : Env = []
  let mut i = 0
  while i < env.length() {
    let frame = env[i]
    let new_frame : Map[String, Value] = {}
    for key in frame.keys() {
      match frame.get(key) {
        Some(value) => new_frame[key] = value
        None => ()
      }
    }
    new_env.push(new_frame)
    i = i + 1
  }
  new_env
}

///|
fn env_get(env : Env, name : String) -> Value? {
  let mut i = env.length() - 1
  while true {
    let frame = env[i]
    match frame.get(name) {
      Some(v) => return Some(v)
      None => ()
    }
    if i == 0 {
      break
    }
    i = i - 1
  }
  None
}

///|
fn env_get_macro(env : Env, name : String) -> SyntaxRules? {
  match env_get(env, name) {
    Some(Value::Macro(rules)) => Some(rules)
    _ => None
  }
}

///|
fn env_define(env : Env, name : String, value : Value) -> Unit {
  let idx = env.length() - 1
  env[idx][name] = value
}

///|
fn env_lookup(env : Env, name : String) -> Value raise EvalError {
  let mut i = env.length() - 1
  while i >= 0 {
    let frame = env[i]
    match frame.get(name) {
      Some(v) => return v
      None => ()
    }
    if i == 0 {
      break
    }
    i = i - 1
  }
  raise @core.EvalError("unbound variable: \{name}")
}

///|
fn env_set(env : Env, name : String, value : Value) -> Unit raise EvalError {
  let mut i = env.length() - 1
  while i >= 0 {
    let frame = env[i]
    if frame.contains(name) {
      frame[name] = value
      return
    }
    if i == 0 {
      break
    }
    i = i - 1
  }
  raise @core.EvalError("unbound variable: \{name}")
}
