///|
/// Create a fresh environment with a single empty frame.
pub fn env_new() -> Env {
  let env : Env = []
  env.push({})
  env
}

///|
/// Extend an environment by pushing a new empty frame.
pub fn env_extend(env : Env) -> Env {
  let new_env = env.copy()
  new_env.push({})
  new_env
}

///|
/// Deep-clone environment frames and bindings.
pub fn env_clone(env : Env) -> Env {
  let new_env : Env = []
  let mut i = 0
  while i < env.length() {
    let frame = env[i]
    let new_frame : Map[String, Binding] = {}
    for key in frame.keys() {
      match frame.get(key) {
        Some(binding) =>
          new_frame[key] = Binding::{ id: binding.id, value: binding.value }
        None => ()
      }
    }
    new_env.push(new_frame)
    i = i + 1
  }
  new_env
}

///|
fn env_get(env : Env, name : String) -> Binding? {
  let mut i = env.length() - 1
  while true {
    let frame = env[i]
    match frame.get(name) {
      Some(binding) => return Some(binding)
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
/// Look up a macro transformer by name, if present.
pub fn env_get_macro(env : Env, name : String) -> MacroTransformer? {
  match env_get(env, name) {
    Some(binding) =>
      match binding.value {
        Value::Macro(transformer) => Some(transformer)
        _ => None
      }
    _ => None
  }
}

///|
/// Define a new binding in the current frame.
pub fn env_define(env : Env, name : String, value : Value) -> Unit {
  let idx = env.length() - 1
  env[idx][name] = Binding::{ id: next_binding_id(), value }
}

///|
/// Define an existing binding in the current frame.
pub fn env_define_binding(env : Env, name : String, binding : Binding) -> Unit {
  let idx = env.length() - 1
  env[idx][name] = binding
}

///|
/// Look up a binding by name or raise if it is unbound.
pub fn env_lookup_binding(env : Env, name : String) -> Binding raise EvalError {
  let mut i = env.length() - 1
  while i >= 0 {
    let frame = env[i]
    match frame.get(name) {
      Some(binding) => return binding
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
/// Look up a binding by name, returning None if missing.
pub fn env_lookup_binding_optional(env : Env, name : String) -> Binding? {
  let mut i = env.length() - 1
  while i >= 0 {
    let frame = env[i]
    match frame.get(name) {
      Some(binding) => return Some(binding)
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
/// Look up a binding id by name, returning None if missing.
pub fn env_binding_id_optional(env : Env, name : String) -> Int? {
  match env_lookup_binding_optional(env, name) {
    Some(binding) => Some(binding.id)
    None => None
  }
}

///|
/// Look up a binding value or raise if it is unbound.
pub fn env_lookup(env : Env, name : String) -> Value raise EvalError {
  let mut i = env.length() - 1
  while i >= 0 {
    let frame = env[i]
    match frame.get(name) {
      Some(binding) => return binding.value
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
/// Look up a binding value, returning None if missing.
pub fn env_lookup_optional(env : Env, name : String) -> Value? {
  let mut i = env.length() - 1
  while i >= 0 {
    let frame = env[i]
    match frame.get(name) {
      Some(binding) => return Some(binding.value)
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
/// Update an existing binding or raise if it is unbound.
pub fn env_set(env : Env, name : String, value : Value) -> Unit raise EvalError {
  let mut i = env.length() - 1
  while i >= 0 {
    let frame = env[i]
    if frame.contains(name) {
      let binding = frame[name]
      frame[name] = Binding::{ id: binding.id, value }
      return
    }
    if i == 0 {
      break
    }
    i = i - 1
  }
  raise @core.EvalError("unbound variable: \{name}")
}
