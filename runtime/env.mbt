///|
/// Create a fresh environment with a single empty frame.
///
/// # Example
/// ```mbt check
/// test "env define and lookup" {
///   let env = env_new()
///   env_define(env, "x", Datum(Int(1)))
///   match env_lookup_optional(env, "x") {
///     Some(Datum(Int(1))) => ()
///     _ => fail("expected bound value")
///   }
/// }
/// ```
pub fn env_new() -> Env {
  let env : Env = []
  env..push({})
  env
}

///|
/// Extend an environment by pushing a new empty frame.
pub fn env_extend(env : Env) -> Env {
  let new_env = env.copy()
  new_env..push({})
  new_env
}

///|
/// Deep-clone environment frames and bindings.
///
/// # Example
/// ```mbt check
/// test "env clone isolates frames" {
///   let env = env_new()
///   env_define(env, "x", Datum(Int(1)))
///   let cloned = env_clone(env)
///   env_set(env, "x", Datum(Int(2)))
///   match env_lookup_optional(cloned, "x") {
///     Some(Datum(Int(1))) => ()
///     _ => fail("expected cloned value to stay 1")
///   }
/// }
/// ```
pub fn env_clone(env : Env) -> Env {
  env.map((frame) =>
    frame.map((_, binding) => Binding::{ id: binding.id, value: binding.value })
  )
}

///|
fn Env::last_frame(self : Env) -> Map[String, Binding] {
  self[self.length() - 1]
}

///|
fn Env::get_binding(self : Env, name : String) -> Binding? {
  if self.is_empty() {
    return None
  }
  let result : Binding? = for i = self.length() - 1; i >= 0; i = i - 1 {
    // invariant : i >= 0 && i < self.length()
    // decreases : i
    // assert : i < self.length()
    let frame = self[i]
    match frame.get(name) {
      Some(binding) => break Some(binding)
      None => ()
    }
  } else {
    None
  }
  result
}

///|
/// Look up a macro transformer by name, if present.
pub fn env_get_macro(env : Env, name : String) -> MacroTransformer? {
  match env.get_binding(name) {
    Some(binding) =>
      match binding.value {
        Macro(transformer) => Some(transformer)
        _ => None
      }
    _ => None
  }
}

///|
/// Define a new binding in the current frame.
pub fn env_define(env : Env, name : String, value : Value) -> Unit {
  let frame = env.last_frame()
  frame[name] = Binding::{ id: next_binding_id(), value }
}

///|
/// Define an existing binding in the current frame.
pub fn env_define_binding(env : Env, name : String, binding : Binding) -> Unit {
  let frame = env.last_frame()
  frame[name] = binding
}

///|
/// Look up a binding by name or raise if it is unbound.
pub fn env_lookup_binding(env : Env, name : String) -> Binding raise EvalError {
  match env.get_binding(name) {
    Some(binding) => binding
    None => raise @core.EvalError("unbound variable: \{name}")
  }
}

///|
/// Look up a binding by name, returning None if missing.
pub fn env_lookup_binding_optional(env : Env, name : String) -> Binding? {
  env.get_binding(name)
}

///|
/// Look up a binding id by name, returning None if missing.
pub fn env_binding_id_optional(env : Env, name : String) -> Int? {
  env.get_binding(name).map((binding) => binding.id)
}

///|
/// Look up a binding value or raise if it is unbound.
///
/// # Example
/// ```mbt check
/// test "env lookup missing" {
///   let env = env_new()
///   try env_lookup(env, "missing") catch {
///     _ => ()
///   } noraise {
///     _ => fail("expected error")
///   }
/// }
/// ```
pub fn env_lookup(env : Env, name : String) -> Value raise EvalError {
  env_lookup_binding(env, name).value
}

///|
/// Look up a binding value, returning None if missing.
pub fn env_lookup_optional(env : Env, name : String) -> Value? {
  env.get_binding(name).map((binding) => binding.value)
}

///|
/// Update an existing binding or raise if it is unbound.
///
/// # Example
/// ```mbt check
/// test "env set" {
///   let env = env_new()
///   env_define(env, "x", Datum(Int(1)))
///   env_set(env, "x", Datum(Int(2)))
///   match env_lookup_optional(env, "x") {
///     Some(Datum(Int(2))) => ()
///     _ => fail("expected updated value")
///   }
/// }
/// ```
pub fn env_set(env : Env, name : String, value : Value) -> Unit raise EvalError {
  if env.is_empty() {
    raise @core.EvalError("unbound variable: \{name}")
  }
  let updated = for i = env.length() - 1; i >= 0; i = i - 1 {
    // invariant : i >= 0 && i < env.length()
    // decreases : i
    // assert : i < env.length()
    let frame = env[i]
    if frame.contains(name) {
      let binding = frame[name]
      frame[name] = Binding::{ id: binding.id, value }
      break true
    }
  } else {
    false
  }
  if !updated {
    raise @core.EvalError("unbound variable: \{name}")
  }
}
