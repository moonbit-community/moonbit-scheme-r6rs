///|
/// Create a fresh environment with a single empty frame.
///
/// # Example
/// ```mbt check
/// test "env define and lookup" {
///   let env = env_new()
///   env_define(env, "x", @core.Value::Datum(@core.Datum::Int(1)))
///   match env_lookup_optional(env, "x") {
///     Some(@core.Value::Datum(@core.Datum::Int(1))) => ()
///     _ => fail("expected bound value")
///   }
/// }
/// ```
pub fn env_new() -> @core.Env {
  let env : @core.Env = []
  env..push({})
  env
}

///|
/// Extend an environment by pushing a new empty frame.
pub fn env_extend(env : @core.Env) -> @core.Env {
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
///   env_define(env, "x", @core.Value::Datum(@core.Datum::Int(1)))
///   let cloned = env_clone(env)
///   env_set(env, "x", @core.Value::Datum(@core.Datum::Int(2)))
///   match env_lookup_optional(cloned, "x") {
///     Some(@core.Value::Datum(@core.Datum::Int(1))) => ()
///     _ => fail("expected cloned value to stay 1")
///   }
/// }
/// ```
pub fn env_clone(env : @core.Env) -> @core.Env {
  env.map((frame) =>
    frame.map((_, binding) => @core.Binding::{ id: binding.id, value: binding.value })
  )
}

///|
fn @core.Env::last_frame(self : @core.Env) -> Map[String, @core.Binding] {
  self[self.length() - 1]
}

///|
fn @core.Env::get_binding(self : @core.Env, name : String) -> @core.Binding? {
  if self.is_empty() {
    return None
  }
  let result : @core.Binding? = for i = self.length() - 1; i >= 0; i = i - 1 {
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
pub fn env_get_macro(env : @core.Env, name : String) -> @core.MacroTransformer? {
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
pub fn env_define(env : @core.Env, name : String, value : @core.Value) -> Unit {
  let frame = env.last_frame()
  frame[name] = make_binding(value)
}

///|
/// Create a new binding with a fresh id.
///
/// # Example
/// ```mbt check
/// test "make binding" {
///   let binding = make_binding(@core.Value::Datum(@core.Datum::Int(1)))
///   match binding.value {
///     @core.Value::Datum(@core.Datum::Int(1)) => ()
///     _ => fail("expected datum binding")
///   }
/// }
/// ```
pub fn make_binding(value : @core.Value) -> @core.Binding {
  @core.Binding::{ id: next_binding_id(), value }
}

///|
/// Define an existing binding in the current frame.
pub fn env_define_binding(env : @core.Env, name : String, binding : @core.Binding) -> Unit {
  let frame = env.last_frame()
  frame[name] = binding
}

///|
/// Look up a binding by name or raise if it is unbound.
pub fn env_lookup_binding(env : @core.Env, name : String) -> @core.Binding raise @core.EvalError {
  match env.get_binding(name) {
    Some(binding) => binding
    None => raise @core.EvalError("unbound variable: \{name}")
  }
}

/// Look up a binding id by name, returning None if missing.
pub fn env_binding_id_optional(env : @core.Env, name : String) -> Int? {
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
pub fn env_lookup(env : @core.Env, name : String) -> @core.Value raise @core.EvalError {
  env_lookup_binding(env, name).value
}

///|
/// Look up a binding value, returning None if missing.
pub fn env_lookup_optional(env : @core.Env, name : String) -> @core.Value? {
  env.get_binding(name).map((binding) => binding.value)
}

///|
/// Update an existing binding or raise if it is unbound.
///
/// # Example
/// ```mbt check
/// test "env set" {
///   let env = env_new()
///   env_define(env, "x", @core.Value::Datum(@core.Datum::Int(1)))
///   env_set(env, "x", @core.Value::Datum(@core.Datum::Int(2)))
///   match env_lookup_optional(env, "x") {
///     Some(@core.Value::Datum(@core.Datum::Int(2))) => ()
///     _ => fail("expected updated value")
///   }
/// }
/// ```
pub fn env_set(env : @core.Env, name : String, value : @core.Value) -> Unit raise @core.EvalError {
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
      frame[name] = @core.Binding::{ id: binding.id, value }
      break true
    }
  } else {
    false
  }
  if !updated {
    raise @core.EvalError("unbound variable: \{name}")
  }
}
