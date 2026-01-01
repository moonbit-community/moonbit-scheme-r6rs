# runtime

Runtime helpers for environments, records, conditions, ports, and value
conversion.

## Entry points

- env_new and env_lookup for lexical environments
- value_to_string for rendering values
- make_record_type and make_record_instance for records
- new_output_string_port and port_get_output_string for ports

## Example

```mbt
///|
let env = @runtime.env_new()

///|
let port = @runtime.new_output_string_port()
```

```mbt check
///|
test "string port" {
  let port = new_output_string_port()
  port_write(port, "hi")
  inspect(port_get_output_string(port), content="hi")
}

///|
test "env basics" {
  let env = env_new()
  env_define(env, "x", @core.Value::Datum(@core.Datum::Int(1)))
  match env_lookup_optional(env, "x") {
    Some(@core.Value::Datum(@core.Datum::Int(1))) => ()
    _ => fail("expected bound value")
  }
}

///|
test "env set" {
  let env = env_new()
  env_define(env, "x", @core.Value::Datum(@core.Datum::Int(1)))
  env_set(env, "x", @core.Value::Datum(@core.Datum::Int(2)))
  match env_lookup_optional(env, "x") {
    Some(@core.Value::Datum(@core.Datum::Int(2))) => ()
    _ => fail("expected updated value")
  }
}

///|
test "env clone" {
  let env = env_new()
  env_define(env, "x", @core.Value::Datum(@core.Datum::Int(1)))
  let cloned = env_clone(env)
  env_set(env, "x", @core.Value::Datum(@core.Datum::Int(2)))
  match env_lookup_optional(cloned, "x") {
    Some(@core.Value::Datum(@core.Datum::Int(1))) => ()
    _ => fail("expected cloned value to stay 1")
  }
}

///|
test "gensym unique suffix" {
  let a = gensym("x")
  let b = gensym("x")
  inspect(a != b, content="true")
  guard a is [.."x__gs", .._rest] else { fail("expected gensym prefix") }
}

///|
test "symbol name" {
  match symbol_name(@core.Datum::Symbol("x")) {
    Some("x") => ()
    _ => fail("expected symbol name")
  }
}

///|
test "enum set from names" {
  let set = enum_set_from_names(["a", "b"], ["b"])
  inspect(enum_set_member_by_name(set, "b"), content="true")
  inspect(enum_set_member_by_name(set, "a"), content="false")
}

///|
test "enum set index" {
  inspect(enum_set_index_of(["a", "b"], "b"), content="Some(1)")
}

///|
test "enum set universe equal" {
  let base = ["a", "b"]
  let same = enum_set_from_names(base, [])
  let other = enum_set_from_names(["b", "a"], [])
  inspect(enum_set_universe_equal(same, same), content="true")
  inspect(enum_set_universe_equal(same, other), content="false")
}

///|
test "value to string" {
  let value = @core.Value::Datum(@core.Datum::Int(5))
  inspect(value_to_string(value), content="5")
}

///|
test "datum unlabel" {
  match datum_unlabel(@core.Datum::Int(3)) {
    @core.Datum::Int(3) => ()
    _ => fail("expected same datum")
  }
}

///|
test "library exports" {
  let binding = make_binding(@core.Value::Datum(@core.Datum::Int(1)))
  register_library("doc/runtime-lib", { "x": binding })
  match lookup_library("doc/runtime-lib") {
    Some(exports) =>
      match exports.get("x") {
        Some(exported) =>
          match exported.value() {
            @core.Value::Datum(@core.Datum::Int(1)) => ()
            _ => fail("expected datum export")
          }
        None => fail("expected export")
      }
    None => fail("expected library")
  }
}
```
