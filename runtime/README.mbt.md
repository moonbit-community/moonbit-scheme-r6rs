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
  env_define(env, "x", Datum(Int(1)))
  match env_lookup_optional(env, "x") {
    Some(Datum(Int(1))) => ()
    _ => fail("expected bound value")
  }
}

///|
test "env set" {
  let env = env_new()
  env_define(env, "x", Datum(Int(1)))
  env_set(env, "x", Datum(Int(2)))
  match env_lookup_optional(env, "x") {
    Some(Datum(Int(2))) => ()
    _ => fail("expected updated value")
  }
}

///|
test "env clone" {
  let env = env_new()
  env_define(env, "x", Datum(Int(1)))
  let cloned = env_clone(env)
  env_set(env, "x", Datum(Int(2)))
  match env_lookup_optional(cloned, "x") {
    Some(Datum(Int(1))) => ()
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
test "list from array" {
  let list = list_from_array([Int(1), Int(2)])
  let value = @core.Value::Datum(list)
  inspect(value_to_string(value), content="(1 2)")
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
test "value to string" {
  let value = @core.Value::Datum(@core.Datum::Int(5))
  inspect(value_to_string(value), content="5")
}

///|
test "normalize rat" {
  match normalize_rat(4, 6) {
    Some(Rat(2, 3)) => ()
    _ => fail("expected reduced ratio")
  }
}

///|
test "value from datum" {
  let value = value_from_datum(Int(3))
  match value {
    Datum(Int(3)) => ()
    _ => fail("expected datum value")
  }
}

///|
test "library exports" {
  let binding = @core.Binding::{
    id: next_binding_id(),
    value: Datum(Int(1)),
  }
  register_library("doc/runtime-lib", { "x": binding })
  match lookup_library("doc/runtime-lib") {
    Some(lib) =>
      match lib.exports().get("x") {
        Some(exported) =>
          match exported.value {
            Datum(Int(1)) => ()
            _ => fail("expected datum export")
          }
        None => fail("expected export")
      }
    None => fail("expected library")
  }
}
```
