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
test "enum set from names" {
  let set = enum_set_from_names(["a", "b"], ["b"])
  inspect(enum_set_member_by_name(set, "b"), content="true")
  inspect(enum_set_member_by_name(set, "a"), content="false")
}

///|
test "value to string" {
  let value = @core.Value::Datum(@core.Datum::Int(5))
  inspect(value_to_string(value), content="5")
}

///|
test "normalize rat" {
  match normalize_rat(4, 6) {
    Some(@core.Datum::Rat(2, 3)) => ()
    _ => fail("expected reduced ratio")
  }
}
```
