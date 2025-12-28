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
```
