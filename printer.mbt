///|
fn escape_string(s : String) -> String {
  let buf = @buffer.new()
  for ch in s {
    match ch {
      '"' => buf.write_string("\\\"")
      '\\' => buf.write_string("\\\\")
      '\n' => buf.write_string("\\n")
      '\t' => buf.write_string("\\t")
      '\r' => buf.write_string("\\r")
      _ => buf.write_char(ch)
    }
  }
  buf.to_string()
}

///|
fn datum_to_string(datum : Datum) -> String {
  match datum {
    Datum::Nil => "()"
    Datum::Bool(true) => "#t"
    Datum::Bool(false) => "#f"
    Datum::Int(n) => n.to_string()
    Datum::String(s) => "\"\{escape_string(s)}\""
    Datum::Symbol(name) => name
    Datum::Pair(_, _) => datum_list_to_string(datum)
  }
}

///|
fn datum_list_to_string(list : Datum) -> String {
  let parts : Array[String] = []
  let mut cur = list
  let mut result = ""
  while true {
    match cur {
      Datum::Nil => {
        let joined = parts.join(" ")
        result = "(\{joined})"
        break
      }
      Datum::Pair(a, b) => {
        parts.push(datum_to_string(a))
        cur = b
      }
      _ => {
        let prefix = parts.join(" ")
        result = "(\{prefix} . \{datum_to_string(cur)})"
        break
      }
    }
  }
  result
}

///|
pub fn value_to_string(value : Value) -> String {
  match value {
    Value::Void => "#<void>"
    Value::Datum(d) => datum_to_string(d)
    Value::Primitive(_) => "#<procedure>"
    Value::Closure(_) => "#<procedure>"
    Value::Macro(_) => "#<syntax>"
  }
}
