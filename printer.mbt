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
fn char_to_string(ch : Char) -> String {
  match ch {
    ' ' => "#\\space"
    '\n' => "#\\newline"
    '\t' => "#\\tab"
    _ => {
      let buf = @buffer.new()
      buf.write_string("#\\")
      buf.write_char(ch)
      buf.to_string()
    }
  }
}

///|
fn datum_to_string(datum : Datum) -> String {
  match datum {
    Datum::Nil => "()"
    Datum::Bool(true) => "#t"
    Datum::Bool(false) => "#f"
    Datum::Int(n) => n.to_string()
    Datum::Rat(n, d) => "\{n}/\{d}"
    Datum::Float(f) => f.to_string()
    Datum::Char(ch) => char_to_string(ch)
    Datum::String(s) => "\"\{escape_string(s.val)}\""
    Datum::Symbol(name) => name
    Datum::Pair(_, _) => datum_list_to_string(datum)
    Datum::Vector(items) => vector_to_string(items)
    Datum::ByteVector(items) => bytevector_to_string(items)
  }
}

///|
fn vector_to_string(items : Array[Datum]) -> String {
  let parts : Array[String] = []
  for item in items {
    parts.push(datum_to_string(item))
  }
  let joined = parts.join(" ")
  "#(\{joined})"
}

///|
fn bytevector_to_string(items : Array[Int]) -> String {
  let parts : Array[String] = []
  for item in items {
    parts.push(item.to_string())
  }
  let joined = parts.join(" ")
  "#vu8(\{joined})"
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
        parts.push(datum_to_string(a.val))
        cur = b.val
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
    Value::CaseClosure(_) => "#<procedure>"
    Value::Values(_) => "#<values>"
    Value::GuardHandler(_) => "#<procedure>"
    Value::Parameter(_) => "#<procedure>"
    Value::Continuation(_) => "#<procedure>"
    Value::Port(_) => "#<port>"
    Value::Record(_) => "#<record>"
    Value::RecordProc(_) => "#<procedure>"
    Value::Macro(_) => "#<syntax>"
  }
}
