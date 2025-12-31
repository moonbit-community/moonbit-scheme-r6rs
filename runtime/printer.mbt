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
priv struct PrinterState {
  labels : Map[Int, Int]
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
fn datum_to_string_state(datum : Datum, state : PrinterState) -> String {
  match datum {
    Datum::Label(id, cell) => {
      let flags = match state.labels.get(id) {
        Some(value) => value
        None => 0
      }
      let defined = (flags & 1) != 0
      let active = (flags & 2) != 0
      if defined || active {
        return "#\{id}#"
      }
      state.labels[id] = 3
      let body = datum_to_string_state(cell.val, state)
      state.labels[id] = 1
      "#\{id}=\{body}"
    }
    Datum::Nil => "()"
    Datum::Bool(true) => "#t"
    Datum::Bool(false) => "#f"
    Datum::Int(n) => n.to_string()
    Datum::BigInt(n) => n.to_string()
    Datum::Rat(n, d) => "\{n}/\{d}"
    Datum::BigRat(n, d) => "\{n.to_string()}/\{d.to_string()}"
    Datum::Float(f) => f.to_string()
    Datum::Complex(real, imag) =>
      complex_to_string_state(real.val, imag.val, state)
    Datum::Char(ch) => char_to_string(ch)
    Datum::String(s) => "\"\{escape_string(s.val)}\""
    Datum::Symbol(name) => name
    Datum::Pair(_, _) => datum_list_to_string_state(datum, state)
    Datum::Vector(items) => vector_to_string_state(items, state)
    Datum::ByteVector(items) => bytevector_to_string(items)
    Datum::Record(_) => "#<record>"
    Datum::Condition(_) => "#<condition>"
    Datum::Value(value) => value_to_string(value)
  }
}

///|
fn datum_to_string(datum : Datum) -> String {
  let state : PrinterState = { labels: {} }
  datum_to_string_state(datum, state)
}

///|
fn datum_is_negative(value : Datum) -> Bool {
  match datum_unlabel(value) {
    Datum::Int(n) => n < 0
    Datum::BigInt(n) => n.compare_int(0) < 0
    Datum::Rat(n, _) => n < 0
    Datum::BigRat(n, _) => n.compare_int(0) < 0
    Datum::Float(f) => f < 0.0
    _ => false
  }
}

///|
fn datum_abs(value : Datum) -> Datum {
  match datum_unlabel(value) {
    Datum::Int(n) => Datum::Int(if n < 0 { -n } else { n })
    Datum::Rat(n, d) => Datum::Rat(if n < 0 { -n } else { n }, d)
    Datum::BigInt(n) =>
      if n.compare_int(0) < 0 {
        Datum::BigInt(-n)
      } else {
        Datum::BigInt(n)
      }
    Datum::BigRat(n, d) =>
      if n.compare_int(0) < 0 {
        Datum::BigRat(-n, d)
      } else {
        Datum::BigRat(n, d)
      }
    Datum::Float(f) => Datum::Float(if f < 0.0 { -f } else { f })
    _ => value
  }
}

///|
fn complex_to_string_state(
  real : Datum,
  imag : Datum,
  state : PrinterState,
) -> String {
  let real_str = datum_to_string_state(real, state)
  let negative = datum_is_negative(imag)
  let imag_abs = if negative { datum_abs(imag) } else { imag }
  let imag_str = datum_to_string_state(imag_abs, state)
  let sign = if negative { "-" } else { "+" }
  "\{real_str}\{sign}\{imag_str}i"
}

///|
fn vector_to_string_state(items : Array[Datum], state : PrinterState) -> String {
  let joined = items.map((item) => datum_to_string_state(item, state)).join(" ")
  "#(\{joined})"
}

///|
fn bytevector_to_string(items : Array[Int]) -> String {
  let joined = items.map((item) => item.to_string()).join(" ")
  "#vu8(\{joined})"
}

///|
fn datum_list_to_string_state(list : Datum, state : PrinterState) -> String {
  let parts : Array[String] = []
  for cur = list; true; {
    match cur {
      Datum::Nil => {
        let joined = parts.join(" ")
        break "(\{joined})"
      }
      Datum::Pair(a, b) => {
        parts.push(datum_to_string_state(a.val, state))
        continue b.val
      }
      _ => {
        let prefix = parts.join(" ")
        let tail = datum_to_string_state(cur, state)
        break "(\{prefix} . \{tail})"
      }
    }
  } else {
    ""
  }
}

///|
/// Render a value using Scheme write/display conventions.
///
/// # Example
/// ```mbt check
/// test "value to string" {
///   let value = @core.Value::Datum(@core.Datum::Int(7))
///   inspect(value_to_string(value), content="7")
/// }
/// ```
pub fn value_to_string(value : Value) -> String {
  match value {
    Value::Void => "#<void>"
    Value::Datum(d) => datum_to_string(d)
    Value::Primitive(_) |
    Value::Closure(_) |
    Value::CaseClosure(_) |
    Value::GuardHandler(_) |
    Value::Parameter(_) |
    Value::Continuation(_) |
    Value::RecordProc(_) |
    Value::ConditionProc(_) |
    Value::EnumSetProc(_) => "#<procedure>"
    Value::Values(_) => "#<values>"
    Value::Promise(_) => "#<promise>"
    Value::EvalEnv(_) => "#<environment>"
    Value::Port(_) => "#<port>"
    Value::Record(_) => "#<record>"
    Value::Hashtable(_) => "#<hashtable>"
    Value::EnumSet(_) => "#<enum-set>"
    Value::RecordTypeDescriptor(_) => "#<record-type-descriptor>"
    Value::RecordConstructorDescriptor(_) => "#<record-constructor-descriptor>"
    Value::SyntaxObject(_) | Value::Macro(_) => "#<syntax>"
    Value::SyntaxKeyword(_) => "#<syntax-keyword>"
  }
}
