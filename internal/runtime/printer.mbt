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
fn PrinterState::new() -> PrinterState {
  { labels: {} }
}

///|
fn PrinterState::label_flags(self : PrinterState, id : Int) -> Int {
  match self.labels.get(id) {
    Some(value) => value
    None => 0
  }
}

///|
fn PrinterState::set_label_flags(self : PrinterState, id : Int, flags : Int) -> Unit {
  self.labels[id] = flags
}

///|
fn char_to_string(ch : Char) -> String {
  match ch {
    ' ' => "#\\space"
    '\n' => "#\\newline"
    '\t' => "#\\tab"
    _ => {
      let buf = @buffer.new()
      buf..write_string("#\\")..write_char(ch)
      buf.to_string()
    }
  }
}

///|
fn datum_to_string_state(datum : @core.Datum, state : PrinterState) -> String {
  match datum {
    Label(id, cell) => {
      let flags = state.label_flags(id)
      let defined = (flags & 1) != 0
      let active = (flags & 2) != 0
      if defined || active {
        return "#\{id}#"
      }
      state.set_label_flags(id, 3)
      let body = datum_to_string_state(cell.val, state)
      state.set_label_flags(id, 1)
      "#\{id}=\{body}"
    }
    Nil => "()"
    Bool(true) => "#t"
    Bool(false) => "#f"
    Int(n) => n.to_string()
    BigInt(n) => n.to_string()
    Rat(n, d) => "\{n}/\{d}"
    BigRat(n, d) => "\{n.to_string()}/\{d.to_string()}"
    Float(f) => f.to_string()
    Complex(real, imag) =>
      complex_to_string_state(real.val, imag.val, state)
    Char(ch) => char_to_string(ch)
    String(s) => "\"\{escape_string(s.val)}\""
    Symbol(name) => name
    Pair(_, _) => datum_list_to_string_state(datum, state)
    Vector(items) => vector_to_string_state(items, state)
    ByteVector(items) => bytevector_to_string(items)
    Record(_) => "#<record>"
    Condition(_) => "#<condition>"
    Value(value) => value_to_string(value)
  }
}

///|
fn datum_to_string(datum : @core.Datum) -> String {
  let state = PrinterState::new()
  datum_to_string_state(datum, state)
}

///|
fn datum_is_negative(value : @core.Datum) -> Bool {
  match datum_unlabel(value) {
    Int(n) => n < 0
    BigInt(n) => n.compare_int(0) < 0
    Rat(n, _) => n < 0
    BigRat(n, _) => n.compare_int(0) < 0
    Float(f) => f < 0.0
    _ => false
  }
}

///|
fn datum_abs(value : @core.Datum) -> @core.Datum {
  match datum_unlabel(value) {
    Int(n) => Int(if n < 0 { -n } else { n })
    Rat(n, d) => Rat(if n < 0 { -n } else { n }, d)
    BigInt(n) =>
      if n.compare_int(0) < 0 {
        BigInt(-n)
      } else {
        BigInt(n)
      }
    BigRat(n, d) =>
      if n.compare_int(0) < 0 {
        BigRat(-n, d)
      } else {
        BigRat(n, d)
      }
    Float(f) => Float(if f < 0.0 { -f } else { f })
    _ => value
  }
}

///|
fn complex_to_string_state(
  real : @core.Datum,
  imag : @core.Datum,
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
fn vector_to_string_state(items : Array[@core.Datum], state : PrinterState) -> String {
  let joined = items.map((item) => datum_to_string_state(item, state)).join(" ")
  "#(\{joined})"
}

///|
fn bytevector_to_string(items : Array[Int]) -> String {
  let joined = items.map((item) => item.to_string()).join(" ")
  "#vu8(\{joined})"
}

///|
fn datum_list_to_string_state(list : @core.Datum, state : PrinterState) -> String {
  let parts : Array[String] = []
  for cur = list; true; {
    // invariant : parts.length() >= 0
    // TODO(decreases) : list length not explicit; possible bug
    // assert : parts.length() >= 0
    match cur {
      Nil => {
        let joined = parts.join(" ")
        break "(\{joined})"
      }
      Pair(a, b) => {
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
pub fn value_to_string(value : @core.Value) -> String {
  match value {
    Void => "#<void>"
    Datum(d) => datum_to_string(d)
    Primitive(_) |
    Closure(_) |
    CaseClosure(_) |
    GuardHandler(_) |
    Parameter(_) |
    Continuation(_) |
    RecordProc(_) |
    ConditionProc(_) |
    EnumSetProc(_) => "#<procedure>"
    Values(_) => "#<values>"
    Promise(_) => "#<promise>"
    EvalEnv(_) => "#<environment>"
    Port(_) => "#<port>"
    Record(_) => "#<record>"
    Hashtable(_) => "#<hashtable>"
    EnumSet(_) => "#<enum-set>"
    RecordTypeDescriptor(_) => "#<record-type-descriptor>"
    RecordConstructorDescriptor(_) => "#<record-constructor-descriptor>"
    SyntaxObject(_) | Macro(_) => "#<syntax>"
    SyntaxKeyword(_) => "#<syntax-keyword>"
  }
}
