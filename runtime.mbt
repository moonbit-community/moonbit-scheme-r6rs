///|
fn bool_value(value : Bool) -> Value {
  Value::Datum(Datum::Bool(value))
}

///|
priv enum PortKind {
  OutputString(Ref[String])
}

///|
struct Port {
  id : Int
  kind : PortKind
}

///|
let port_counter : Ref[Int] = Ref::new(0)

///|
fn next_port_id() -> Int {
  let id = port_counter.val
  port_counter.val = id + 1
  id
}

///|
fn new_output_string_port() -> Port {
  let buffer = Ref::new("")
  Port::{ id: next_port_id(), kind: PortKind::OutputString(buffer) }
}

///|
let current_output_port : Ref[Port] = Ref::new(new_output_string_port())

///|
fn get_current_output_port() -> Port {
  current_output_port.val
}

///|
fn set_current_output_port(port : Port) -> Unit {
  current_output_port.val = port
}

///|
fn port_write(port : Port, text : String) -> Unit {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val = buffer.val + text
  }
}

///|
fn port_get_output_string(port : Port) -> String {
  match port.kind {
    PortKind::OutputString(buffer) => buffer.val
  }
}

///|
fn is_false(value : Value) -> Bool {
  match value {
    Value::Datum(Datum::Bool(false)) => true
    _ => false
  }
}

///|
fn value_as_datum(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(d) => d
    _ => raise EvalError("type error: datum expected")
  }
}

///|
///|
fn int_abs(value : Int) -> Int {
  if value < 0 { -value } else { value }
}

///|
fn gcd(a : Int, b : Int) -> Int {
  let mut x = int_abs(a)
  let mut y = int_abs(b)
  while y != 0 {
    let temp = x % y
    x = y
    y = temp
  }
  x
}

///|
fn normalize_rat(num : Int, den : Int) -> Datum? {
  if den == 0 {
    None
  } else if num == 0 {
    Some(Datum::Int(0))
  } else {
    let mut n = num
    let mut d = den
    if d < 0 {
      n = -n
      d = -d
    }
    let g = gcd(n, d)
    let nn = n / g
    let dd = d / g
    if dd == 1 {
      Some(Datum::Int(nn))
    } else {
      Some(Datum::Rat(nn, dd))
    }
  }
}

///|
fn value_as_pair(value : Value) -> (Datum, Datum) raise EvalError {
  match value {
    Value::Datum(Datum::Pair(a, b)) => (a, b)
    _ => raise EvalError("type error: pair expected")
  }
}

///|
fn datum_list_to_array(list : Datum) -> Array[Datum] raise EvalError {
  let items : Array[Datum] = []
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(a)
        cur = b
      }
      _ => raise EvalError("type error: proper list expected")
    }
  }
  items
}

///|
fn datum_list_to_value_array(list : Datum) -> Array[Value] raise EvalError {
  let items : Array[Value] = []
  let mut cur = list
  while true {
    match cur {
      Datum::Nil => break
      Datum::Pair(a, b) => {
        items.push(Value::Datum(a))
        cur = b
      }
      _ => raise EvalError("type error: list expected")
    }
  }
  items
}

///|
fn list_from_array(items : Array[Datum]) -> Datum {
  let mut tail = Datum::Nil
  for item in items.rev_iter() {
    tail = Datum::Pair(item, tail)
  }
  tail
}

///|
fn symbol_name(expr : Datum) -> String? {
  match expr {
    Datum::Symbol(name) => Some(name)
    _ => None
  }
}
