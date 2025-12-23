///|
fn bool_value(value : Bool) -> Value {
  Value::Datum(Datum::Bool(value))
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
fn value_as_int(value : Value) -> Int raise EvalError {
  match value {
    Value::Datum(Datum::Int(n)) => n
    _ => raise EvalError("type error: number expected")
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
