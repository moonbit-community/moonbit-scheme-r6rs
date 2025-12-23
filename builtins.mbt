///|
fn arity_mismatch(expected : Int, got : Int) -> EvalError {
  EvalError("arity mismatch: expected \{expected} got \{got}")
}

///|
fn is_proper_list(value : Datum) -> Bool {
  let mut cur = value
  while true {
    match cur {
      Datum::Nil => return true
      Datum::Pair(_, cdr) => cur = cdr
      _ => return false
    }
  }
  false
}

///|
fn primitive_eq(left : Primitive, right : Primitive) -> Bool {
  match (left, right) {
    (Primitive::Add, Primitive::Add)
    | (Primitive::Sub, Primitive::Sub)
    | (Primitive::Mul, Primitive::Mul)
    | (Primitive::Div, Primitive::Div)
    | (Primitive::NumEq, Primitive::NumEq)
    | (Primitive::Less, Primitive::Less)
    | (Primitive::Greater, Primitive::Greater)
    | (Primitive::LessEq, Primitive::LessEq)
    | (Primitive::GreaterEq, Primitive::GreaterEq)
    | (Primitive::Eq, Primitive::Eq)
    | (Primitive::Eqv, Primitive::Eqv)
    | (Primitive::Equal, Primitive::Equal)
    | (Primitive::Cons, Primitive::Cons)
    | (Primitive::Car, Primitive::Car)
    | (Primitive::Cdr, Primitive::Cdr)
    | (Primitive::List, Primitive::List)
    | (Primitive::NullP, Primitive::NullP)
    | (Primitive::PairP, Primitive::PairP)
    | (Primitive::SymbolP, Primitive::SymbolP)
    | (Primitive::BooleanP, Primitive::BooleanP)
    | (Primitive::NumberP, Primitive::NumberP)
    | (Primitive::IntegerP, Primitive::IntegerP)
    | (Primitive::RationalP, Primitive::RationalP)
    | (Primitive::RealP, Primitive::RealP)
    | (Primitive::ExactP, Primitive::ExactP)
    | (Primitive::InexactP, Primitive::InexactP)
    | (Primitive::ProcedureP, Primitive::ProcedureP)
    | (Primitive::Not, Primitive::Not)
    | (Primitive::Apply, Primitive::Apply)
    | (Primitive::CallCC, Primitive::CallCC)
    | (Primitive::Values, Primitive::Values)
    | (Primitive::CallWithValues, Primitive::CallWithValues)
    | (Primitive::MakeParameter, Primitive::MakeParameter)
    | (Primitive::DynamicWind, Primitive::DynamicWind)
    | (Primitive::ExactToInexact, Primitive::ExactToInexact)
    | (Primitive::InexactToExact, Primitive::InexactToExact)
    | (Primitive::Numerator, Primitive::Numerator)
    | (Primitive::Denominator, Primitive::Denominator)
    | (Primitive::ListP, Primitive::ListP)
    | (Primitive::MakeList, Primitive::MakeList)
    | (Primitive::Length, Primitive::Length)
    | (Primitive::Append, Primitive::Append)
    | (Primitive::Reverse, Primitive::Reverse)
    | (Primitive::ListRef, Primitive::ListRef)
    | (Primitive::ListTail, Primitive::ListTail)
    | (Primitive::CharP, Primitive::CharP)
    | (Primitive::StringP, Primitive::StringP)
    | (Primitive::StringLength, Primitive::StringLength)
    | (Primitive::StringAppend, Primitive::StringAppend)
    | (Primitive::StringRef, Primitive::StringRef)
    | (Primitive::StringToList, Primitive::StringToList)
    | (Primitive::ListToString, Primitive::ListToString)
    | (Primitive::Vector, Primitive::Vector)
    | (Primitive::MakeVector, Primitive::MakeVector)
    | (Primitive::VectorP, Primitive::VectorP)
    | (Primitive::VectorLength, Primitive::VectorLength)
    | (Primitive::VectorRef, Primitive::VectorRef)
    | (Primitive::VectorSet, Primitive::VectorSet)
    | (Primitive::VectorToList, Primitive::VectorToList)
    | (Primitive::ListToVector, Primitive::ListToVector)
    | (Primitive::ByteVector, Primitive::ByteVector)
    | (Primitive::ByteVectorP, Primitive::ByteVectorP)
    | (Primitive::ByteVectorLength, Primitive::ByteVectorLength)
    | (Primitive::ByteVectorU8Ref, Primitive::ByteVectorU8Ref)
    | (Primitive::ByteVectorU8Set, Primitive::ByteVectorU8Set)
    | (Primitive::Display, Primitive::Display)
    | (Primitive::Write, Primitive::Write)
    | (Primitive::Newline, Primitive::Newline)
    | (Primitive::OpenOutputString, Primitive::OpenOutputString)
    | (Primitive::GetOutputString, Primitive::GetOutputString)
    | (Primitive::CurrentOutputPort, Primitive::CurrentOutputPort)
    | (Primitive::WithExceptionHandler, Primitive::WithExceptionHandler)
    | (Primitive::Raise, Primitive::Raise)
    | (Primitive::RaiseContinuable, Primitive::RaiseContinuable) => true
    _ => false
  }
}

///|
fn value_eqv(left : Value, right : Value) -> Bool {
  match (left, right) {
    (Value::Datum(a), Value::Datum(b)) => datum_equal(a, b)
    (Value::Primitive(a), Value::Primitive(b)) => primitive_eq(a, b)
    (Value::Closure(a), Value::Closure(b)) => a.id == b.id
    (Value::CaseClosure(a), Value::CaseClosure(b)) => a.id == b.id
    (Value::GuardHandler(a), Value::GuardHandler(b)) => a.id == b.id
    (Value::Parameter(a), Value::Parameter(b)) => a.id == b.id
    (Value::Continuation(a), Value::Continuation(b)) => a.id == b.id
    (Value::Port(a), Value::Port(b)) => a.id == b.id
    (Value::Record(a), Value::Record(b)) => a.id == b.id
    (Value::RecordProc(a), Value::RecordProc(b)) => a.id == b.id
    (Value::Void, Value::Void) => true
    _ => false
  }
}

///|
fn value_equal(left : Value, right : Value) -> Bool {
  value_eqv(left, right)
}

///|
fn value_as_number(value : Value) -> Datum raise EvalError {
  match value {
    Value::Datum(d) => {
      match d {
        Datum::Int(_) | Datum::Rat(_, _) | Datum::Float(_) => d
        _ => raise EvalError("type error: number expected")
      }
    }
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn number_to_float(value : Datum) -> Float raise EvalError {
  match value {
    Datum::Int(n) => Float::from_int(n)
    Datum::Rat(n, d) => Float::from_int(n) / Float::from_int(d)
    Datum::Float(f) => f
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn number_to_rat(value : Datum) -> (Int, Int) raise EvalError {
  match value {
    Datum::Int(n) => (n, 1)
    Datum::Rat(n, d) => (n, d)
    _ => raise EvalError("type error: number expected")
  }
}

///|
fn normalize_rat_or_error(num : Int, den : Int) -> Datum raise EvalError {
  match normalize_rat(num, den) {
    Some(result) => result
    None => raise EvalError("division by zero")
  }
}

///|
fn num_add(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) + number_to_float(right))
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * d2 + n2 * d1, d1 * d2)
  }
}

///|
fn num_sub(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) - number_to_float(right))
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * d2 - n2 * d1, d1 * d2)
  }
}

///|
fn num_mul(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Float(_) || right is Datum::Float(_) {
    Datum::Float(number_to_float(left) * number_to_float(right))
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    normalize_rat_or_error(n1 * n2, d1 * d2)
  }
}

///|
fn num_div(left : Datum, right : Datum) -> Datum raise EvalError {
  if left is Datum::Float(_) || right is Datum::Float(_) {
    let denom = number_to_float(right)
    if denom == 0.0 {
      raise EvalError("division by zero")
    }
    Datum::Float(number_to_float(left) / denom)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    if n2 == 0 {
      raise EvalError("division by zero")
    }
    normalize_rat_or_error(n1 * d2, d1 * n2)
  }
}

///|
fn num_equal(left : Datum, right : Datum) -> Bool raise EvalError {
  if left is Datum::Float(_) || right is Datum::Float(_) {
    number_to_float(left) == number_to_float(right)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    n1 * d2 == n2 * d1
  }
}

///|
fn num_less(left : Datum, right : Datum) -> Bool raise EvalError {
  if left is Datum::Float(_) || right is Datum::Float(_) {
    number_to_float(left) < number_to_float(right)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    n1 * d2 < n2 * d1
  }
}

///|
fn num_greater(left : Datum, right : Datum) -> Bool raise EvalError {
  if left is Datum::Float(_) || right is Datum::Float(_) {
    number_to_float(left) > number_to_float(right)
  } else {
    let (n1, d1) = number_to_rat(left)
    let (n2, d2) = number_to_rat(right)
    n1 * d2 > n2 * d1
  }
}

///|
fn num_less_eq(left : Datum, right : Datum) -> Bool raise EvalError {
  num_less(left, right) || num_equal(left, right)
}

///|
fn num_greater_eq(left : Datum, right : Datum) -> Bool raise EvalError {
  num_greater(left, right) || num_equal(left, right)
}

///|
fn value_as_string(value : Value) -> String raise EvalError {
  match value {
    Value::Datum(Datum::String(s)) => s
    _ => raise EvalError("type error: string expected")
  }
}

///|
fn value_as_int_index(value : Value) -> Int raise EvalError {
  match value {
    Value::Datum(Datum::Int(n)) => n
    _ => raise EvalError("type error: exact integer expected")
  }
}

///|
fn value_as_nonnegative_int(value : Value) -> Int raise EvalError {
  let n = value_as_int_index(value)
  if n < 0 {
    raise EvalError("type error: exact nonnegative integer expected")
  }
  n
}

///|
fn check_index(idx : Int, len : Int) -> Unit raise EvalError {
  if idx < 0 || idx >= len {
    raise EvalError("index out of range")
  }
}

///|
fn value_as_vector(value : Value) -> Array[Datum] raise EvalError {
  match value {
    Value::Datum(Datum::Vector(items)) => items
    _ => raise EvalError("type error: vector expected")
  }
}

///|
fn value_as_bytevector(value : Value) -> Array[Int] raise EvalError {
  match value {
    Value::Datum(Datum::ByteVector(items)) => items
    _ => raise EvalError("type error: bytevector expected")
  }
}

///|
fn value_as_byte(value : Value) -> Int raise EvalError {
  match value {
    Value::Datum(Datum::Int(n)) => {
      if n < 0 || n > 255 {
        raise EvalError("type error: byte expected")
      }
      n
    }
    _ => raise EvalError("type error: byte expected")
  }
}

///|
fn value_as_port(value : Value) -> Port raise EvalError {
  match value {
    Value::Port(port) => port
    _ => raise EvalError("type error: port expected")
  }
}

///|
fn char_to_raw_string(ch : Char) -> String {
  let buf = @buffer.new()
  buf.write_char(ch)
  buf.to_string()
}

///|
fn display_string(value : Value) -> String {
  match value {
    Value::Datum(Datum::String(s)) => s
    Value::Datum(Datum::Char(ch)) => char_to_raw_string(ch)
    Value::Datum(Datum::Symbol(name)) => name
    _ => value_to_string(value)
  }
}

///|
fn apply_primitive(prim : Primitive, args : Array[Value]) -> Value raise EvalError {
  match prim {
    Primitive::Add => {
      let mut acc = Datum::Int(0)
      for arg in args {
        acc = num_add(acc, value_as_number(arg))
      }
      Value::Datum(acc)
    }
    Primitive::Sub => {
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let value = value_as_number(args[0])
          Value::Datum(num_sub(Datum::Int(0), value))
        }
        _ => {
          let mut result = value_as_number(args[0])
          let mut i = 1
          while i < args.length() {
            result = num_sub(result, value_as_number(args[i]))
            i = i + 1
          }
          Value::Datum(result)
        }
      }
    }
    Primitive::Mul => {
      let mut product = Datum::Int(1)
      for arg in args {
        product = num_mul(product, value_as_number(arg))
      }
      Value::Datum(product)
    }
    Primitive::Div => {
      match args.length() {
        0 => raise arity_mismatch(1, 0)
        1 => {
          let value = value_as_number(args[0])
          Value::Datum(num_div(Datum::Int(1), value))
        }
        _ => {
          let mut result = value_as_number(args[0])
          let mut i = 1
          while i < args.length() {
            result = num_div(result, value_as_number(args[i]))
            i = i + 1
          }
          Value::Datum(result)
        }
      }
    }
    Primitive::NumEq => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let first = value_as_number(args[0])
        let mut ok = true
        let mut i = 1
        while i < args.length() {
          if !num_equal(value_as_number(args[i]), first) {
            ok = false
            break
          } else {
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::Less => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_less(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::Greater => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_greater(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::LessEq => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_less_eq(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::GreaterEq => {
      if args.length() <= 1 {
        bool_value(true)
      } else {
        let mut ok = true
        let mut prev = value_as_number(args[0])
        let mut i = 1
        while i < args.length() {
          let cur = value_as_number(args[i])
          if !num_greater_eq(prev, cur) {
            ok = false
            break
          } else {
            prev = cur
            i = i + 1
          }
        }
        bool_value(ok)
      }
    }
    Primitive::ExactToInexact => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(_)) => args[0]
            _ => {
              let value = value_as_number(args[0])
              Value::Datum(Datum::Float(number_to_float(value)))
            }
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::InexactToExact => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(f)) => {
              let n = f.to_int()
              if f == Float::from_int(n) {
                Value::Datum(Datum::Int(n))
              } else {
                raise EvalError("inexact->exact: unsupported")
              }
            }
            Value::Datum(Datum::Int(_)) | Value::Datum(Datum::Rat(_, _)) => args[0]
            _ => raise EvalError("type error: number expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Numerator => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(n)) => Value::Datum(Datum::Int(n))
            Value::Datum(Datum::Rat(n, _)) => Value::Datum(Datum::Int(n))
            _ => raise EvalError("type error: rational expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Denominator => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_)) => Value::Datum(Datum::Int(1))
            Value::Datum(Datum::Rat(_, d)) => Value::Datum(Datum::Int(d))
            _ => raise EvalError("type error: rational expected")
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Eq => {
      match args.length() {
        2 => bool_value(value_eqv(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Eqv => {
      match args.length() {
        2 => bool_value(value_eqv(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Equal => {
      match args.length() {
        2 => bool_value(value_equal(args[0], args[1]))
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Cons => {
      match args.length() {
        2 => {
          let car = value_as_datum(args[0])
          let cdr = value_as_datum(args[1])
          Value::Datum(Datum::Pair(car, cdr))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::Car => {
      match args.length() {
        1 => {
          let (car, _) = value_as_pair(args[0])
          Value::Datum(car)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Cdr => {
      match args.length() {
        1 => {
          let (_, cdr) = value_as_pair(args[0])
          Value::Datum(cdr)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::List => {
      let items : Array[Datum] = []
      for arg in args {
        items.push(value_as_datum(arg))
      }
      Value::Datum(list_from_array(items))
    }
    Primitive::MakeList => {
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 {
            value_as_datum(args[1])
          } else {
            Datum::Nil
          }
          let items : Array[Datum] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::NullP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Nil) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::PairP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Pair(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::SymbolP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Symbol(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::BooleanP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Bool(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::NumberP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::Float(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::IntegerP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RationalP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_)) | Value::Datum(Datum::Rat(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::RealP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_))
            | Value::Datum(Datum::Rat(_, _))
            | Value::Datum(Datum::Float(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ExactP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Int(_)) | Value::Datum(Datum::Rat(_, _)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::InexactP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Float(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ProcedureP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Primitive(_)
            | Value::Closure(_)
            | Value::CaseClosure(_)
            | Value::GuardHandler(_)
            | Value::Parameter(_)
            | Value::Continuation(_)
            | Value::RecordProc(_) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CharP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Char(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::String(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringLength => {
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          Value::Datum(Datum::Int(s.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::StringAppend => {
      let buf = @buffer.new()
      for arg in args {
        buf.write_string(value_as_string(arg))
      }
      Value::Datum(Datum::String(buf.to_string()))
    }
    Primitive::StringRef => {
      match args.length() {
        2 => {
          let s = value_as_string(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, s.length())
          match s.get_char(idx) {
            Some(ch) => Value::Datum(Datum::Char(ch))
            None => raise EvalError("index out of range")
          }
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::StringToList => {
      match args.length() {
        1 => {
          let s = value_as_string(args[0])
          let items : Array[Datum] = []
          let mut i = 0
          while i < s.length() {
            match s.get_char(i) {
              Some(ch) => items.push(Datum::Char(ch))
              None => raise EvalError("index out of range")
            }
            i = i + 1
          }
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ListToString => {
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          let buf = @buffer.new()
          for item in items {
            match item {
              Datum::Char(ch) => buf.write_char(ch)
              _ => raise EvalError("type error: char expected")
            }
          }
          Value::Datum(Datum::String(buf.to_string()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Vector => {
      let items : Array[Datum] = []
      for arg in args {
        items.push(value_as_datum(arg))
      }
      Value::Datum(Datum::Vector(items))
    }
    Primitive::MakeVector => {
      match args.length() {
        1 | 2 => {
          let len = value_as_nonnegative_int(args[0])
          let fill = if args.length() == 2 {
            value_as_datum(args[1])
          } else {
            Datum::Nil
          }
          let items : Array[Datum] = []
          let mut i = 0
          while i < len {
            items.push(fill)
            i = i + 1
          }
          Value::Datum(Datum::Vector(items))
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::VectorP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::Vector(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::VectorLength => {
      match args.length() {
        1 => {
          let items = value_as_vector(args[0])
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::VectorRef => {
      match args.length() {
        2 => {
          let items = value_as_vector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          Value::Datum(items[idx])
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::VectorSet => {
      match args.length() {
        3 => {
          let items = value_as_vector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          items[idx] = value_as_datum(args[2])
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    }
    Primitive::VectorToList => {
      match args.length() {
        1 => {
          let items = value_as_vector(args[0])
          Value::Datum(list_from_array(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ListToVector => {
      match args.length() {
        1 => {
          let datum = value_as_datum(args[0])
          let items = datum_list_to_array(datum)
          Value::Datum(Datum::Vector(items))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ByteVector => {
      let items : Array[Int] = []
      for arg in args {
        items.push(value_as_byte(arg))
      }
      Value::Datum(Datum::ByteVector(items))
    }
    Primitive::ByteVectorP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(Datum::ByteVector(_)) => bool_value(true)
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ByteVectorLength => {
      match args.length() {
        1 => {
          let items = value_as_bytevector(args[0])
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ByteVectorU8Ref => {
      match args.length() {
        2 => {
          let items = value_as_bytevector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          Value::Datum(Datum::Int(items[idx]))
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::ByteVectorU8Set => {
      match args.length() {
        3 => {
          let items = value_as_bytevector(args[0])
          let idx = value_as_int_index(args[1])
          check_index(idx, items.length())
          items[idx] = value_as_byte(args[2])
          Value::Void
        }
        n => raise arity_mismatch(3, n)
      }
    }
    Primitive::Display => {
      match args.length() {
        1 => {
          let port = get_current_output_port()
          port_write(port, display_string(args[0]))
          Value::Void
        }
        2 => {
          let port = value_as_port(args[1])
          port_write(port, display_string(args[0]))
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::Write => {
      match args.length() {
        1 => {
          let port = get_current_output_port()
          port_write(port, value_to_string(args[0]))
          Value::Void
        }
        2 => {
          let port = value_as_port(args[1])
          port_write(port, value_to_string(args[0]))
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    }
    Primitive::Newline => {
      match args.length() {
        0 => {
          let port = get_current_output_port()
          port_write(port, "\n")
          Value::Void
        }
        1 => {
          let port = value_as_port(args[0])
          port_write(port, "\n")
          Value::Void
        }
        n => raise EvalError("arity mismatch: expected 0 or 1 got \{n}")
      }
    }
    Primitive::OpenOutputString => {
      match args.length() {
        0 => Value::Port(new_output_string_port())
        n => raise arity_mismatch(0, n)
      }
    }
    Primitive::GetOutputString => {
      match args.length() {
        1 => {
          let port = value_as_port(args[0])
          Value::Datum(Datum::String(port_get_output_string(port)))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::CurrentOutputPort => {
      match args.length() {
        0 => Value::Port(get_current_output_port())
        n => raise arity_mismatch(0, n)
      }
    }
    Primitive::WithExceptionHandler => {
      raise EvalError("internal error: with-exception-handler should be handled in apply_proc")
    }
    Primitive::Raise => {
      raise EvalError("internal error: raise should be handled in apply_proc")
    }
    Primitive::RaiseContinuable => {
      raise EvalError("internal error: raise-continuable should be handled in apply_proc")
    }
    Primitive::Not => {
      match args.length() {
        1 => bool_value(is_false(args[0]))
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Apply => {
      // handled by apply_proc to allow proper argument flattening
      raise EvalError("internal error: apply should be handled in apply_proc")
    }
    Primitive::CallCC => {
      raise EvalError("internal error: call/cc should be handled in apply_proc")
    }
    Primitive::Values => Value::Values(args)
    Primitive::CallWithValues => {
      raise EvalError("internal error: call-with-values should be handled in apply_proc")
    }
    Primitive::MakeParameter => {
      raise EvalError("internal error: make-parameter should be handled in apply_proc")
    }
    Primitive::DynamicWind => {
      raise EvalError("internal error: dynamic-wind should be handled in apply_proc")
    }
    Primitive::ListP => {
      match args.length() {
        1 => {
          match args[0] {
            Value::Datum(d) => bool_value(is_proper_list(d))
            _ => bool_value(false)
          }
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Length => {
      match args.length() {
        1 => {
          let datum = match args[0] {
            Value::Datum(d) => d
            _ => raise EvalError("type error: list expected")
          }
          let items = datum_list_to_array(datum)
          Value::Datum(Datum::Int(items.length()))
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::Append => {
      match args.length() {
        0 => Value::Datum(Datum::Nil)
        1 => args[0]
        _ => {
          let items : Array[Datum] = []
          let mut i = 0
          while i < args.length() - 1 {
            let datum = match args[i] {
              Value::Datum(d) => d
              _ => raise EvalError("type error: list expected")
            }
            let part = datum_list_to_array(datum)
            for item in part {
              items.push(item)
            }
            i = i + 1
          }
          let tail = value_as_datum(args[args.length() - 1])
          Value::Datum(list_with_tail(items, tail))
        }
      }
    }
    Primitive::Reverse => {
      match args.length() {
        1 => {
          let datum = match args[0] {
            Value::Datum(d) => d
            _ => raise EvalError("type error: list expected")
          }
          let items = datum_list_to_array(datum)
          let mut result = Datum::Nil
          for item in items {
            result = Datum::Pair(item, result)
          }
          Value::Datum(result)
        }
        n => raise arity_mismatch(1, n)
      }
    }
    Primitive::ListRef => {
      match args.length() {
        2 => {
          let datum = value_as_datum(args[0])
          let idx = value_as_int_index(args[1])
          let items = datum_list_to_array(datum)
          check_index(idx, items.length())
          Value::Datum(items[idx])
        }
        n => raise arity_mismatch(2, n)
      }
    }
    Primitive::ListTail => {
      match args.length() {
        2 => {
          let datum = value_as_datum(args[0])
          let idx = value_as_int_index(args[1])
          let items = datum_list_to_array(datum)
          if idx < 0 || idx > items.length() {
            raise EvalError("index out of range")
          }
          Value::Datum(list_from_array(items.sub(start=idx).to_array()))
        }
        n => raise arity_mismatch(2, n)
      }
    }
  }
}
