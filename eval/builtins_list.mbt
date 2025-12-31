///|
/// Apply pair and list primitives.
fn apply_pair_list_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::Cons =>
      match args.length() {
        2 =>
          Some({
            let car = value_to_datum_element(args[0])
            let cdr = value_to_datum_element(args[1])
            Value::Datum(pair_new(car, cdr))
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::SetCar =>
      match args.length() {
        2 =>
          Some({
            let (car_cell, _) = value_as_pair_cells(args[0])
            car_cell.val = value_to_datum_element(args[1])
            Value::Void
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::SetCdr =>
      match args.length() {
        2 =>
          Some({
            let (_, cdr_cell) = value_as_pair_cells(args[0])
            cdr_cell.val = value_to_datum_element(args[1])
            Value::Void
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::Car =>
      match args.length() {
        1 =>
          Some({
            let (car, _) = value_as_pair(args[0])
            value_from_datum(car)
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::Cdr =>
      match args.length() {
        1 =>
          Some({
            let (_, cdr) = value_as_pair(args[0])
            value_from_datum(cdr)
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::Cxr(chain) =>
      match args.length() {
        1 =>
          Some({
            let cur = value_as_datum(args[0])
            let cur = for i = chain.length(), cur = cur; i > 0; {
              // invariant : i >= 0 && i <= chain.length()
              // decreases : i
              // assert : i <= chain.length()
              let idx = i - 1
              let op = match chain.get_char(idx) {
                Some(ch) => ch
                None => raise @core.EvalError("internal error: invalid cxr")
              }
              let next = match cur {
                Datum::Pair(car, cdr) =>
                  match op {
                    'a' => car.val
                    'd' => cdr.val
                    _ => raise @core.EvalError("internal error: invalid cxr")
                  }
                _ => raise @core.EvalError("type error: pair expected")
              }
              continue i - 1, next
            } else {
              cur
            }
            value_from_datum(cur)
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::List => {
      let items : Array[Datum] = []
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        items.push(value_to_datum_element(arg))
      }
      Some(Value::Datum(list_from_array(items)))
    }
    Primitive::MakeList =>
      match args.length() {
        1 | 2 =>
          Some({
            let len = value_as_nonnegative_int(args[0])
            let fill = if args.length() == 2 {
              value_to_datum_element(args[1])
            } else {
              Datum::Nil
            }
            let items : Array[Datum] = []
            for i = 0; i < len; {
              // invariant : i >= 0 && i <= len
              // decreases : len - i
              // assert : i <= len
              items.push(fill)
              continue i + 1
            }
            Value::Datum(list_from_array(items))
          })
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::NullP =>
      match args {
        [Value::Datum(Datum::Nil)] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::PairP =>
      match args {
        [Value::Datum(Datum::Pair(_, _))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ListP =>
      match args {
        [Value::Datum(d)] => Some(bool_value(is_proper_list(d)))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::Length =>
      match args {
        [Value::Datum(d)] =>
          Some({
            let items = datum_list_to_array(d)
            Value::Datum(Datum::Int(items.length()))
          })
        [_] => raise @core.EvalError("type error: list expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::Append =>
      Some(
        match args.length() {
          0 => Value::Datum(Datum::Nil)
          1 => args[0]
          _ => {
            let items : Array[Datum] = []
            for i = 0; i < args.length() - 1; {
              // invariant : i >= 0 && i <= args.length() - 1
              // decreases : args.length() - 1 - i
              // assert : i <= args.length() - 1
              let datum = match args[i] {
                Value::Datum(d) => d
                _ => raise @core.EvalError("type error: list expected")
              }
              let part = datum_list_to_array(datum)
              for item in part {
                // invariant : part.length() >= 0
                // TODO(decreases) : loop index not exposed; possible bug
                // assert : part.length() >= 0
                items.push(item)
              }
              continue i + 1
            }
            let tail = value_to_datum_element(args[args.length() - 1])
            Value::Datum(list_with_tail(items, tail))
          }
        },
      )
    Primitive::Reverse =>
      match args {
        [Value::Datum(datum)] =>
          Some({
            let items = datum_list_to_array(datum)
            let result = for i = 0, result = Datum::Nil; i < items.length(); {
              // invariant : i >= 0 && i <= items.length()
              // decreases : items.length() - i
              // assert : i <= items.length()
              continue i + 1, pair_new(items[i], result)
            } else {
              result
            }
            Value::Datum(result)
          })
        [_] => raise @core.EvalError("type error: list expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ListRef =>
      match args.length() {
        2 =>
          Some({
            let datum = value_as_datum(args[0])
            let idx = value_as_int_index(args[1])
            let items = datum_list_to_array(datum)
            check_index(idx, items.length())
            value_from_datum(items[idx])
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::ListTail =>
      match args.length() {
        2 =>
          Some({
            let datum = value_as_datum(args[0])
            let idx = value_as_int_index(args[1])
            let items = datum_list_to_array(datum)
            if idx < 0 || idx > items.length() {
              raise @core.EvalError("index out of range")
            }
            Value::Datum(list_from_array(items.sub(start=idx).to_array()))
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::ListCopy =>
      match args.length() {
        1 =>
          Some({
            let datum = value_as_datum(args[0])
            let items = datum_list_to_array(datum)
            Value::Datum(list_from_array(items))
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::Member =>
      match args.length() {
        2 =>
          Some({
            let list = value_as_datum(args[1])
            list_member(EqualityMode::Equal, args[0], list)
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::Memq =>
      match args.length() {
        2 =>
          Some({
            let list = value_as_datum(args[1])
            list_member(EqualityMode::Eqv, args[0], list)
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::Memv =>
      match args.length() {
        2 =>
          Some({
            let list = value_as_datum(args[1])
            list_member(EqualityMode::Eqv, args[0], list)
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::Assoc =>
      match args.length() {
        2 =>
          Some({
            let list = value_as_datum(args[1])
            list_assoc(EqualityMode::Equal, args[0], list)
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::Assq =>
      match args.length() {
        2 =>
          Some({
            let list = value_as_datum(args[1])
            list_assoc(EqualityMode::Eqv, args[0], list)
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::Assv =>
      match args.length() {
        2 =>
          Some({
            let list = value_as_datum(args[1])
            list_assoc(EqualityMode::Eqv, args[0], list)
          })
        n => raise arity_mismatch(2, n)
      }
    _ => None
  }
}
