///|
/// Apply pair and list primitives.
fn apply_pair_list_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::Cons =>
      match args {
        [car_value, cdr_value] =>
          Some({
            let car = value_to_datum_element(car_value)
            let cdr = value_to_datum_element(cdr_value)
            Value::Datum(pair_new(car, cdr))
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::SetCar =>
      match args {
        [pair_value, item_value] =>
          Some({
            let (car_cell, _) = value_as_pair_cells(pair_value)
            car_cell.val = value_to_datum_element(item_value)
            Value::Void
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::SetCdr =>
      match args {
        [pair_value, item_value] =>
          Some({
            let (_, cdr_cell) = value_as_pair_cells(pair_value)
            cdr_cell.val = value_to_datum_element(item_value)
            Value::Void
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Car =>
      match args {
        [pair_value] =>
          Some({
            let (car, _) = value_as_pair(pair_value)
            value_from_datum(car)
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::Cdr =>
      match args {
        [pair_value] =>
          Some({
            let (_, cdr) = value_as_pair(pair_value)
            value_from_datum(cdr)
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::Cxr(chain) =>
      match args {
        [datum_value] =>
          Some({
            let cur = value_as_datum(datum_value)
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
        _ => raise arity_mismatch(1, args.length())
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
      match args {
        [len_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = Datum::Nil
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
        [len_value, fill_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = value_to_datum_element(fill_value)
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
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
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
        match args {
          [] => Value::Datum(Datum::Nil)
          [single] => single
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
      match args {
        [list_value, idx_value] =>
          Some({
            let datum = value_as_datum(list_value)
            let idx = value_as_int_index(idx_value)
            let items = datum_list_to_array(datum)
            check_index(idx, items.length())
            value_from_datum(items[idx])
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::ListTail =>
      match args {
        [list_value, idx_value] =>
          Some({
            let datum = value_as_datum(list_value)
            let idx = value_as_int_index(idx_value)
            let items = datum_list_to_array(datum)
            if idx < 0 || idx > items.length() {
              raise @core.EvalError("index out of range")
            }
            Value::Datum(list_from_array(items.sub(start=idx).to_array()))
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::ListCopy =>
      match args {
        [list_value] =>
          Some({
            let datum = value_as_datum(list_value)
            let items = datum_list_to_array(datum)
            Value::Datum(list_from_array(items))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::Member =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(EqualityMode::Equal, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Memq =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(EqualityMode::Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Memv =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(EqualityMode::Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Assoc =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(EqualityMode::Equal, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Assq =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(EqualityMode::Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::Assv =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(EqualityMode::Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    _ => None
  }
}
