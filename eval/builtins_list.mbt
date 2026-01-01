///|
/// Apply pair and list primitives.
fn apply_pair_list_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Cons =>
      match args {
        [car_value, cdr_value] =>
          Some({
            let car = value_to_datum_element(car_value)
            let cdr = value_to_datum_element(cdr_value)
            Datum(pair_new(car, cdr))
          })
        _ => raise arity_mismatch(2, args.length())
      }
    SetCar =>
      match args {
        [pair_value, item_value] =>
          Some({
            let (car_cell, _) = value_as_pair_cells(pair_value)
            car_cell.val = value_to_datum_element(item_value)
            Void
          })
        _ => raise arity_mismatch(2, args.length())
      }
    SetCdr =>
      match args {
        [pair_value, item_value] =>
          Some({
            let (_, cdr_cell) = value_as_pair_cells(pair_value)
            cdr_cell.val = value_to_datum_element(item_value)
            Void
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Car =>
      match args {
        [pair_value] =>
          Some({
            let (car, _) = value_as_pair(pair_value)
            value_from_datum(car)
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Cdr =>
      match args {
        [pair_value] =>
          Some({
            let (_, cdr) = value_as_pair(pair_value)
            value_from_datum(cdr)
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Cxr(chain) =>
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
                Pair(car, cdr) =>
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
    List => {
      let items : Array[Datum] =
        args.map((arg) => value_to_datum_element(arg))
      Some(Value::Datum(list_from_array(items)))
    }
    MakeList =>
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
            Datum(list_from_array(items))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    NullP =>
      match args {
        [Datum(Nil)] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    PairP =>
      match args {
        [Datum(Pair(_, _))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    ListP =>
      match args {
        [Datum(d)] => Some(bool_value(is_proper_list(d)))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Length =>
      match args {
        [Datum(d)] =>
          Some({
            let items = datum_list_to_array(d)
            Datum(Int(items.length()))
          })
        [_] => raise @core.EvalError("type error: list expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Append =>
      Some(
        match args {
          [] => Datum(Nil)
          [single] => single
          _ => {
            let items : Array[Datum] = []
            for i = 0; i < args.length() - 1; {
              // invariant : i >= 0 && i <= args.length() - 1
              // decreases : args.length() - 1 - i
              // assert : i <= args.length() - 1
              let datum = match args[i] {
                Datum(d) => d
                _ => raise @core.EvalError("type error: list expected")
              }
              let part = datum_list_to_array(datum)
              items.push_iter(part.iter())
              continue i + 1
            }
            let tail = value_to_datum_element(args[args.length() - 1])
            Value::Datum(list_with_tail(items, tail))
          }
        },
      )
    Reverse =>
      match args {
        [Datum(datum)] =>
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
    ListRef =>
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
    ListTail =>
      match args {
        [list_value, idx_value] =>
          Some({
            let datum = value_as_datum(list_value)
            let idx = value_as_int_index(idx_value)
            let items = datum_list_to_array(datum)
            if idx < 0 || idx > items.length() {
              raise @core.EvalError("index out of range")
            }
            Datum(list_from_array(items.sub(start=idx).to_array()))
          })
        _ => raise arity_mismatch(2, args.length())
      }
    ListCopy =>
      match args {
        [list_value] =>
          Some({
            let datum = value_as_datum(list_value)
            let items = datum_list_to_array(datum)
            Datum(list_from_array(items))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Member =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(Equal, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Memq =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Memv =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Assoc =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(Equal, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Assq =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Assv =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(Eqv, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    _ => None
  }
}
