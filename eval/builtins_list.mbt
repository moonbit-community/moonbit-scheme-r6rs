///|
/// @core.Primitive::Apply pair and list primitives.
fn apply_pair_list_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    @core.Primitive::Cons =>
      match args {
        [car_value, cdr_value] =>
          Some({
            let car = value_to_datum_element(car_value)
            let cdr = value_to_datum_element(cdr_value)
            @core.Value::Datum(pair_new(car, cdr))
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::SetCar =>
      match args {
        [pair_value, item_value] =>
          Some({
            let (car_cell, _) = value_as_pair_cells(pair_value)
            car_cell.val = value_to_datum_element(item_value)
            @core.Value::Void
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::SetCdr =>
      match args {
        [pair_value, item_value] =>
          Some({
            let (_, cdr_cell) = value_as_pair_cells(pair_value)
            cdr_cell.val = value_to_datum_element(item_value)
            @core.Value::Void
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Car =>
      match args {
        [pair_value] =>
          Some({
            let (car, _) = value_as_pair(pair_value)
            value_from_datum(car)
          })
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Cdr =>
      match args {
        [pair_value] =>
          Some({
            let (_, cdr) = value_as_pair(pair_value)
            value_from_datum(cdr)
          })
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Cxr(chain) =>
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
    @core.Primitive::List => {
      let items : Array[@core.Datum] =
        args.map((arg) => value_to_datum_element(arg))
      Some(@core.Value::Datum(list_from_array(items)))
    }
    @core.Primitive::MakeList =>
      match args {
        [len_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = @core.Datum::Nil
            let items : Array[@core.Datum] = []
            for i = 0; i < len; {
              // invariant : i >= 0 && i <= len
              // decreases : len - i
              // assert : i <= len
              items.push(fill)
              continue i + 1
            }
            @core.Value::Datum(list_from_array(items))
          })
        [len_value, fill_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = value_to_datum_element(fill_value)
            let items : Array[@core.Datum] = []
            for i = 0; i < len; {
              // invariant : i >= 0 && i <= len
              // decreases : len - i
              // assert : i <= len
              items.push(fill)
              continue i + 1
            }
            @core.Value::Datum(list_from_array(items))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    @core.Primitive::NullP =>
      match args {
        [Datum(Nil)] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::PairP =>
      match args {
        [Datum(Pair(_, _))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ListP =>
      match args {
        [Datum(d)] => Some(bool_value(is_proper_list(d)))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Length =>
      match args {
        [Datum(d)] =>
          Some({
            let items = datum_list_to_array(d)
            @core.Value::Datum(@core.Datum::Int(items.length()))
          })
        [_] => raise @core.EvalError("type error: list expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Append =>
      Some(
        match args {
          [] => @core.Value::Datum(@core.Datum::Nil)
          [single] => single
          _ => {
            let items : Array[@core.Datum] = []
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
            @core.Value::Datum(list_with_tail(items, tail))
          }
        },
      )
    @core.Primitive::Reverse =>
      match args {
        [Datum(datum)] =>
          Some({
            let items = datum_list_to_array(datum)
            let result = for i = 0, result = @core.Datum::Nil; i < items.length(); {
              // invariant : i >= 0 && i <= items.length()
              // decreases : items.length() - i
              // assert : i <= items.length()
              continue i + 1, pair_new(items[i], result)
            } else {
              result
            }
            @core.Value::Datum(result)
          })
        [_] => raise @core.EvalError("type error: list expected")
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ListRef =>
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
    @core.Primitive::ListTail =>
      match args {
        [list_value, idx_value] =>
          Some({
            let datum = value_as_datum(list_value)
            let idx = value_as_int_index(idx_value)
            let items = datum_list_to_array(datum)
            if idx < 0 || idx > items.length() {
              raise @core.EvalError("index out of range")
            }
            @core.Value::Datum(list_from_array(items.sub(start=idx).to_array()))
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::ListCopy =>
      match args {
        [list_value] =>
          Some({
            let datum = value_as_datum(list_value)
            let items = datum_list_to_array(datum)
            @core.Value::Datum(list_from_array(items))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::Member =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(EqualMode, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Memq =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(EqvMode, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Memv =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_member(EqvMode, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Assoc =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(EqualMode, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Assq =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(EqvMode, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::Assv =>
      match args {
        [item, list_value] =>
          Some({
            let list = value_as_datum(list_value)
            list_assoc(EqvMode, item, list)
          })
        _ => raise arity_mismatch(2, args.length())
      }
    _ => None
  }
}
