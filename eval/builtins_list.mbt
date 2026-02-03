///|
/// Primitive::Apply pair and list primitives.
fn is_pair_list_primitive(prim : @core.Primitive) -> Bool {
  match prim {
    Cons
    | SetCar
    | SetCdr
    | Car
    | Cdr
    | Cxr(_)
    | List
    | MakeList
    | NullP
    | PairP
    | ListP
    | Length
    | Append
    | Reverse
    | ListRef
    | ListTail
    | ListCopy
    | Member
    | Memq
    | Memv
    | Assoc
    | Assq
    | Assv => true
    _ => false
  }
}

///|
/// Primitive::Apply pair and list primitives.
fn apply_pair_list_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value raise @core.EvalError {
  match prim {
    Cons =>
      match args {
        [car_value, cdr_value] => {
          let car = value_to_datum_element(car_value)
          let cdr = value_to_datum_element(cdr_value)
          Datum(pair_new(car, cdr))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    SetCar =>
      match args {
        [pair_value, item_value] => {
          let (car_cell, _) = value_as_pair_cells(pair_value)
          car_cell.val = value_to_datum_element(item_value)
          Void
        }
        _ => raise arity_mismatch(2, args.length())
      }
    SetCdr =>
      match args {
        [pair_value, item_value] => {
          let (_, cdr_cell) = value_as_pair_cells(pair_value)
          cdr_cell.val = value_to_datum_element(item_value)
          Void
        }
        _ => raise arity_mismatch(2, args.length())
      }
    Car =>
      match args {
        [pair_value] => {
          let (car, _) = value_as_pair(pair_value)
          value_from_datum(car)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Cdr =>
      match args {
        [pair_value] => {
          let (_, cdr) = value_as_pair(pair_value)
          value_from_datum(cdr)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Cxr(chain) =>
      match args {
        [datum_value] => {
          let cur = value_as_datum(datum_value)
          let cur = for i = chain.length(), cur = cur; i > 0; {
            // invariant : i >= 0 && i <= chain.length()
            // decreases : i
            // assert : i <= chain.length()
            let idx = i - 1
            let op = match chain.get_char(idx) {
              Some(ch) => ch
              None => raise EvalError("internal error: invalid cxr")
            }
            let next = match cur {
              Pair(car, cdr) =>
                match op {
                  'a' => car.val
                  'd' => cdr.val
                  _ => raise EvalError("internal error: invalid cxr")
                }
              _ => raise EvalError("type error: pair expected")
            }
            continue i - 1, next
          } else {
            cur
          }
          value_from_datum(cur)
        }
        _ => raise arity_mismatch(1, args.length())
      }
    List => {
      let items : Array[@core.Datum] = args.map(arg => value_to_datum_element(
        arg,
      ))
      Datum(list_from_array(items))
    }
    MakeList =>
      match args {
        [len_value] => {
          let len = value_as_nonnegative_int(len_value)
          let fill = Datum::Nil
          let items : Array[@core.Datum] = []
          for i in 0..<len {
            items.push(fill)
          }
          Datum(list_from_array(items))
        }
        [len_value, fill_value] => {
          let len = value_as_nonnegative_int(len_value)
          let fill = value_to_datum_element(fill_value)
          let items : Array[@core.Datum] = []
          for i in 0..<len {
            items.push(fill)
          }
          Datum(list_from_array(items))
        }
        _ =>
          raise EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    NullP =>
      match args {
        [Datum(Nil)] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    PairP =>
      match args {
        [Datum(Pair(_, _))] => bool_value(true)
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    ListP =>
      match args {
        [Datum(d)] => bool_value(is_proper_list(d))
        [_] => bool_value(false)
        _ => raise arity_mismatch(1, args.length())
      }
    Length =>
      match args {
        [Datum(d)] => {
          let items = datum_list_to_array(d)
          Datum(Int(items.length()))
        }
        [_] => raise EvalError("type error: list expected")
        _ => raise arity_mismatch(1, args.length())
      }
    Append =>
      match args {
        [] => Datum(Nil)
        [single] => single
        [.. prefix, last] => {
          let items : Array[@core.Datum] = []
          for value in prefix {
            let datum = match value {
              Datum(d) => d
              _ => raise EvalError("type error: list expected")
            }
            let part = datum_list_to_array(datum)
            items.push_iter(part.iter())
          }
          let tail = value_to_datum_element(last)
          Datum(list_with_tail(items, tail))
        }
      }
    Reverse =>
      match args {
        [Datum(datum)] => {
          let items = datum_list_to_array(datum)
          let result = for i = 0, result = Datum::Nil; i < items.length(); {
            // invariant : i >= 0 && i <= items.length()
            // decreases : items.length() - i
            // assert : i <= items.length()
            continue i + 1, pair_new(items[i], result)
          } else {
            result
          }
          Datum(result)
        }
        [_] => raise EvalError("type error: list expected")
        _ => raise arity_mismatch(1, args.length())
      }
    ListRef =>
      match args {
        [list_value, idx_value] => {
          let datum = value_as_datum(list_value)
          let idx = value_as_int_index(idx_value)
          let items = datum_list_to_array(datum)
          check_index(idx, items.length())
          value_from_datum(items[idx])
        }
        _ => raise arity_mismatch(2, args.length())
      }
    ListTail =>
      match args {
        [list_value, idx_value] => {
          let datum = value_as_datum(list_value)
          let idx = value_as_int_index(idx_value)
          let items = datum_list_to_array(datum)
          if idx < 0 || idx > items.length() {
            raise EvalError("index out of range")
          }
          Datum(list_from_array(items[idx:].to_array()))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    ListCopy =>
      match args {
        [list_value] => {
          let datum = value_as_datum(list_value)
          let items = datum_list_to_array(datum)
          Datum(list_from_array(items))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    Member | Memq | Memv | Assoc | Assq | Assv =>
      match args {
        [item, list_value] => {
          let list = value_as_datum(list_value)
          let mode = if prim is Member || prim is Assoc {
            EqualMode
          } else {
            EqvMode
          }
          if prim is Member || prim is Memq || prim is Memv {
            list_member(mode, item, list)
          } else {
            list_assoc(mode, item, list)
          }
        }
        _ => raise arity_mismatch(2, args.length())
      }
    _ => raise EvalError("internal error: unexpected pair/list primitive")
  }
}
