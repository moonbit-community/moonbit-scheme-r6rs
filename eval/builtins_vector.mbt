///|
/// Apply vector primitives.
fn apply_vector_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    Vector => {
      let items : Array[@core.Datum] = args.map(arg => value_to_datum_element(
        arg,
      ))
      Some(@core.Value::Datum(Vector(items)))
    }
    MakeVector =>
      match args {
        [len_value] =>
          Some(
            {
              let len = value_as_nonnegative_int(len_value)
              let fill = @core.Datum::Nil
              let items : Array[@core.Datum] = []
              for i in 0..<len {
                items.push(fill)
              }
              @core.Value::Datum(Vector(items))
            },
          )
        [len_value, fill_value] =>
          Some(
            {
              let len = value_as_nonnegative_int(len_value)
              let fill = value_to_datum_element(fill_value)
              let items : Array[@core.Datum] = []
              for i in 0..<len {
                items.push(fill)
              }
              @core.Value::Datum(Vector(items))
            },
          )
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    VectorP =>
      match args {
        [Datum(Vector(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    VectorLength =>
      match args {
        [vec_value] => {
          let items = value_as_vector(vec_value)
          Some(@core.Value::Datum(Int(items.length())))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    VectorRef =>
      match args {
        [vec_value, idx_value] => {
          let items = value_as_vector(vec_value)
          let idx = value_as_int_index(idx_value)
          check_index(idx, items.length())
          Some(value_from_datum(items[idx]))
        }
        _ => raise arity_mismatch(2, args.length())
      }
    VectorSet =>
      match args {
        [vec_value, idx_value, item_value] => {
          let items = value_as_vector(vec_value)
          let idx = value_as_int_index(idx_value)
          check_index(idx, items.length())
          items[idx] = value_to_datum_element(item_value)
          Some(@core.Value::Void)
        }
        _ => raise arity_mismatch(3, args.length())
      }
    VectorFill =>
      match args {
        [vec_value, fill_value, .. rest] if rest.length() <= 2 => {
          let items = value_as_vector(vec_value)
          let fill = value_to_datum_element(fill_value)
          let start = if rest.length() >= 1 {
            value_as_int_index(rest[0])
          } else {
            0
          }
          let end = if rest.length() >= 2 {
            value_as_int_index(rest[1])
          } else {
            items.length()
          }
          check_slice_range(start, end, items.length())
          for i in start..<end {
            items[i] = fill
          }
          Some(@core.Value::Void)
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 to 4 got \{args.length()}",
          )
      }
    VectorCopy =>
      match args {
        [vec_value, .. rest] if rest.length() <= 2 => {
          let items = value_as_vector(vec_value)
          let start = if rest.length() >= 1 {
            value_as_int_index(rest[0])
          } else {
            0
          }
          let end = if rest.length() >= 2 {
            value_as_int_index(rest[1])
          } else {
            items.length()
          }
          check_slice_range(start, end, items.length())
          let slice = items[start:end].to_array()
          Some(@core.Value::Datum(Vector(slice)))
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    VectorCopyBang =>
      match args {
        [to_value, at_value, from_value, .. rest] if rest.length() <= 2 => {
          let to_items = value_as_vector(to_value)
          let at = value_as_int_index(at_value)
          let from_items = value_as_vector(from_value)
          let start = if rest.length() >= 1 {
            value_as_int_index(rest[0])
          } else {
            0
          }
          let end = if rest.length() >= 2 {
            value_as_int_index(rest[1])
          } else {
            from_items.length()
          }
          let count = check_copy_range(
            start,
            end,
            from_items.length(),
            at,
            to_items.length(),
          )
          let temp : Array[@core.Datum] = []
          for i in 0..<count {
            temp.push(from_items[start + i])
          }
          for j in 0..<count {
            to_items[at + j] = temp[j]
          }
          Some(@core.Value::Void)
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 3 to 5 got \{args.length()}",
          )
      }
    VectorAppend => {
      let items : Array[@core.Datum] = []
      for arg in args {
        let vec = value_as_vector(arg)
        items.push_iter(vec.iter())
      }
      Some(@core.Value::Datum(Vector(items)))
    }
    VectorToList =>
      match args {
        [vec_value, .. rest] if rest.length() <= 2 => {
          let items = value_as_vector(vec_value)
          let start = if rest.length() >= 1 {
            value_as_int_index(rest[0])
          } else {
            0
          }
          let end = if rest.length() >= 2 {
            value_as_int_index(rest[1])
          } else {
            items.length()
          }
          check_slice_range(start, end, items.length())
          Some(@core.Value::Datum(list_from_array(items[start:end].to_array())))
        }
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    ListToVector =>
      match args {
        [datum_value] => {
          let datum = value_as_datum(datum_value)
          let items = datum_list_to_array(datum)
          Some(@core.Value::Datum(Vector(items)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    _ => None
  }
}
