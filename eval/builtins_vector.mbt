///|
/// Apply vector primitives.
fn apply_vector_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::Vector => {
      let items : Array[Datum] = []
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        items.push(value_to_datum_element(arg))
      }
      Some(Value::Datum(Datum::Vector(items)))
    }
    Primitive::MakeVector =>
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
            Value::Datum(Datum::Vector(items))
          })
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::VectorP =>
      match args {
        [Value::Datum(Datum::Vector(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::VectorLength =>
      match args.length() {
        1 =>
          Some({
            let items = value_as_vector(args[0])
            Value::Datum(Datum::Int(items.length()))
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::VectorRef =>
      match args.length() {
        2 =>
          Some({
            let items = value_as_vector(args[0])
            let idx = value_as_int_index(args[1])
            check_index(idx, items.length())
            value_from_datum(items[idx])
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::VectorSet =>
      match args.length() {
        3 =>
          Some({
            let items = value_as_vector(args[0])
            let idx = value_as_int_index(args[1])
            check_index(idx, items.length())
            items[idx] = value_to_datum_element(args[2])
            Value::Void
          })
        n => raise arity_mismatch(3, n)
      }
    Primitive::VectorFill =>
      match args.length() {
        2 | 3 | 4 =>
          Some({
            let items = value_as_vector(args[0])
            let fill = value_to_datum_element(args[1])
            let start = if args.length() >= 3 {
              value_as_int_index(args[2])
            } else {
              0
            }
            let end = if args.length() == 4 {
              value_as_int_index(args[3])
            } else {
              items.length()
            }
            check_slice_range(start, end, items.length())
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              items[i] = fill
              continue i + 1
            }
            Value::Void
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    Primitive::VectorCopy =>
      match args.length() {
        1 | 2 | 3 =>
          Some({
            let items = value_as_vector(args[0])
            let start = if args.length() >= 2 {
              value_as_int_index(args[1])
            } else {
              0
            }
            let end = if args.length() == 3 {
              value_as_int_index(args[2])
            } else {
              items.length()
            }
            check_slice_range(start, end, items.length())
            let slice = items.sub(start~, end~).to_array()
            Value::Datum(Datum::Vector(slice))
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::VectorCopyBang =>
      match args.length() {
        3 | 4 | 5 =>
          Some({
            let to_items = value_as_vector(args[0])
            let at = value_as_int_index(args[1])
            let from_items = value_as_vector(args[2])
            let start = if args.length() >= 4 {
              value_as_int_index(args[3])
            } else {
              0
            }
            let end = if args.length() == 5 {
              value_as_int_index(args[4])
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
            let temp : Array[Datum] = []
            for i = 0; i < count; {
              // invariant : i >= 0 && i <= count
              // decreases : count - i
              // assert : i <= count
              temp.push(from_items[start + i])
              continue i + 1
            }
            for j = 0; j < count; {
              // invariant : j >= 0 && j <= count
              // decreases : count - j
              // assert : j <= count
              to_items[at + j] = temp[j]
              continue j + 1
            }
            Value::Void
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    Primitive::VectorAppend => {
      let items : Array[Datum] = []
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        let vec = value_as_vector(arg)
        for item in vec {
          // invariant : vec.length() >= 0
          // TODO(decreases) : loop index not exposed; possible bug
          // assert : vec.length() >= 0
          items.push(item)
        }
      }
      Some(Value::Datum(Datum::Vector(items)))
    }
    Primitive::VectorToList =>
      match args.length() {
        1 | 2 | 3 =>
          Some({
            let items = value_as_vector(args[0])
            let start = if args.length() >= 2 {
              value_as_int_index(args[1])
            } else {
              0
            }
            let end = if args.length() == 3 {
              value_as_int_index(args[2])
            } else {
              items.length()
            }
            check_slice_range(start, end, items.length())
            Value::Datum(list_from_array(items.sub(start~, end~).to_array()))
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::ListToVector =>
      match args.length() {
        1 =>
          Some({
            let datum = value_as_datum(args[0])
            let items = datum_list_to_array(datum)
            Value::Datum(Datum::Vector(items))
          })
        n => raise arity_mismatch(1, n)
      }
    _ => None
  }
}
