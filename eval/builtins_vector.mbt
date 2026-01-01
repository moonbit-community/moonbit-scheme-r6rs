///|
/// Apply vector primitives.
fn apply_vector_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Vector => {
      let items : Array[Datum] =
        args.map((arg) => value_to_datum_element(arg))
      Some(Datum(Vector(items)))
    }
    MakeVector =>
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
            Datum(Vector(items))
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
            Datum(Vector(items))
          })
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
          Some(Datum(Int(items.length())))
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
          Some(Void)
        }
        _ => raise arity_mismatch(3, args.length())
      }
    VectorFill =>
      match args {
        [vec_value, fill_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let fill = value_to_datum_element(fill_value)
            let start = 0
            let end = items.length()
            check_slice_range(start, end, items.length())
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              items[i] = fill
              continue i + 1
            }
            Void
          })
        [vec_value, fill_value, start_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let fill = value_to_datum_element(fill_value)
            let start = value_as_int_index(start_value)
            let end = items.length()
            check_slice_range(start, end, items.length())
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              items[i] = fill
              continue i + 1
            }
            Void
          })
        [vec_value, fill_value, start_value, end_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let fill = value_to_datum_element(fill_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            check_slice_range(start, end, items.length())
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              items[i] = fill
              continue i + 1
            }
            Void
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 to 4 got \{args.length()}",
          )
      }
    VectorCopy =>
      match args {
        [vec_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let start = 0
            let end = items.length()
            check_slice_range(start, end, items.length())
            let slice = items.sub(start~, end~).to_array()
            Datum(Vector(slice))
          })
        [vec_value, start_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let start = value_as_int_index(start_value)
            let end = items.length()
            check_slice_range(start, end, items.length())
            let slice = items.sub(start~, end~).to_array()
            Datum(Vector(slice))
          })
        [vec_value, start_value, end_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            check_slice_range(start, end, items.length())
            let slice = items.sub(start~, end~).to_array()
            Datum(Vector(slice))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    VectorCopyBang =>
      match args {
        [to_value, at_value, from_value] =>
          Some({
            let to_items = value_as_vector(to_value)
            let at = value_as_int_index(at_value)
            let from_items = value_as_vector(from_value)
            let start = 0
            let end = from_items.length()
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
            Void
          })
        [to_value, at_value, from_value, start_value] =>
          Some({
            let to_items = value_as_vector(to_value)
            let at = value_as_int_index(at_value)
            let from_items = value_as_vector(from_value)
            let start = value_as_int_index(start_value)
            let end = from_items.length()
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
            Void
          })
        [to_value, at_value, from_value, start_value, end_value] =>
          Some({
            let to_items = value_as_vector(to_value)
            let at = value_as_int_index(at_value)
            let from_items = value_as_vector(from_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
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
            Void
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 3 to 5 got \{args.length()}",
          )
      }
    VectorAppend => {
      let items : Array[Datum] = []
      for arg in args {
        let vec = value_as_vector(arg)
        items.push_iter(vec.iter())
      }
      Some(Datum(Vector(items)))
    }
    VectorToList =>
      match args {
        [vec_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let start = 0
            let end = items.length()
            check_slice_range(start, end, items.length())
            Datum(list_from_array(items.sub(start~, end~).to_array()))
          })
        [vec_value, start_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let start = value_as_int_index(start_value)
            let end = items.length()
            check_slice_range(start, end, items.length())
            Datum(list_from_array(items.sub(start~, end~).to_array()))
          })
        [vec_value, start_value, end_value] =>
          Some({
            let items = value_as_vector(vec_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            check_slice_range(start, end, items.length())
            Datum(list_from_array(items.sub(start~, end~).to_array()))
          })
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
          Some(Datum(Vector(items)))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    _ => None
  }
}
