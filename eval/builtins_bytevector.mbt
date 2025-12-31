///|
/// Apply bytevector primitives.
fn apply_bytevector_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::ByteVector => {
      let items : Array[Int] = []
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        items.push(value_as_byte(arg))
      }
      Some(Value::Datum(Datum::ByteVector(items)))
    }
    Primitive::MakeByteVector =>
      match args.length() {
        1 | 2 =>
          Some({
            let len = value_as_nonnegative_int(args[0])
            let fill = if args.length() == 2 {
              value_as_byte(args[1])
            } else {
              0
            }
            let items : Array[Int] = []
            for i = 0; i < len; {
              // invariant : i >= 0 && i <= len
              // decreases : len - i
              // assert : i <= len
              items.push(fill)
              continue i + 1
            }
            Value::Datum(Datum::ByteVector(items))
          })
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::ByteVectorP =>
      match args {
        [Value::Datum(Datum::ByteVector(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::ByteVectorLength =>
      match args.length() {
        1 =>
          Some({
            let items = value_as_bytevector(args[0])
            Value::Datum(Datum::Int(items.length()))
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::ByteVectorEq =>
      match args.length() {
        2 =>
          Some({
            let left = value_as_bytevector(args[0])
            let right = value_as_bytevector(args[1])
            if left.length() != right.length() {
              bool_value(false)
            } else {
              let ok = for i = 0; i < left.length(); {
                // invariant : i >= 0 && i <= left.length()
                // decreases : left.length() - i
                // assert : i <= left.length()
                if left[i] != right[i] {
                  break false
                }
                continue i + 1
              } else {
                true
              }
              bool_value(ok)
            }
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::ByteVectorU8Ref =>
      match args.length() {
        2 =>
          Some({
            let items = value_as_bytevector(args[0])
            let idx = value_as_int_index(args[1])
            check_index(idx, items.length())
            Value::Datum(Datum::Int(items[idx]))
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::ByteVectorU8Set =>
      match args.length() {
        3 =>
          Some({
            let items = value_as_bytevector(args[0])
            let idx = value_as_int_index(args[1])
            check_index(idx, items.length())
            items[idx] = value_as_byte(args[2])
            Value::Void
          })
        n => raise arity_mismatch(3, n)
      }
    Primitive::ByteVectorCopy =>
      match args.length() {
        1 | 2 | 3 =>
          Some({
            let items = value_as_bytevector(args[0])
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
            Value::Datum(Datum::ByteVector(slice))
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::ByteVectorCopyBang =>
      match args.length() {
        3 | 4 | 5 =>
          Some({
            let to_items = value_as_bytevector(args[0])
            let at = value_as_int_index(args[1])
            let from_items = value_as_bytevector(args[2])
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
            let temp : Array[Int] = []
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
    Primitive::ByteVectorAppend => {
      let items : Array[Int] = []
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        let vec = value_as_bytevector(arg)
        for item in vec {
          // invariant : vec.length() >= 0
          // TODO(decreases) : loop index not exposed; possible bug
          // assert : vec.length() >= 0
          items.push(item)
        }
      }
      Some(Value::Datum(Datum::ByteVector(items)))
    }
    Primitive::ByteVectorFill =>
      match args.length() {
        2 | 3 | 4 =>
          Some({
            let items = value_as_bytevector(args[0])
            let fill = value_as_byte(args[1])
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
    Primitive::ByteVectorToU8List =>
      match args.length() {
        1 | 2 | 3 =>
          Some({
            let items = value_as_bytevector(args[0])
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
            let datums : Array[Datum] = []
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              datums.push(Datum::Int(items[i]))
              continue i + 1
            }
            Value::Datum(list_from_array(datums))
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::U8ListToByteVector =>
      match args.length() {
        1 =>
          Some({
            let datum = value_as_datum(args[0])
            let items = datum_list_to_array(datum)
            let bytes : Array[Int] = []
            for item in items {
              // invariant : items.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : items.length() >= 0
              match item {
                Datum::Int(n) => {
                  if n < 0 || n > 255 {
                    raise @core.EvalError("type error: byte expected")
                  }
                  bytes.push(n)
                }
                _ => raise @core.EvalError("type error: byte expected")
              }
            }
            Value::Datum(Datum::ByteVector(bytes))
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringToUtf8 =>
      match args.length() {
        1 | 2 | 3 =>
          Some({
            let s = value_as_string(args[0])
            let start = if args.length() >= 2 {
              value_as_int_index(args[1])
            } else {
              0
            }
            let end = if args.length() == 3 {
              value_as_int_index(args[2])
            } else {
              string_char_length(s)
            }
            let bytes = string_to_utf8_bytes(s, start, end)
            Value::Datum(Datum::ByteVector(bytes))
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::Utf8ToString =>
      match args.length() {
        1 | 2 | 3 =>
          Some({
            let items = value_as_bytevector(args[0])
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
            let text = utf8_decode_to_string(items, start, end)
            Value::Datum(Datum::String(Ref::new(text)))
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::NativeEndianness =>
      match args.length() {
        0 => Some(native_endianness_value())
        n => raise arity_mismatch(0, n)
      }
    Primitive::ByteVectorUintRef =>
      match args.length() {
        4 =>
          Some({
            let items = value_as_bytevector(args[0])
            let start = value_as_int_index(args[1])
            let endian = endianness_from_value(args[2])
            let size = bytevector_size_from_value(args[3])
            check_bytevector_range(start, size, items.length())
            let value = bytevector_uint_ref(items, start, size, endian)
            Value::Datum(bigint_to_integer_datum(value))
          })
        n => raise arity_mismatch(4, n)
      }
    Primitive::ByteVectorSintRef =>
      match args.length() {
        4 =>
          Some({
            let items = value_as_bytevector(args[0])
            let start = value_as_int_index(args[1])
            let endian = endianness_from_value(args[2])
            let size = bytevector_size_from_value(args[3])
            check_bytevector_range(start, size, items.length())
            let value = bytevector_sint_ref(items, start, size, endian)
            Value::Datum(bigint_to_integer_datum(value))
          })
        n => raise arity_mismatch(4, n)
      }
    Primitive::ByteVectorUintSet =>
      match args.length() {
        5 =>
          Some({
            let items = value_as_bytevector(args[0])
            let start = value_as_int_index(args[1])
            let endian = endianness_from_value(args[2])
            let size = bytevector_size_from_value(args[3])
            check_bytevector_range(start, size, items.length())
            let value = datum_to_bigint(value_as_exact_integer(args[4]))
            bytevector_uint_set(items, start, size, endian, value)
            Value::Void
          })
        n => raise arity_mismatch(5, n)
      }
    Primitive::ByteVectorSintSet =>
      match args.length() {
        5 =>
          Some({
            let items = value_as_bytevector(args[0])
            let start = value_as_int_index(args[1])
            let endian = endianness_from_value(args[2])
            let size = bytevector_size_from_value(args[3])
            check_bytevector_range(start, size, items.length())
            let value = datum_to_bigint(value_as_exact_integer(args[4]))
            let unsigned = bytevector_sint_to_uint(value, size)
            bytevector_uint_set(items, start, size, endian, unsigned)
            Value::Void
          })
        n => raise arity_mismatch(5, n)
      }
    _ => None
  }
}
