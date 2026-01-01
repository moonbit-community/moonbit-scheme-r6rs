///|
/// @core.Primitive::Apply bytevector primitives.
fn apply_bytevector_primitive(
  prim : @core.Primitive,
  args : Array[@core.Value],
) -> @core.Value? raise @core.EvalError {
  match prim {
    @core.Primitive::ByteVector => {
      let items : Array[Int] = args.map((arg) => value_as_byte(arg))
      Some(@core.Value::Datum(@core.Datum::ByteVector(items)))
    }
    @core.Primitive::MakeByteVector =>
      match args {
        [len_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = 0
            let items : Array[Int] = []
            for i = 0; i < len; {
              // invariant : i >= 0 && i <= len
              // decreases : len - i
              // assert : i <= len
              items.push(fill)
              continue i + 1
            }
            @core.Value::Datum(@core.Datum::ByteVector(items))
          })
        [len_value, fill_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = value_as_byte(fill_value)
            let items : Array[Int] = []
            for i = 0; i < len; {
              // invariant : i >= 0 && i <= len
              // decreases : len - i
              // assert : i <= len
              items.push(fill)
              continue i + 1
            }
            @core.Value::Datum(@core.Datum::ByteVector(items))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    @core.Primitive::ByteVectorP =>
      match args {
        [@core.Value::Datum(@core.Datum::ByteVector(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ByteVectorLength =>
      match args {
        [vec_value] => {
          let items = value_as_bytevector(vec_value)
          Some(@core.Value::Datum(@core.Datum::Int(items.length())))
        }
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::ByteVectorEq =>
      match args {
        [left_value, right_value] =>
          Some({
            let left = value_as_bytevector(left_value)
            let right = value_as_bytevector(right_value)
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
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::ByteVectorU8Ref =>
      match args {
        [vec_value, idx_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let idx = value_as_int_index(idx_value)
            check_index(idx, items.length())
            @core.Value::Datum(@core.Datum::Int(items[idx]))
          })
        _ => raise arity_mismatch(2, args.length())
      }
    @core.Primitive::ByteVectorU8Set =>
      match args {
        [vec_value, idx_value, item_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let idx = value_as_int_index(idx_value)
            check_index(idx, items.length())
            items[idx] = value_as_byte(item_value)
            @core.Value::Void
          })
        _ => raise arity_mismatch(3, args.length())
      }
    @core.Primitive::ByteVectorCopy =>
      match args {
        [vec_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = 0
            let end = items.length()
            check_slice_range(start, end, items.length())
            let slice = items.sub(start~, end~).to_array()
            @core.Value::Datum(@core.Datum::ByteVector(slice))
          })
        [vec_value, start_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let end = items.length()
            check_slice_range(start, end, items.length())
            let slice = items.sub(start~, end~).to_array()
            @core.Value::Datum(@core.Datum::ByteVector(slice))
          })
        [vec_value, start_value, end_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            check_slice_range(start, end, items.length())
            let slice = items.sub(start~, end~).to_array()
            @core.Value::Datum(@core.Datum::ByteVector(slice))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    @core.Primitive::ByteVectorCopyBang =>
      match args {
        [to_value, at_value, from_value] =>
          Some({
            let to_items = value_as_bytevector(to_value)
            let at = value_as_int_index(at_value)
            let from_items = value_as_bytevector(from_value)
            let start = 0
            let end = from_items.length()
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
            @core.Value::Void
          })
        [to_value, at_value, from_value, start_value] =>
          Some({
            let to_items = value_as_bytevector(to_value)
            let at = value_as_int_index(at_value)
            let from_items = value_as_bytevector(from_value)
            let start = value_as_int_index(start_value)
            let end = from_items.length()
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
            @core.Value::Void
          })
        [to_value, at_value, from_value, start_value, end_value] =>
          Some({
            let to_items = value_as_bytevector(to_value)
            let at = value_as_int_index(at_value)
            let from_items = value_as_bytevector(from_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
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
            @core.Value::Void
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 3 to 5 got \{args.length()}",
          )
      }
    @core.Primitive::ByteVectorAppend => {
      let items : Array[Int] = []
      for arg in args {
        let vec = value_as_bytevector(arg)
        items.push_iter(vec.iter())
      }
      Some(@core.Value::Datum(@core.Datum::ByteVector(items)))
    }
    @core.Primitive::ByteVectorFill =>
      match args {
        [vec_value, fill_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let fill = value_as_byte(fill_value)
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
            @core.Value::Void
          })
        [vec_value, fill_value, start_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let fill = value_as_byte(fill_value)
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
            @core.Value::Void
          })
        [vec_value, fill_value, start_value, end_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let fill = value_as_byte(fill_value)
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
            @core.Value::Void
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 to 4 got \{args.length()}",
          )
      }
    @core.Primitive::ByteVectorToU8List =>
      match args {
        [vec_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = 0
            let end = items.length()
            check_slice_range(start, end, items.length())
            let datums : Array[@core.Datum] = []
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              datums.push(@core.Datum::Int(items[i]))
              continue i + 1
            }
            @core.Value::Datum(list_from_array(datums))
          })
        [vec_value, start_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let end = items.length()
            check_slice_range(start, end, items.length())
            let datums : Array[@core.Datum] = []
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              datums.push(@core.Datum::Int(items[i]))
              continue i + 1
            }
            @core.Value::Datum(list_from_array(datums))
          })
        [vec_value, start_value, end_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            check_slice_range(start, end, items.length())
            let datums : Array[@core.Datum] = []
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              datums.push(@core.Datum::Int(items[i]))
              continue i + 1
            }
            @core.Value::Datum(list_from_array(datums))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    @core.Primitive::U8ListToByteVector =>
      match args {
        [datum_value] =>
          Some({
            let datum = value_as_datum(datum_value)
            let items = datum_list_to_array(datum)
            let bytes : Array[Int] = items.map((item) =>
              match item {
                @core.Datum::Int(n) => {
                  if n < 0 || n > 255 {
                    raise @core.EvalError("type error: byte expected")
                  }
                  n
                }
                _ => raise @core.EvalError("type error: byte expected")
              }
            )
            @core.Value::Datum(@core.Datum::ByteVector(bytes))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    @core.Primitive::StringToUtf8 =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = 0
            let end = string_char_length(s)
            let bytes = string_to_utf8_bytes(s, start, end)
            @core.Value::Datum(@core.Datum::ByteVector(bytes))
          })
        [string_value, start_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = value_as_int_index(start_value)
            let end = string_char_length(s)
            let bytes = string_to_utf8_bytes(s, start, end)
            @core.Value::Datum(@core.Datum::ByteVector(bytes))
          })
        [string_value, start_value, end_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            let bytes = string_to_utf8_bytes(s, start, end)
            @core.Value::Datum(@core.Datum::ByteVector(bytes))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    @core.Primitive::Utf8ToString =>
      match args {
        [vec_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = 0
            let end = items.length()
            let text = utf8_decode_to_string(items, start, end)
            @core.Value::Datum(@core.Datum::String(Ref::new(text)))
          })
        [vec_value, start_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let end = items.length()
            let text = utf8_decode_to_string(items, start, end)
            @core.Value::Datum(@core.Datum::String(Ref::new(text)))
          })
        [vec_value, start_value, end_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            let text = utf8_decode_to_string(items, start, end)
            @core.Value::Datum(@core.Datum::String(Ref::new(text)))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    @core.Primitive::NativeEndianness =>
      match args {
        [] => Some(native_endianness_value())
        _ => raise arity_mismatch(0, args.length())
      }
    @core.Primitive::ByteVectorUintRef =>
      match args {
        [vec_value, start_value, endian_value, size_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let endian = endianness_from_value(endian_value)
            let size = bytevector_size_from_value(size_value)
            check_bytevector_range(start, size, items.length())
            let value = bytevector_uint_ref(items, start, size, endian)
            @core.Value::Datum(bigint_to_integer_datum(value))
          })
        _ => raise arity_mismatch(4, args.length())
      }
    @core.Primitive::ByteVectorSintRef =>
      match args {
        [vec_value, start_value, endian_value, size_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let endian = endianness_from_value(endian_value)
            let size = bytevector_size_from_value(size_value)
            check_bytevector_range(start, size, items.length())
            let value = bytevector_sint_ref(items, start, size, endian)
            @core.Value::Datum(bigint_to_integer_datum(value))
          })
        _ => raise arity_mismatch(4, args.length())
      }
    @core.Primitive::ByteVectorUintSet =>
      match args {
        [vec_value, start_value, endian_value, size_value, value_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let endian = endianness_from_value(endian_value)
            let size = bytevector_size_from_value(size_value)
            check_bytevector_range(start, size, items.length())
            let value = datum_to_bigint(value_as_exact_integer(value_value))
            bytevector_uint_set(items, start, size, endian, value)
            @core.Value::Void
          })
        _ => raise arity_mismatch(5, args.length())
      }
    @core.Primitive::ByteVectorSintSet =>
      match args {
        [vec_value, start_value, endian_value, size_value, value_value] =>
          Some({
            let items = value_as_bytevector(vec_value)
            let start = value_as_int_index(start_value)
            let endian = endianness_from_value(endian_value)
            let size = bytevector_size_from_value(size_value)
            check_bytevector_range(start, size, items.length())
            let value = datum_to_bigint(value_as_exact_integer(value_value))
            let unsigned = bytevector_sint_to_uint(value, size)
            bytevector_uint_set(items, start, size, endian, unsigned)
            @core.Value::Void
          })
        _ => raise arity_mismatch(5, args.length())
      }
    _ => None
  }
}
