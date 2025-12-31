///|
/// Apply character and string primitives.
fn apply_char_string_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    Primitive::CharEq =>
      Some(bool_value(compare_chain_char(args, CompareMode::Eq, false)))
    Primitive::CharLess =>
      Some(bool_value(compare_chain_char(args, CompareMode::Lt, false)))
    Primitive::CharGreater =>
      Some(bool_value(compare_chain_char(args, CompareMode::Gt, false)))
    Primitive::CharLessEq =>
      Some(bool_value(compare_chain_char(args, CompareMode::Le, false)))
    Primitive::CharGreaterEq =>
      Some(bool_value(compare_chain_char(args, CompareMode::Ge, false)))
    Primitive::CharCiEq =>
      Some(bool_value(compare_chain_char(args, CompareMode::Eq, true)))
    Primitive::CharCiLess =>
      Some(bool_value(compare_chain_char(args, CompareMode::Lt, true)))
    Primitive::CharCiGreater =>
      Some(bool_value(compare_chain_char(args, CompareMode::Gt, true)))
    Primitive::CharCiLessEq =>
      Some(bool_value(compare_chain_char(args, CompareMode::Le, true)))
    Primitive::CharCiGreaterEq =>
      Some(bool_value(compare_chain_char(args, CompareMode::Ge, true)))
    Primitive::CharP =>
      match args {
        [Value::Datum(Datum::Char(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharToInteger =>
      match args.length() {
        1 => Some(Value::Datum(Datum::Int(value_as_char(args[0]).to_int())))
        n => raise arity_mismatch(1, n)
      }
    Primitive::IntegerToChar =>
      match args.length() {
        1 =>
          Some({
            let ch = char_from_int_value(args[0])
            Value::Datum(Datum::Char(ch))
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharAlphabeticP =>
      match args.length() {
        1 =>
          Some({
            let ch = value_as_char(args[0])
            bool_value(unicode_char(ch).is_alphabetic())
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharNumericP =>
      match args.length() {
        1 =>
          Some({
            let ch = value_as_char(args[0])
            bool_value(ch.is_numeric())
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharWhitespaceP =>
      match args.length() {
        1 =>
          Some({
            let ch = value_as_char(args[0])
            bool_value(ch.is_whitespace())
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharUpperCaseP =>
      match args.length() {
        1 =>
          Some({
            let ch = value_as_char(args[0])
            bool_value(unicode_char(ch).is_uppercase())
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharLowerCaseP =>
      match args.length() {
        1 =>
          Some({
            let ch = value_as_char(args[0])
            bool_value(unicode_char(ch).is_lowercase())
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharUpcase =>
      match args.length() {
        1 =>
          Some(Value::Datum(Datum::Char(unicode_char(value_as_char(args[0])).upcase())))
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharDowncase =>
      match args.length() {
        1 =>
          Some(
            Value::Datum(Datum::Char(unicode_char(value_as_char(args[0])).downcase())),
          )
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharFoldcase =>
      match args.length() {
        1 =>
          Some(
            Value::Datum(Datum::Char(unicode_char(value_as_char(args[0])).foldcase())),
          )
        n => raise arity_mismatch(1, n)
      }
    Primitive::CharGeneralCategory =>
      match args.length() {
        1 =>
          Some(
            Value::Datum(
              Datum::Symbol(
                unicode_char(value_as_char(args[0])).general_category(),
              ),
            ),
          )
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringEq =>
      Some(bool_value(compare_chain_string(args, CompareMode::Eq, false)))
    Primitive::StringLess =>
      Some(bool_value(compare_chain_string(args, CompareMode::Lt, false)))
    Primitive::StringGreater =>
      Some(bool_value(compare_chain_string(args, CompareMode::Gt, false)))
    Primitive::StringLessEq =>
      Some(bool_value(compare_chain_string(args, CompareMode::Le, false)))
    Primitive::StringGreaterEq =>
      Some(bool_value(compare_chain_string(args, CompareMode::Ge, false)))
    Primitive::StringCiEq =>
      Some(bool_value(compare_chain_string(args, CompareMode::Eq, true)))
    Primitive::StringCiLess =>
      Some(bool_value(compare_chain_string(args, CompareMode::Lt, true)))
    Primitive::StringCiGreater =>
      Some(bool_value(compare_chain_string(args, CompareMode::Gt, true)))
    Primitive::StringCiLessEq =>
      Some(bool_value(compare_chain_string(args, CompareMode::Le, true)))
    Primitive::StringCiGreaterEq =>
      Some(bool_value(compare_chain_string(args, CompareMode::Ge, true)))
    Primitive::String => {
      let buf = @buffer.new()
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        buf.write_char(value_as_char(arg))
      }
      Some(Value::Datum(Datum::String(Ref::new(buf.to_string()))))
    }
    Primitive::MakeString =>
      match args.length() {
        1 | 2 =>
          Some({
            let len = value_as_nonnegative_int(args[0])
            let fill = if args.length() == 2 {
              value_as_char(args[1])
            } else {
              let zero = 0
              match zero.to_char() {
                Some(ch) => ch
                None =>
                  raise @core.EvalError("internal error: invalid default char")
              }
            }
            let buf = @buffer.new()
            for i = 0; i < len; {
              // invariant : i >= 0 && i <= len
              // decreases : len - i
              // assert : i <= len
              buf.write_char(fill)
              continue i + 1
            }
            Value::Datum(Datum::String(Ref::new(buf.to_string())))
          })
        n => raise @core.EvalError("arity mismatch: expected 1 or 2 got \{n}")
      }
    Primitive::StringP =>
      match args {
        [Value::Datum(Datum::String(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringLength =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(Datum::Int(s.length()))
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringAppend => {
      let buf = @buffer.new()
      for arg in args {
        // invariant : args.length() >= 0
        // TODO(decreases) : loop index not exposed; possible bug
        // assert : args.length() >= 0
        buf.write_string(value_as_string(arg))
      }
      Some(Value::Datum(Datum::String(Ref::new(buf.to_string()))))
    }
    Primitive::StringRef =>
      match args.length() {
        2 =>
          Some({
            let s = value_as_string(args[0])
            let idx = value_as_int_index(args[1])
            check_index(idx, s.length())
            match s.get_char(idx) {
              Some(ch) => Value::Datum(Datum::Char(ch))
              None => raise @core.EvalError("index out of range")
            }
          })
        n => raise arity_mismatch(2, n)
      }
    Primitive::StringSet =>
      match args.length() {
        3 =>
          Some({
            let sref = value_as_string_ref(args[0])
            let idx = value_as_int_index(args[1])
            let ch = value_as_char(args[2])
            let updated = string_replace_range(sref.val, idx, idx + 1, ch)
            sref.val = updated
            Value::Void
          })
        n => raise arity_mismatch(3, n)
      }
    Primitive::StringCopy =>
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
              s.length()
            }
            check_slice_range(start, end, s.length())
            let view = try? s[start:end]
            match view {
              Ok(slice) =>
                Value::Datum(Datum::String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::Substring =>
      match args.length() {
        3 =>
          Some({
            let s = value_as_string(args[0])
            let start = value_as_int_index(args[1])
            let end = value_as_int_index(args[2])
            check_slice_range(start, end, s.length())
            let view = try? s[start:end]
            match view {
              Ok(slice) =>
                Value::Datum(Datum::String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        n => raise arity_mismatch(3, n)
      }
    Primitive::StringCopyBang =>
      match args.length() {
        3 | 4 | 5 =>
          Some({
            let to_ref = value_as_string_ref(args[0])
            let at = value_as_int_index(args[1])
            let from_str = value_as_string(args[2])
            let start = if args.length() >= 4 {
              value_as_int_index(args[3])
            } else {
              0
            }
            let end = if args.length() == 5 {
              value_as_int_index(args[4])
            } else {
              from_str.length()
            }
            let to_str = to_ref.val
            let to_len = to_str.length()
            let count = check_copy_range(start, end, from_str.length(), at, to_len)
            let buf = @buffer.new()
            for i = 0; i < to_len; {
              // invariant : i >= 0 && i <= to_len
              // decreases : to_len - i
              // assert : i <= to_len
              match to_str.get_char(i) {
                Some(ch) =>
                  if i >= at && i < at + count {
                    let from_idx = start + (i - at)
                    match from_str.get_char(from_idx) {
                      Some(fch) => buf.write_char(fch)
                      None => raise @core.EvalError("index out of range")
                    }
                  } else {
                    buf.write_char(ch)
                  }
                None => raise @core.EvalError("index out of range")
              }
              continue i + 1
            }
            to_ref.val = buf.to_string()
            Value::Void
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 3 to 5 got \{n}")
      }
    Primitive::StringFill =>
      match args.length() {
        2 | 3 | 4 =>
          Some({
            let sref = value_as_string_ref(args[0])
            let fill = value_as_char(args[1])
            let start = if args.length() >= 3 {
              value_as_int_index(args[2])
            } else {
              0
            }
            let end = if args.length() == 4 {
              value_as_int_index(args[3])
            } else {
              sref.val.length()
            }
            let updated = string_replace_range(sref.val, start, end, fill)
            sref.val = updated
            Value::Void
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 2 to 4 got \{n}")
      }
    Primitive::StringToList =>
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
              s.length()
            }
            check_slice_range(start, end, s.length())
            let items : Array[Datum] = []
            for i = start; i < end; {
              // invariant : i >= start && i <= end
              // decreases : end - i
              // assert : i <= end
              match s.get_char(i) {
                Some(ch) => items.push(Datum::Char(ch))
                None => raise @core.EvalError("index out of range")
              }
              continue i + 1
            }
            Value::Datum(list_from_array(items))
          })
        n =>
          raise @core.EvalError("arity mismatch: expected 1 to 3 got \{n}")
      }
    Primitive::ListToString =>
      match args.length() {
        1 =>
          Some({
            let datum = value_as_datum(args[0])
            let items = datum_list_to_array(datum)
            let buf = @buffer.new()
            for item in items {
              // invariant : items.length() >= 0
              // TODO(decreases) : loop index not exposed; possible bug
              // assert : items.length() >= 0
              match item {
                Datum::Char(ch) => buf.write_char(ch)
                _ => raise @core.EvalError("type error: char expected")
              }
            }
            Value::Datum(Datum::String(Ref::new(buf.to_string())))
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringUpcase =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(
              Datum::String(Ref::new(unicode_string(s).upcase().into_string())),
            )
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringDowncase =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(
              Datum::String(Ref::new(unicode_string(s).downcase().into_string())),
            )
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringFoldcase =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(
              Datum::String(Ref::new(unicode_string(s).foldcase().into_string())),
            )
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfc =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfc().into_string()),
              ),
            )
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfd =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfd().into_string()),
              ),
            )
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfkc =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfkc().into_string()),
              ),
            )
          })
        n => raise arity_mismatch(1, n)
      }
    Primitive::StringNormalizeNfkd =>
      match args.length() {
        1 =>
          Some({
            let s = value_as_string(args[0])
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfkd().into_string()),
              ),
            )
          })
        n => raise arity_mismatch(1, n)
      }
    _ => None
  }
}
