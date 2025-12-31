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
      match args {
        [char_value] =>
          Some(Value::Datum(Datum::Int(value_as_char(char_value).to_int())))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::IntegerToChar =>
      match args {
        [int_value] =>
          Some({
            let ch = char_from_int_value(int_value)
            Value::Datum(Datum::Char(ch))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharAlphabeticP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(unicode_char(ch).is_alphabetic())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharNumericP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(ch.is_numeric())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharWhitespaceP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(ch.is_whitespace())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharUpperCaseP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(unicode_char(ch).is_uppercase())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharLowerCaseP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(unicode_char(ch).is_lowercase())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharUpcase =>
      match args {
        [char_value] =>
          Some(
            Value::Datum(
              Datum::Char(unicode_char(value_as_char(char_value)).upcase()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharDowncase =>
      match args {
        [char_value] =>
          Some(
            Value::Datum(
              Datum::Char(unicode_char(value_as_char(char_value)).downcase()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharFoldcase =>
      match args {
        [char_value] =>
          Some(
            Value::Datum(
              Datum::Char(unicode_char(value_as_char(char_value)).foldcase()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::CharGeneralCategory =>
      match args {
        [char_value] =>
          Some(
            Value::Datum(
              Datum::Symbol(unicode_char(value_as_char(char_value)).general_category()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
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
      match args {
        [len_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let zero = 0
            let fill = match zero.to_char() {
              Some(ch) => ch
              None => raise @core.EvalError("internal error: invalid default char")
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
        [len_value, fill_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = value_as_char(fill_value)
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
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    Primitive::StringP =>
      match args {
        [Value::Datum(Datum::String(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringLength =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(Datum::Int(s.length()))
          })
        _ => raise arity_mismatch(1, args.length())
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
      match args {
        [string_value, idx_value] =>
          Some({
            let s = value_as_string(string_value)
            let idx = value_as_int_index(idx_value)
            check_index(idx, s.length())
            match s.get_char(idx) {
              Some(ch) => Value::Datum(Datum::Char(ch))
              None => raise @core.EvalError("index out of range")
            }
          })
        _ => raise arity_mismatch(2, args.length())
      }
    Primitive::StringSet =>
      match args {
        [string_value, idx_value, char_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let idx = value_as_int_index(idx_value)
            let ch = value_as_char(char_value)
            let updated = string_replace_range(sref.val, idx, idx + 1, ch)
            sref.val = updated
            Value::Void
          })
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::StringCopy =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = 0
            let end = s.length()
            check_slice_range(start, end, s.length())
            let view = try? s[start:end]
            match view {
              Ok(slice) =>
                Value::Datum(Datum::String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        [string_value, start_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = value_as_int_index(start_value)
            let end = s.length()
            check_slice_range(start, end, s.length())
            let view = try? s[start:end]
            match view {
              Ok(slice) =>
                Value::Datum(Datum::String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        [string_value, start_value, end_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            check_slice_range(start, end, s.length())
            let view = try? s[start:end]
            match view {
              Ok(slice) =>
                Value::Datum(Datum::String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    Primitive::Substring =>
      match args {
        [string_value, start_value, end_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            check_slice_range(start, end, s.length())
            let view = try? s[start:end]
            match view {
              Ok(slice) =>
                Value::Datum(Datum::String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        _ => raise arity_mismatch(3, args.length())
      }
    Primitive::StringCopyBang =>
      match args {
        [to_value, at_value, from_value] =>
          Some({
            let to_ref = value_as_string_ref(to_value)
            let at = value_as_int_index(at_value)
            let from_str = value_as_string(from_value)
            let start = 0
            let end = from_str.length()
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
        [to_value, at_value, from_value, start_value] =>
          Some({
            let to_ref = value_as_string_ref(to_value)
            let at = value_as_int_index(at_value)
            let from_str = value_as_string(from_value)
            let start = value_as_int_index(start_value)
            let end = from_str.length()
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
        [to_value, at_value, from_value, start_value, end_value] =>
          Some({
            let to_ref = value_as_string_ref(to_value)
            let at = value_as_int_index(at_value)
            let from_str = value_as_string(from_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
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
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 3 to 5 got \{args.length()}",
          )
      }
    Primitive::StringFill =>
      match args {
        [string_value, fill_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let fill = value_as_char(fill_value)
            let start = 0
            let end = sref.val.length()
            let updated = string_replace_range(sref.val, start, end, fill)
            sref.val = updated
            Value::Void
          })
        [string_value, fill_value, start_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let fill = value_as_char(fill_value)
            let start = value_as_int_index(start_value)
            let end = sref.val.length()
            let updated = string_replace_range(sref.val, start, end, fill)
            sref.val = updated
            Value::Void
          })
        [string_value, fill_value, start_value, end_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let fill = value_as_char(fill_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            let updated = string_replace_range(sref.val, start, end, fill)
            sref.val = updated
            Value::Void
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 to 4 got \{args.length()}",
          )
      }
    Primitive::StringToList =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = 0
            let end = s.length()
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
        [string_value, start_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = value_as_int_index(start_value)
            let end = s.length()
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
        [string_value, start_value, end_value] =>
          Some({
            let s = value_as_string(string_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
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
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    Primitive::ListToString =>
      match args {
        [datum_value] =>
          Some({
            let datum = value_as_datum(datum_value)
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
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringUpcase =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(
              Datum::String(Ref::new(unicode_string(s).upcase().into_string())),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringDowncase =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(
              Datum::String(Ref::new(unicode_string(s).downcase().into_string())),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringFoldcase =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(
              Datum::String(Ref::new(unicode_string(s).foldcase().into_string())),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringNormalizeNfc =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfc().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringNormalizeNfd =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfd().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringNormalizeNfkc =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfkc().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    Primitive::StringNormalizeNfkd =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Value::Datum(
              Datum::String(
                Ref::new(unicode_string(s).normalize_nfkd().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    _ => None
  }
}
