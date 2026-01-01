///|
/// Apply character and string primitives.
fn apply_char_string_primitive(
  prim : Primitive,
  args : Array[Value],
) -> Value? raise EvalError {
  match prim {
    CharEq =>
      Some(bool_value(compare_chain_char(args, Eq, false)))
    CharLess =>
      Some(bool_value(compare_chain_char(args, Lt, false)))
    CharGreater =>
      Some(bool_value(compare_chain_char(args, Gt, false)))
    CharLessEq =>
      Some(bool_value(compare_chain_char(args, Le, false)))
    CharGreaterEq =>
      Some(bool_value(compare_chain_char(args, Ge, false)))
    CharCiEq =>
      Some(bool_value(compare_chain_char(args, Eq, true)))
    CharCiLess =>
      Some(bool_value(compare_chain_char(args, Lt, true)))
    CharCiGreater =>
      Some(bool_value(compare_chain_char(args, Gt, true)))
    CharCiLessEq =>
      Some(bool_value(compare_chain_char(args, Le, true)))
    CharCiGreaterEq =>
      Some(bool_value(compare_chain_char(args, Ge, true)))
    CharP =>
      match args {
        [Datum(Char(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    CharToInteger =>
      match args {
        [char_value] =>
          Some(Datum(Int(value_as_char(char_value).to_int())))
        _ => raise arity_mismatch(1, args.length())
      }
    IntegerToChar =>
      match args {
        [int_value] =>
          Some({
            let ch = char_from_int_value(int_value)
            Datum(Char(ch))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    CharAlphabeticP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(unicode_char(ch).is_alphabetic())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    CharNumericP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(ch.is_numeric())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    CharWhitespaceP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(ch.is_whitespace())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    CharUpperCaseP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(unicode_char(ch).is_uppercase())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    CharLowerCaseP =>
      match args {
        [char_value] =>
          Some({
            let ch = value_as_char(char_value)
            bool_value(unicode_char(ch).is_lowercase())
          })
        _ => raise arity_mismatch(1, args.length())
      }
    CharUpcase =>
      match args {
        [char_value] =>
          Some(
            Datum(
              Char(unicode_char(value_as_char(char_value)).upcase()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    CharDowncase =>
      match args {
        [char_value] =>
          Some(
            Datum(
              Char(unicode_char(value_as_char(char_value)).downcase()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    CharFoldcase =>
      match args {
        [char_value] =>
          Some(
            Datum(
              Char(unicode_char(value_as_char(char_value)).foldcase()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    CharGeneralCategory =>
      match args {
        [char_value] =>
          Some(
            Datum(
              Symbol(unicode_char(value_as_char(char_value)).general_category()),
            ),
          )
        _ => raise arity_mismatch(1, args.length())
      }
    StringEq =>
      Some(bool_value(compare_chain_string(args, Eq, false)))
    StringLess =>
      Some(bool_value(compare_chain_string(args, Lt, false)))
    StringGreater =>
      Some(bool_value(compare_chain_string(args, Gt, false)))
    StringLessEq =>
      Some(bool_value(compare_chain_string(args, Le, false)))
    StringGreaterEq =>
      Some(bool_value(compare_chain_string(args, Ge, false)))
    StringCiEq =>
      Some(bool_value(compare_chain_string(args, Eq, true)))
    StringCiLess =>
      Some(bool_value(compare_chain_string(args, Lt, true)))
    StringCiGreater =>
      Some(bool_value(compare_chain_string(args, Gt, true)))
    StringCiLessEq =>
      Some(bool_value(compare_chain_string(args, Le, true)))
    StringCiGreaterEq =>
      Some(bool_value(compare_chain_string(args, Ge, true)))
    String => {
      let chars : Array[Char] = args.map((arg) => value_as_char(arg))
      Some(Datum(String(Ref::new(String::from_array(chars)))))
    }
    MakeString =>
      match args {
        [len_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let zero = 0
            let fill = match zero.to_char() {
              Some(ch) => ch
              None => raise @core.EvalError("internal error: invalid default char")
            }
            let repeated = String::from_array([fill]).repeat(len)
            Datum(String(Ref::new(repeated)))
          })
        [len_value, fill_value] =>
          Some({
            let len = value_as_nonnegative_int(len_value)
            let fill = value_as_char(fill_value)
            let repeated = String::from_array([fill]).repeat(len)
            Datum(String(Ref::new(repeated)))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 or 2 got \{args.length()}",
          )
      }
    StringP =>
      match args {
        [Datum(String(_))] => Some(bool_value(true))
        [_] => Some(bool_value(false))
        _ => raise arity_mismatch(1, args.length())
      }
    StringLength =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(Int(s.length()))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringAppend => {
      let items : Array[String] = args.map((arg) => value_as_string(arg))
      Some(Datum(String(Ref::new(items.join("")))))
    }
    StringRef =>
      match args {
        [string_value, idx_value] =>
          Some({
            let s = value_as_string(string_value)
            let idx = value_as_int_index(idx_value)
            check_index(idx, s.length())
            match s.get_char(idx) {
              Some(ch) => Datum(Char(ch))
              None => raise @core.EvalError("index out of range")
            }
          })
        _ => raise arity_mismatch(2, args.length())
      }
    StringSet =>
      match args {
        [string_value, idx_value, char_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let idx = value_as_int_index(idx_value)
            let ch = value_as_char(char_value)
            let updated = string_replace_range(sref.val, idx, idx + 1, ch)
            sref.val = updated
            Void
          })
        _ => raise arity_mismatch(3, args.length())
      }
    StringCopy =>
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
                Datum(String(Ref::new(slice.to_string())))
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
                Datum(String(Ref::new(slice.to_string())))
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
                Datum(String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    Substring =>
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
                Datum(String(Ref::new(slice.to_string())))
              Err(_) => raise @core.EvalError("index out of range")
            }
          })
        _ => raise arity_mismatch(3, args.length())
      }
    StringCopyBang =>
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
            Void
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
            Void
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
            Void
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 3 to 5 got \{args.length()}",
          )
      }
    StringFill =>
      match args {
        [string_value, fill_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let fill = value_as_char(fill_value)
            let start = 0
            let end = sref.val.length()
            let updated = string_replace_range(sref.val, start, end, fill)
            sref.val = updated
            Void
          })
        [string_value, fill_value, start_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let fill = value_as_char(fill_value)
            let start = value_as_int_index(start_value)
            let end = sref.val.length()
            let updated = string_replace_range(sref.val, start, end, fill)
            sref.val = updated
            Void
          })
        [string_value, fill_value, start_value, end_value] =>
          Some({
            let sref = value_as_string_ref(string_value)
            let fill = value_as_char(fill_value)
            let start = value_as_int_index(start_value)
            let end = value_as_int_index(end_value)
            let updated = string_replace_range(sref.val, start, end, fill)
            sref.val = updated
            Void
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 2 to 4 got \{args.length()}",
          )
      }
    StringToList =>
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
                Some(ch) => items.push(Char(ch))
                None => raise @core.EvalError("index out of range")
              }
              continue i + 1
            }
            Datum(list_from_array(items))
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
                Some(ch) => items.push(Char(ch))
                None => raise @core.EvalError("index out of range")
              }
              continue i + 1
            }
            Datum(list_from_array(items))
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
                Some(ch) => items.push(Char(ch))
                None => raise @core.EvalError("index out of range")
              }
              continue i + 1
            }
            Datum(list_from_array(items))
          })
        _ =>
          raise @core.EvalError(
            "arity mismatch: expected 1 to 3 got \{args.length()}",
          )
      }
    ListToString =>
      match args {
        [datum_value] =>
          Some({
            let datum = value_as_datum(datum_value)
            let items = datum_list_to_array(datum)
            let buf = @buffer.new()
            for item in items {
              match item {
                Char(ch) => buf.write_char(ch)
                _ => raise @core.EvalError("type error: char expected")
              }
            }
            Datum(String(Ref::new(buf.to_string())))
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringUpcase =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(
              String(Ref::new(unicode_string(s).upcase().into_string())),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringDowncase =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(
              String(Ref::new(unicode_string(s).downcase().into_string())),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringFoldcase =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(
              String(Ref::new(unicode_string(s).foldcase().into_string())),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringNormalizeNfc =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(
              String(
                Ref::new(unicode_string(s).normalize_nfc().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringNormalizeNfd =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(
              String(
                Ref::new(unicode_string(s).normalize_nfd().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringNormalizeNfkc =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(
              String(
                Ref::new(unicode_string(s).normalize_nfkc().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    StringNormalizeNfkd =>
      match args {
        [string_value] =>
          Some({
            let s = value_as_string(string_value)
            Datum(
              String(
                Ref::new(unicode_string(s).normalize_nfkd().into_string()),
              ),
            )
          })
        _ => raise arity_mismatch(1, args.length())
      }
    _ => None
  }
}
