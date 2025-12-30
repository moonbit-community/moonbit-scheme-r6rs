///|
using @core {type Datum, type ParseError, unicode_char_foldcase}

///|
pub(all) struct Reader {
  chars : Array[Char]
  mut pos : Int
  mut fold_case : Bool
  labels : Map[Int, Ref[Datum]]
}

///|
/// Create a reader over the given source string.
pub fn make_reader(src : String) -> Reader {
  let chars : Array[Char] = []
  for ch in src {
    chars.push(ch)
  }
  { chars, pos: 0, fold_case: false, labels: Map::new() }
}

///|
/// Peek the next character without consuming it.
pub fn reader_peek(r : Reader) -> Char? {
  if r.pos < r.chars.length() {
    Some(r.chars[r.pos])
  } else {
    None
  }
}

///|
/// Consume and return the next character.
pub fn Reader::next(self : Reader) -> Char? {
  let ch = reader_peek(self)
  match ch {
    Some(_) => self.pos = self.pos + 1
    None => ()
  }
  ch
}

///|
fn is_delim(ch : Char) -> Bool {
  if ch.is_ascii_whitespace() {
    true
  } else {
    ch == '(' ||
    ch == ')' ||
    ch == '\'' ||
    ch == '`' ||
    ch == ',' ||
    ch == '"' ||
    ch == ';'
  }
}

///|
/// Skip whitespace and comments, leaving the next token ready to read.
pub fn skip_ws_and_comments(r : Reader) -> Unit raise ParseError {
  while true {
    match reader_peek(r) {
      Some(ch) if ch.is_ascii_whitespace() => {
        r.pos = r.pos + 1
        continue
      }
      Some(';') => {
        // Line comment
        r.pos = r.pos + 1
        while true {
          match reader_peek(r) {
            Some('\n') => {
              r.pos = r.pos + 1
              break
            }
            Some(_) => {
              r.pos = r.pos + 1
              continue
            }
            None => break
          }
        }
        continue
      }
      Some('#') =>
        if r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '|' {
          skip_block_comment(r)
          continue
        } else {
          break
        }
      _ => break
    }
  }
}

///|
fn skip_block_comment(r : Reader) -> Unit raise ParseError {
  r.pos = r.pos + 2
  let mut depth = 1
  while depth > 0 {
    if r.pos >= r.chars.length() {
      raise @core.ParseError("unterminated block comment")
    }
    if r.chars[r.pos] == '#' &&
      r.pos + 1 < r.chars.length() &&
      r.chars[r.pos + 1] == '|' {
      depth = depth + 1
      r.pos = r.pos + 2
      continue
    }
    if r.chars[r.pos] == '|' &&
      r.pos + 1 < r.chars.length() &&
      r.chars[r.pos + 1] == '#' {
      depth = depth - 1
      r.pos = r.pos + 2
      continue
    }
    r.pos = r.pos + 1
  }
}

///|
/// Return true if the reader is positioned at an ellipsis token.
pub fn is_ellipsis_start(r : Reader) -> Bool {
  if r.pos + 2 >= r.chars.length() {
    false
  } else if r.chars[r.pos] == '.' &&
    r.chars[r.pos + 1] == '.' &&
    r.chars[r.pos + 2] == '.' {
    if r.pos + 3 >= r.chars.length() {
      true
    } else {
      is_delim(r.chars[r.pos + 3])
    }
  } else {
    false
  }
}

///|
/// Return true if the reader is positioned at a vector literal start.
pub fn is_vector_start(r : Reader) -> Bool {
  r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == '('
}

///|
/// Return true if the reader is positioned at a bytevector literal start.
pub fn is_bytevector_start(r : Reader) -> Bool {
  if r.pos + 4 >= r.chars.length() {
    false
  } else {
    let mut ok = r.chars[r.pos + 1] == 'v'
    ok = ok && r.chars[r.pos + 2] == 'u'
    ok = ok && r.chars[r.pos + 3] == '8'
    ok = ok && r.chars[r.pos + 4] == '('
    ok
  }
}

///|
/// Read the next token from the reader.
pub fn read_token(r : Reader) -> String raise ParseError {
  let chars : Array[Char] = []
  let mut in_bar = false
  let mut started = false
  let mut fold_case = r.fold_case
  while true {
    match reader_peek(r) {
      Some(ch) => {
        if !in_bar && is_delim(ch) {
          break
        }
        started = true
        r.pos = r.pos + 1
        if !in_bar && chars.is_empty() && ch == '#' {
          if r.pos < r.chars.length() && r.chars[r.pos] == '\\' {
            fold_case = false
          }
        }
        if in_bar {
          if ch == '|' {
            in_bar = false
          } else if ch == '\\' {
            chars.push(read_identifier_escape(r))
          } else {
            chars.push(ch)
          }
        } else if ch == '|' {
          in_bar = true
        } else if ch == '\\' {
          if chars.length() == 1 && chars[0] == '#' {
            chars.push(ch)
          } else {
            let escaped = read_identifier_escape(r)
            chars.push(
              if fold_case {
                unicode_char_foldcase(escaped)
              } else {
                escaped
              },
            )
          }
        } else {
          let next_char = if fold_case { unicode_char_foldcase(ch) } else { ch }
          chars.push(next_char)
        }
        continue
      }
      _ => break
    }
  }
  if in_bar {
    raise @core.ParseError("unterminated identifier")
  }
  if !started {
    raise @core.ParseError("expected token")
  }
  String::from_array(chars)
}

///|
fn read_identifier_escape(r : Reader) -> Char raise ParseError {
  match r.next() {
    Some('x') | Some('X') => read_hex_escape(r)
    Some(ch) => ch
    None => raise @core.ParseError("unterminated identifier")
  }
}

///|
/// Convert a digit character to its numeric value.
pub fn digit_value(ch : Char) -> Int? {
  if ch.is_ascii_digit() {
    Some(ch.to_int() - '0'.to_int())
  } else if ch.is_ascii_lowercase() {
    Some(ch.to_int() - 'a'.to_int() + 10)
  } else if ch.is_ascii_uppercase() {
    Some(ch.to_int() - 'A'.to_int() + 10)
  } else {
    None
  }
}

///|
/// Read a string literal, handling escapes.
pub fn read_string(r : Reader) -> String raise ParseError {
  let chars : Array[Char] = []
  while true {
    match r.next() {
      None => raise @core.ParseError("unterminated string")
      Some('"') => break
      Some('\\') =>
        match r.next() {
          Some('\n') => skip_string_line_continuation(r)
          Some('\r') => {
            if reader_peek(r) == Some('\n') {
              r.pos = r.pos + 1
            }
            skip_string_line_continuation(r)
          }
          Some('n') => chars.push('\n')
          Some('t') => chars.push('\t')
          Some('r') => chars.push('\r')
          Some('a') => chars.push('\u{7}')
          Some('b') => chars.push('\u{8}')
          Some('v') => chars.push('\u{0b}')
          Some('f') => chars.push('\u{0c}')
          Some('x') | Some('X') => chars.push(read_hex_escape(r))
          Some('"') => chars.push('"')
          Some('\\') => chars.push('\\')
          Some(ch) => chars.push(ch)
          None => raise @core.ParseError("unterminated string")
        }
      Some(ch) => chars.push(ch)
    }
  }
  String::from_array(chars)
}

///|
fn skip_string_line_continuation(r : Reader) -> Unit {
  while true {
    match reader_peek(r) {
      Some(ch) if ch.is_ascii_whitespace() => {
        r.pos = r.pos + 1
        continue
      }
      _ => break
    }
  }
}

///|
fn read_hex_escape(r : Reader) -> Char raise ParseError {
  let mut acc = 0
  let mut has_digit = false
  while true {
    match r.next() {
      None => raise @core.ParseError("unterminated string")
      Some(';') => {
        if !has_digit {
          raise @core.ParseError("invalid hex escape")
        }
        match acc.to_char() {
          Some(ch) => return ch
          None => raise @core.ParseError("invalid hex escape")
        }
      }
      Some(ch) =>
        match digit_value(ch) {
          Some(digit) if digit < 16 => {
            acc = acc * 16 + digit
            has_digit = true
          }
          _ => raise @core.ParseError("invalid hex escape")
        }
    }
  }
  raise @core.ParseError("invalid hex escape")
}
