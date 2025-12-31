///|
using @core {type Datum, type ParseError, unicode_char}

///|
pub(all) struct Reader {
  chars : Array[Char]
  mut pos : Int
  mut fold_case : Bool
  labels : Map[Int, Ref[Datum]]
}

///|
/// Create a reader over the given source string.
///
/// # Example
/// ```mbt check
/// test "make_reader basics" {
///   let r = make_reader("foo bar")
///   inspect(r.read_token(), content="foo")
/// }
/// ```
pub fn make_reader(src : String) -> Reader {
  let chars = src.to_array()
  { chars, pos: 0, fold_case: false, labels: Map::new() }
}

///|
/// Peek the next character without consuming it.
///
/// # Example
/// ```mbt check
/// test "peek does not consume" {
///   let r = make_reader("ab")
///   guard r.peek() is Some('a') else { fail("expected a") }
///   guard r.peek() is Some('a') else { fail("expected a again") }
/// }
/// ```
pub fn Reader::peek(self : Reader) -> Char? {
  self.chars.get(self.pos)
}

///|
fn Reader::peek_offset(self : Reader, offset : Int) -> Char? {
  self.chars.get(self.pos + offset)
}

///|
/// Consume and return the next character.
///
/// # Example
/// ```mbt check
/// test "next advances reader" {
///   let r = make_reader("ab")
///   guard r.next() is Some('a') else { fail("expected a") }
///   guard r.next() is Some('b') else { fail("expected b") }
///   guard r.next() is None else { fail("expected eof") }
/// }
/// ```
pub fn Reader::next(self : Reader) -> Char? {
  let ch = self.peek()
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
///
/// # Example
/// ```mbt check
/// test "skip whitespace and comments" {
///   let r = make_reader("  ; hi\nfoo")
///   r.skip_ws_and_comments()
///   guard r.peek() is Some('f') else { fail("expected f") }
/// }
/// ```
pub fn Reader::skip_ws_and_comments(self : Reader) -> Unit raise ParseError {
  while true {
    match self.peek() {
      Some(ch) if ch.is_ascii_whitespace() => {
        ignore(self.next())
        continue
      }
      Some(';') => {
        // Line comment
        ignore(self.next())
        while true {
          match self.next() {
            Some('\n') => break
            Some(_) => continue
            None => break
          }
        }
        continue
      }
      Some('#') =>
        if self.peek_offset(1) == Some('|') {
          self.skip_block_comment()
          continue
        } else {
          break
        }
      _ => break
    }
  }
}

///|
fn Reader::skip_block_comment(self : Reader) -> Unit raise ParseError {
  ignore(self.next())
  ignore(self.next())
  let mut depth = 1
  while depth > 0 {
    match (self.peek(), self.peek_offset(1)) {
      (None, _) => raise @core.ParseError("unterminated block comment")
      (Some('#'), Some('|')) => {
        depth = depth + 1
        ignore(self.next())
        ignore(self.next())
        continue
      }
      (Some('|'), Some('#')) => {
        depth = depth - 1
        ignore(self.next())
        ignore(self.next())
        continue
      }
      _ => {
        ignore(self.next())
        continue
      }
    }
  }
}

///|
/// Return true if the reader is positioned at an ellipsis token.
///
/// # Example
/// ```mbt check
/// test "ellipsis start" {
///   let r = make_reader("...")
///   inspect(r.is_ellipsis_start(), content="true")
/// }
/// ```
pub fn Reader::is_ellipsis_start(self : Reader) -> Bool {
  match (
    self.peek(),
    self.peek_offset(1),
    self.peek_offset(2),
  ) {
    (Some('.'), Some('.'), Some('.')) =>
      match self.peek_offset(3) {
        None => true
        Some(ch) => is_delim(ch)
      }
    _ => false
  }
}

///|
/// Return true if the reader is positioned at a vector literal start.
///
/// # Example
/// ```mbt check
/// test "vector start" {
///   let r = make_reader("#(1)")
///   inspect(r.is_vector_start(), content="true")
/// }
/// ```
pub fn Reader::is_vector_start(self : Reader) -> Bool {
  self.peek_offset(1) == Some('(')
}

///|
/// Return true if the reader is positioned at a bytevector literal start.
///
/// # Example
/// ```mbt check
/// test "bytevector start" {
///   let r = make_reader("#vu8(1)")
///   inspect(r.is_bytevector_start(), content="true")
/// }
/// ```
pub fn Reader::is_bytevector_start(self : Reader) -> Bool {
  match (
    self.peek_offset(1),
    self.peek_offset(2),
    self.peek_offset(3),
    self.peek_offset(4),
  ) {
    (Some('v'), Some('u'), Some('8'), Some('(')) => true
    _ => false
  }
}

///|
/// Read the next token from the reader.
///
/// # Example
/// ```mbt check
/// test "read token" {
///   let r = make_reader("alpha beta")
///   inspect(r.read_token(), content="alpha")
/// }
/// ```
pub fn Reader::read_token(self : Reader) -> String raise ParseError {
  let chars : Array[Char] = []
  let mut in_bar = false
  let mut started = false
  let mut fold_case = self.fold_case
  while true {
    match self.peek() {
      Some(ch) => {
        if !in_bar && is_delim(ch) {
          break
        }
        started = true
        ignore(self.next())
        if !in_bar && chars.is_empty() && ch == '#' {
          if self.peek() == Some('\\') {
            fold_case = false
          }
        }
        if in_bar {
          if ch == '|' {
            in_bar = false
          } else if ch == '\\' {
            chars.push(self.read_identifier_escape())
          } else {
            chars.push(ch)
          }
        } else if ch == '|' {
          in_bar = true
        } else if ch == '\\' {
          if chars.length() == 1 && chars[0] == '#' {
            chars.push(ch)
          } else {
            let escaped = self.read_identifier_escape()
            chars.push(
              if fold_case {
                unicode_char(escaped).foldcase()
              } else {
                escaped
              },
            )
          }
        } else {
          let next_char = if fold_case { unicode_char(ch).foldcase() } else { ch }
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
fn Reader::read_identifier_escape(self : Reader) -> Char raise ParseError {
  match self.next() {
    Some('x') | Some('X') => self.read_hex_escape()
    Some(ch) => ch
    None => raise @core.ParseError("unterminated identifier")
  }
}

///|
/// Convert a digit character to its numeric value.
///
/// # Example
/// ```mbt check
/// test "digit value" {
///   inspect(digit_value('9'), content="Some(9)")
///   inspect(digit_value('a'), content="Some(10)")
/// }
/// ```
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
///
/// # Example
/// ```mbt check
/// test "read string literal" {
///   let r = make_reader("\"hi\"")
///   ignore(r.next()) // consume opening quote
///   inspect(r.read_string(), content="hi")
/// }
/// ```
pub fn Reader::read_string(self : Reader) -> String raise ParseError {
  let chars : Array[Char] = []
  while true {
    match self.next() {
      None => raise @core.ParseError("unterminated string")
      Some('"') => break
      Some('\\') =>
        match self.next() {
          Some('\n') => self.skip_string_line_continuation()
          Some('\r') => {
            if self.peek() == Some('\n') {
              ignore(self.next())
            }
            self.skip_string_line_continuation()
          }
          Some('n') => chars.push('\n')
          Some('t') => chars.push('\t')
          Some('r') => chars.push('\r')
          Some('a') => chars.push('\u{7}')
          Some('b') => chars.push('\u{8}')
          Some('v') => chars.push('\u{0b}')
          Some('f') => chars.push('\u{0c}')
          Some('x') | Some('X') => chars.push(self.read_hex_escape())
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
fn Reader::skip_string_line_continuation(self : Reader) -> Unit {
  while true {
    match self.peek() {
      Some(ch) if ch.is_ascii_whitespace() => {
        ignore(self.next())
        continue
      }
      _ => break
    }
  }
}

///|
fn Reader::read_hex_escape(self : Reader) -> Char raise ParseError {
  let mut acc = 0
  let mut has_digit = false
  while true {
    match self.next() {
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
