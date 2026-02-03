///|
pub struct Reader {
  chars : Array[Char]
  mut pos : Int
  mut fold_case : Bool
  labels : Map[Int, Ref[@core.Datum]]
}

///|
/// Create a reader over the given source string.
///
/// # Example
/// ```mbt check
/// test "Reader::new basics" {
///   let r = Reader::new("foo bar")
///   inspect(r.read_token(), content="foo")
/// }
/// ```
pub fn Reader::new(src : String) -> Reader {
  let chars = src.to_array()
  { chars, pos: 0, fold_case: false, labels: Map::new() }
}

///|
/// Peek the next character without consuming it.
///
/// # Example
/// ```mbt check
/// test "peek does not consume" {
///   let r = Reader::new("ab")
///   guard r.peek() is Some('a') else { fail("expected a") }
///   guard r.peek() is Some('a') else { fail("expected a again") }
/// }
/// ```
pub fn Reader::peek(self : Reader) -> Char? {
  self.chars.get(self.pos)
}

///|
/// Peek ahead by a fixed offset without consuming input.
fn Reader::peek_offset(self : Reader, offset : Int) -> Char? {
  self.chars.get(self.pos + offset)
}

///|
/// Peek the next character after the current position without consuming it.
///
/// # Example
/// ```mbt check
/// test "peek_next" {
///   let r = Reader::new("ab")
///   guard r.peek_next() is Some('b') else { fail("expected b") }
///   guard r.peek() is Some('a') else { fail("expected a") }
/// }
/// ```
pub fn Reader::peek_next(self : Reader) -> Char? {
  self.peek_offset(1)
}

///|
/// Advance the reader by a fixed number of characters.
fn Reader::advance(self : Reader, count : Int) -> Unit {
  self.pos = self.pos + count
}

///|
/// Enable or disable case folding for subsequent reads.
pub fn Reader::set_fold_case(self : Reader, value : Bool) -> Unit {
  self.fold_case = value
}

///|
/// Look up a label cell by id.
pub fn Reader::label_get(self : Reader, label : Int) -> Ref[@core.Datum]? {
  self.labels.get(label)
}

///|
/// Store a label cell by id.
///
/// # Example
/// ```mbt check
/// test "label set/get" {
///   let r = Reader::new("x")
///   let cell = Ref::new(Datum::Nil)
///   r.label_set(1, cell)
///   guard r.label_get(1) is Some(_) else { fail("expected label") }
///   guard r.label_get(2) is None else { fail("expected missing label") }
/// }
/// ```
pub fn Reader::label_set(
  self : Reader,
  label : Int,
  cell : Ref[@core.Datum],
) -> Unit {
  self.labels[label] = cell
}

///|
/// Consume and return the next character.
///
/// # Example
/// ```mbt check
/// test "next advances reader" {
///   let r = Reader::new("ab")
///   guard r.next() is Some('a') else { fail("expected a") }
///   guard r.next() is Some('b') else { fail("expected b") }
///   guard r.next() is None else { fail("expected eof") }
/// }
/// ```
pub fn Reader::next(self : Reader) -> Char? {
  let ch = self.peek()
  match ch {
    Some(_) => self.advance(1)
    None => ()
  }
  ch
}

///|
/// Return true if a character is a token delimiter.
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
///   let r = Reader::new("  ; hi\nfoo")
///   r.skip_ws_and_comments()
///   guard r.peek() is Some('f') else { fail("expected f") }
/// }
/// ```
pub fn Reader::skip_ws_and_comments(
  self : Reader,
) -> Unit raise @core.ParseError {
  for cur = self.peek(); true; {
    // invariant : self.pos >= 0 && self.pos <= self.chars.length()
    // decreases : self.chars.length() - self.pos
    // assert : self.pos <= self.chars.length()
    match cur {
      Some(ch) if ch.is_ascii_whitespace() => {
        ignore(self.next())
        continue self.peek()
      }
      Some(';') => {
        // Line comment
        ignore(self.next())
        self.skip_line_comment()
        continue self.peek()
      }
      Some('#') =>
        if self.peek_offset(1) == Some('|') {
          self.skip_block_comment()
          continue self.peek()
        } else {
          break
        }
      _ => break
    }
  } else {
    ()
  }
}

///|
/// Consume a line comment after `;`.
fn Reader::skip_line_comment(self : Reader) -> Unit {
  for cur = self.next(); true; {
    // invariant : self.pos >= 0 && self.pos <= self.chars.length()
    // decreases : self.chars.length() - self.pos
    // assert : self.pos <= self.chars.length()
    match cur {
      Some('\n') => break
      Some(_) => continue self.next()
      None => break
    }
  } else {
    ()
  }
}

///|
/// Consume a nested block comment starting with `#|`.
fn Reader::skip_block_comment(self : Reader) -> Unit raise @core.ParseError {
  ignore(self.next())
  ignore(self.next())
  for depth = 1; depth > 0; {
    // invariant : depth > 0
    // TODO(decreases) : depth can increase; possible bug
    // assert : depth > 0
    match (self.peek(), self.peek_offset(1)) {
      (None, _) => raise ParseError("unterminated block comment")
      (Some('#'), Some('|')) => {
        ignore(self.next())
        ignore(self.next())
        continue depth + 1
      }
      (Some('|'), Some('#')) => {
        ignore(self.next())
        ignore(self.next())
        continue depth - 1
      }
      _ => {
        ignore(self.next())
        continue depth
      }
    }
  } else {
    ()
  }
}

///|
/// Return true if the reader is positioned at an ellipsis token.
///
/// # Example
/// ```mbt check
/// test "ellipsis start" {
///   let r = Reader::new("...")
///   inspect(r.is_ellipsis_start(), content="true")
/// }
/// ```
pub fn Reader::is_ellipsis_start(self : Reader) -> Bool {
  match (self.peek(), self.peek_offset(1), self.peek_offset(2)) {
    (Some('.'), Some('.'), Some('.')) =>
      match self.peek_offset(3) {
        None => true
        Some(ch) => is_delim(ch)
      }
    _ => false
  }
}

///|
/// Read the next token from the reader.
///
/// # Example
/// ```mbt check
/// test "read token" {
///   let r = Reader::new("alpha beta")
///   inspect(r.read_token(), content="alpha")
/// }
/// ```
pub fn Reader::read_token(self : Reader) -> String raise @core.ParseError {
  let chars : Array[Char] = []
  let (in_bar, started) = for in_bar = false, started = false, fold_case = self.fold_case; true; {
    // invariant : self.pos >= 0 && self.pos <= self.chars.length()
    // decreases : self.chars.length() - self.pos
    // assert : !in_bar || started
    match self.peek() {
      Some(ch) => {
        if !in_bar && is_delim(ch) {
          break (in_bar, started)
        }
        let started = true
        ignore(self.next())
        let fold_case = if !in_bar && chars.is_empty() && ch == '#' {
          if self.peek() == Some('\\') {
            false
          } else {
            fold_case
          }
        } else {
          fold_case
        }
        if in_bar {
          if ch == '|' {
            continue false, started, fold_case
          } else if ch == '\\' {
            chars.push(self.read_identifier_escape())
            continue in_bar, started, fold_case
          } else {
            chars.push(ch)
            continue in_bar, started, fold_case
          }
        } else if ch == '|' {
          continue true, started, fold_case
        } else if ch == '\\' {
          match chars {
            ['#'] => chars.push(ch)
            _ => {
              let escaped = self.read_identifier_escape()
              chars.push(
                if fold_case {
                  @core.UnicodeChar::new(escaped).foldcase()
                } else {
                  escaped
                },
              )
            }
          }
          continue in_bar, started, fold_case
        } else {
          let next_char = if fold_case {
            @core.UnicodeChar::new(ch).foldcase()
          } else {
            ch
          }
          chars.push(next_char)
          continue in_bar, started, fold_case
        }
      }
      _ => break (in_bar, started)
    }
  } else {
    (false, false)
  }
  if in_bar {
    raise ParseError("unterminated identifier")
  }
  if !started {
    raise ParseError("expected token")
  }
  String::from_array(chars)
}

///|
/// Read an escaped identifier character (supports `\x..;`).
fn Reader::read_identifier_escape(self : Reader) -> Char raise @core.ParseError {
  match self.next() {
    Some('x') | Some('X') => self.read_hex_escape()
    Some(ch) => ch
    None => raise ParseError("unterminated identifier")
  }
}

///|
/// Convert a digit character to its numeric value.
fn digit_value(ch : Char) -> Int? {
  match ch {
    '0'..='9' => Some(ch.to_int() - '0'.to_int())
    'a'..='f' => Some(ch.to_int() - 'a'.to_int() + 10)
    'A'..='F' => Some(ch.to_int() - 'A'.to_int() + 10)
    _ => None
  }
}

///|
/// Read a string literal, handling escapes.
///
/// # Example
/// ```mbt check
/// test "read string literal" {
///   let r = Reader::new("\"hi\"")
///   ignore(r.next()) // consume opening quote
///   inspect(r.read_string(), content="hi")
/// }
/// ```
pub fn Reader::read_string(self : Reader) -> String raise @core.ParseError {
  let chars : Array[Char] = []
  for cur = self.next(); true; {
    // invariant : self.pos >= 0 && self.pos <= self.chars.length()
    // decreases : self.chars.length() - self.pos
    // assert : self.pos <= self.chars.length()
    match cur {
      None => raise ParseError("unterminated string")
      Some('"') => break
      Some('\\') => {
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
          None => raise ParseError("unterminated string")
        }
        continue self.next()
      }
      Some(ch) => {
        chars.push(ch)
        continue self.next()
      }
    }
  } else {
    ()
  }
  String::from_array(chars)
}

///|
/// Skip trailing whitespace after a string line continuation.
fn Reader::skip_string_line_continuation(self : Reader) -> Unit {
  for cur = self.peek(); true; {
    // invariant : self.pos >= 0 && self.pos <= self.chars.length()
    // decreases : self.chars.length() - self.pos
    // assert : self.pos <= self.chars.length()
    match cur {
      Some(ch) if ch.is_ascii_whitespace() => {
        ignore(self.next())
        continue self.peek()
      }
      _ => break
    }
  } else {
    ()
  }
}

///|
/// Read a `\x..;` hex escape, requiring at least one hex digit.
fn Reader::read_hex_escape(self : Reader) -> Char raise @core.ParseError {
  for acc = 0, has_digit = false; true; {
    // invariant : acc >= 0
    // decreases : self.chars.length() - self.pos
    // assert : acc >= 0
    match self.next() {
      None => raise ParseError("unterminated string")
      Some(';') => {
        if !has_digit {
          raise ParseError("invalid hex escape")
        }
        match acc.to_char() {
          Some(ch) => break ch
          None => raise ParseError("invalid hex escape")
        }
      }
      Some(ch) =>
        match digit_value(ch) {
          Some(digit) if digit < 16 => continue acc * 16 + digit, true
          _ => raise ParseError("invalid hex escape")
        }
    }
  } else {
    raise ParseError("invalid hex escape")
  }
}
