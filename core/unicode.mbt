///|
fn char_in_ranges(ch : Char, ranges : ReadOnlyArray[Char]) -> Bool {
  for left = 0, right = ranges.length() / 2 - 1; left <= right; {
    // invariant : left >= 0 && right >= 0 && right < ranges.length() / 2
    // decreases : right - left + 1
    // assert : left <= right
    let mid = (left + right) / 2
    let start = ranges[mid * 2]
    let end = ranges[mid * 2 + 1]
    if ch < start {
      continue left, mid - 1
    } else if ch > end {
      continue mid + 1, right
    } else {
      return true
    }
  } else {
    false
  }
}

///|
fn unicode_category_has(ch : Char, category : String) -> Bool {
  match category {
    "L" => char_in_ranges(ch, unicode_ranges_L)
    "Nl" => char_in_ranges(ch, unicode_ranges_NL)
    "Lu" => char_in_ranges(ch, unicode_ranges_LU)
    "Ll" => char_in_ranges(ch, unicode_ranges_LL)
    "Lt" => char_in_ranges(ch, unicode_ranges_LT)
    _ => false
  }
}

///|
pub struct UnicodeChar {
  value : Char
}

///|
/// Wrap a `Char` to access Unicode helpers with chaining.
///
/// # Example
/// ```mbt check
/// test "unicode char wrapper" {
///   inspect(unicode_char('A').is_uppercase(), content="true")
/// }
/// ```
pub fn unicode_char(ch : Char) -> UnicodeChar {
  { value: ch }
}

///|
pub struct UnicodeString {
  value : String
}

///|
/// Wrap a `String` to access Unicode helpers with chaining.
///
/// # Example
/// ```mbt check
/// test "unicode string wrapper" {
///   inspect(
///     unicode_string("e\u{301}").normalize_nfc().foldcase().into_string(),
///     content="\u{00e9}",
///   )
/// }
/// ```
pub fn unicode_string(s : String) -> UnicodeString {
  { value: s }
}

///|
/// Unwrap a Unicode string wrapper back into a `String`.
///
/// # Example
/// ```mbt check
/// test "unicode string into_string" {
///   let wrapped = unicode_string("Hi")
///   inspect(wrapped.into_string(), content="Hi")
/// }
/// ```
pub fn UnicodeString::into_string(self : UnicodeString) -> String {
  self.value
}

///|
/// Return true if the character is alphabetic.
///
/// # Example
/// ```mbt check
/// test "alphabetic predicate" {
///   inspect(unicode_char('A').is_alphabetic(), content="true")
///   inspect(unicode_char('1').is_alphabetic(), content="false")
/// }
/// ```
pub fn UnicodeChar::is_alphabetic(self : UnicodeChar) -> Bool {
  let ch = self.value
  unicode_category_has(ch, "L") || unicode_category_has(ch, "Nl")
}

///|
/// Return true if the character is uppercase.
///
/// # Example
/// ```mbt check
/// test "uppercase predicate" {
///   inspect(unicode_char('A').is_uppercase(), content="true")
///   inspect(unicode_char('a').is_uppercase(), content="false")
/// }
/// ```
pub fn UnicodeChar::is_uppercase(self : UnicodeChar) -> Bool {
  unicode_category_has(self.value, "Lu")
}

///|
/// Return true if the character is lowercase.
///
/// # Example
/// ```mbt check
/// test "lowercase predicate" {
///   inspect(unicode_char('a').is_lowercase(), content="true")
///   inspect(unicode_char('A').is_lowercase(), content="false")
/// }
/// ```
pub fn UnicodeChar::is_lowercase(self : UnicodeChar) -> Bool {
  unicode_category_has(self.value, "Ll")
}

///|
let unicode_general_category_order : ReadOnlyArray[String] = [
  "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps",
  "Pe", "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Cs",
  "Co", "Cn",
]

///|
/// Return the general category tag for the character.
///
/// # Example
/// ```mbt check
/// test "general category" {
///   inspect(unicode_char('A').general_category(), content="Lu")
/// }
/// ```
pub fn UnicodeChar::general_category(self : UnicodeChar) -> String {
  let ch = self.value
  for category in unicode_general_category_order {
    match general_category_ranges.get(category) {
      Some(ranges) => if char_in_ranges(ch, ranges) { return category }
      None => ()
    }
  }
  "Cn"
}

///|
///|
fn unicode_case_equivalents(ch : Char) -> Array[Char] {
  let out : Array[Char] = [ch]
  for cur = ch; true; {
    // invariant : out.length() >= 1
    // TODO(decreases) : cycle detection uses out; possible bug
    // assert : out.length() >= 1
    match case_folding.get(cur) {
      Some(next) => {
        if out.contains(next) {
          break
        }
        out.push(next)
        continue next
      }
      None => break
    }
  } else {
    ()
  }
  out
}

///|
fn unicode_pick_equivalent_category(
  chars : Array[Char],
  category : String,
) -> Char? {
  let ranges = match category {
    "Lu" => unicode_ranges_LU
    "Ll" => unicode_ranges_LL
    "Lt" => unicode_ranges_LT
    _ => return None
  }
  for ch in chars {
    if char_in_ranges(ch, ranges) {
      return Some(ch)
    }
  }
  None
}

///|
fn unicode_min_equivalent(chars : Array[Char], fallback : Char) -> Char {
  if chars.length() == 0 {
    return fallback
  }
  for i = 1, min = chars[0]; i < chars.length(); {
    // invariant : i >= 1 && i <= chars.length()
    // decreases : chars.length() - i
    // assert : i < chars.length()
    let ch = chars[i]
    if ch < min {
      continue i + 1, ch
    }
    continue i + 1, min
  } else {
    min
  }
}

///|
/// Uppercase a character using Unicode case mapping.
///
/// # Example
/// ```mbt check
/// test "char upcase" {
///   inspect(unicode_char('a').upcase(), content="A")
/// }
/// ```
pub fn UnicodeChar::upcase(self : UnicodeChar) -> Char {
  let ch = self.value
  let equiv = unicode_case_equivalents(ch)
  match unicode_pick_equivalent_category(equiv, "Lu") {
    Some(upper) => upper
    None =>
      match unicode_pick_equivalent_category(equiv, "Lt") {
        Some(title) => title
        None => ch
      }
  }
}

///|
/// Lowercase a character using Unicode case mapping.
///
/// # Example
/// ```mbt check
/// test "char downcase" {
///   inspect(unicode_char('A').downcase(), content="a")
/// }
/// ```
pub fn UnicodeChar::downcase(self : UnicodeChar) -> Char {
  let ch = self.value
  let equiv = unicode_case_equivalents(ch)
  match unicode_pick_equivalent_category(equiv, "Ll") {
    Some(lower) => lower
    None => ch
  }
}

///|
/// Case-fold a character for case-insensitive comparison.
///
/// # Example
/// ```mbt check
/// test "char foldcase" {
///   inspect(unicode_char('A').foldcase(), content="a")
/// }
/// ```
pub fn UnicodeChar::foldcase(self : UnicodeChar) -> Char {
  let ch = self.value
  let equiv = unicode_case_equivalents(ch)
  match unicode_pick_equivalent_category(equiv, "Ll") {
    Some(lower) => lower
    None => unicode_min_equivalent(equiv, ch)
  }
}

///|
/// Uppercase a string using Unicode case mapping.
///
/// # Example
/// ```mbt check
/// test "string upcase" {
///   inspect(unicode_string("abc").upcase().into_string(), content="ABC")
/// }
/// ```
pub fn UnicodeString::upcase(self : UnicodeString) -> UnicodeString {
  let buf = @buffer.new()
  for ch in self.value {
    buf.write_char(unicode_char(ch).upcase())
  }
  { value: buf.to_string() }
}

///|
/// Lowercase a string using Unicode case mapping.
///
/// # Example
/// ```mbt check
/// test "string downcase" {
///   inspect(unicode_string("ABC").downcase().into_string(), content="abc")
/// }
/// ```
pub fn UnicodeString::downcase(self : UnicodeString) -> UnicodeString {
  let buf = @buffer.new()
  for ch in self.value {
    buf.write_char(unicode_char(ch).downcase())
  }
  { value: buf.to_string() }
}

///|
/// Case-fold a string for case-insensitive comparison.
///
/// # Example
/// ```mbt check
/// test "string foldcase" {
///   inspect(unicode_string("ABC").foldcase().into_string(), content="abc")
/// }
/// ```
pub fn UnicodeString::foldcase(self : UnicodeString) -> UnicodeString {
  let buf = @buffer.new()
  for ch in self.value {
    buf.write_char(unicode_char(ch).foldcase())
  }
  { value: buf.to_string() }
}
