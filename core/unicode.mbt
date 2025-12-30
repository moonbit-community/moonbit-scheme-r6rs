///|
fn char_in_ranges(ch : Char, ranges : ReadOnlyArray[Char]) -> Bool {
  let mut left = 0
  let mut right = ranges.length() / 2 - 1
  while left <= right {
    let mid = (left + right) / 2
    let start = ranges[mid * 2]
    let end = ranges[mid * 2 + 1]
    if ch < start {
      right = mid - 1
    } else if ch > end {
      left = mid + 1
    } else {
      return true
    }
  }
  false
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
/// Return true if the character is alphabetic.
///
/// # Example
/// ```mbt check
/// test "alphabetic predicate" {
///   inspect(unicode_char_is_alphabetic('A'), content="true")
///   inspect(unicode_char_is_alphabetic('1'), content="false")
/// }
/// ```
pub fn unicode_char_is_alphabetic(ch : Char) -> Bool {
  unicode_category_has(ch, "L") || unicode_category_has(ch, "Nl")
}

///|
/// Return true if the character is uppercase.
pub fn unicode_char_is_uppercase(ch : Char) -> Bool {
  unicode_category_has(ch, "Lu")
}

///|
/// Return true if the character is lowercase.
pub fn unicode_char_is_lowercase(ch : Char) -> Bool {
  unicode_category_has(ch, "Ll")
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
///   inspect(unicode_char_general_category('A'), content="Lu")
/// }
/// ```
pub fn unicode_char_general_category(ch : Char) -> String {
  for category in unicode_general_category_order {
    match general_category_ranges.get(category) {
      Some(ranges) => if char_in_ranges(ch, ranges) { return category }
      None => ()
    }
  }
  "Cn"
}

///|
fn array_char_contains(values : Array[Char], target : Char) -> Bool {
  for item in values {
    if item == target {
      return true
    }
  }
  false
}

///|
fn unicode_case_equivalents(ch : Char) -> Array[Char] {
  let out : Array[Char] = [ch]
  let mut cur = ch
  while case_folding.get(cur) is Some(next) {
    if array_char_contains(out, next) {
      break
    }
    out.push(next)
    cur = next
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
  let mut min = chars[0]
  let mut i = 1
  while i < chars.length() {
    let ch = chars[i]
    if ch < min {
      min = ch
    }
    i = i + 1
  }
  min
}

///|
/// Uppercase a character using Unicode case mapping.
///
/// # Example
/// ```mbt check
/// test "char upcase" {
///   inspect(unicode_char_upcase('a'), content="A")
/// }
/// ```
pub fn unicode_char_upcase(ch : Char) -> Char {
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
///   inspect(unicode_char_downcase('A'), content="a")
/// }
/// ```
pub fn unicode_char_downcase(ch : Char) -> Char {
  let equiv = unicode_case_equivalents(ch)
  match unicode_pick_equivalent_category(equiv, "Ll") {
    Some(lower) => lower
    None => ch
  }
}

///|
/// Case-fold a character for case-insensitive comparison.
pub fn unicode_char_foldcase(ch : Char) -> Char {
  let equiv = unicode_case_equivalents(ch)
  match unicode_pick_equivalent_category(equiv, "Ll") {
    Some(lower) => lower
    None => unicode_min_equivalent(equiv, ch)
  }
}

///|
/// Uppercase a string using Unicode case mapping.
pub fn unicode_string_upcase(s : String) -> String {
  let buf = @buffer.new()
  for ch in s {
    buf.write_char(unicode_char_upcase(ch))
  }
  buf.to_string()
}

///|
/// Lowercase a string using Unicode case mapping.
pub fn unicode_string_downcase(s : String) -> String {
  let buf = @buffer.new()
  for ch in s {
    buf.write_char(unicode_char_downcase(ch))
  }
  buf.to_string()
}

///|
/// Case-fold a string for case-insensitive comparison.
///
/// # Example
/// ```mbt check
/// test "string foldcase" {
///   inspect(unicode_string_foldcase("ABC"), content="abc")
/// }
/// ```
pub fn unicode_string_foldcase(s : String) -> String {
  let buf = @buffer.new()
  for ch in s {
    buf.write_char(unicode_char_foldcase(ch))
  }
  buf.to_string()
}
