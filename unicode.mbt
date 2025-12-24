///|
fn char_in_ranges(ch : Char, ranges : Array[Char]) -> Bool {
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
fn unicode_char_is_alphabetic(ch : Char) -> Bool {
  unicode_category_has(ch, "L") || unicode_category_has(ch, "Nl")
}

///|
fn unicode_char_is_uppercase(ch : Char) -> Bool {
  unicode_category_has(ch, "Lu")
}

///|
fn unicode_char_is_lowercase(ch : Char) -> Bool {
  unicode_category_has(ch, "Ll")
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
fn unicode_pick_equivalent_category(chars : Array[Char], category : String) -> Char? {
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
fn unicode_char_upcase(ch : Char) -> Char {
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
fn unicode_char_downcase(ch : Char) -> Char {
  let equiv = unicode_case_equivalents(ch)
  match unicode_pick_equivalent_category(equiv, "Ll") {
    Some(lower) => lower
    None => ch
  }
}

///|
fn unicode_char_foldcase(ch : Char) -> Char {
  let equiv = unicode_case_equivalents(ch)
  match unicode_pick_equivalent_category(equiv, "Ll") {
    Some(lower) => lower
    None => unicode_min_equivalent(equiv, ch)
  }
}

///|
fn unicode_string_upcase(s : String) -> String {
  let buf = @buffer.new()
  for ch in s {
    buf.write_char(unicode_char_upcase(ch))
  }
  buf.to_string()
}

///|
fn unicode_string_downcase(s : String) -> String {
  let buf = @buffer.new()
  for ch in s {
    buf.write_char(unicode_char_downcase(ch))
  }
  buf.to_string()
}

///|
fn unicode_string_foldcase(s : String) -> String {
  let buf = @buffer.new()
  for ch in s {
    buf.write_char(unicode_char_foldcase(ch))
  }
  buf.to_string()
}
