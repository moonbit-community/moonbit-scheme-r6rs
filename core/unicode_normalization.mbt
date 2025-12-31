///|
const HANGUL_S_BASE : Int = 0xAC00

///|
const HANGUL_L_BASE : Int = 0x1100

///|
const HANGUL_V_BASE : Int = 0x1161

///|
const HANGUL_T_BASE : Int = 0x11A7

///|
const HANGUL_L_COUNT : Int = 19

///|
const HANGUL_V_COUNT : Int = 21

///|
const HANGUL_T_COUNT : Int = 28

///|
const HANGUL_N_COUNT : Int = HANGUL_V_COUNT * HANGUL_T_COUNT

///|
const HANGUL_S_COUNT : Int = HANGUL_L_COUNT * HANGUL_N_COUNT

///|
fn data_chars(data : String) -> ReadOnlyArray[Char] {
  let out = data.to_array().filter((ch) => ch != '\n' && ch != '\r')
  ReadOnlyArray::from_array(out[:])
}

///|
let canon_decomp_data : ReadOnlyArray[Char] = data_chars(DECOMP_CANON_DATA)

///|
let compat_decomp_data : ReadOnlyArray[Char] = data_chars(DECOMP_COMPAT_DATA)

///|
let combining_class_data : ReadOnlyArray[Char] = data_chars(
  COMBINING_CLASS_DATA,
)

///|
let composition_data : ReadOnlyArray[Char] = data_chars(COMPOSITION_DATA)

///|
fn build_decomp_map(
  data : ReadOnlyArray[Char],
  count : Int,
) -> Map[Char, Array[Char]] {
  let map = Map::new(capacity=count)
  for i = 0; i < data.length(); {
    let ch = data[i]
    let len = data[i + 1].to_int()
    let start = i + 2
    let seq : Array[Char] = []
    for j = 0; j < len; {
      seq.push(data[start + j])
      continue j + 1
    } else {
      ()
    }
    map[ch] = seq
    continue start + len
  } else {
    map
  }
}

///|
fn build_combining_class_map(
  data : ReadOnlyArray[Char],
  count : Int,
) -> Map[Char, Int] {
  let map = Map::new(capacity=count)
  for i = 0; i < data.length(); {
    let ch = data[i]
    let cls = data[i + 1].to_int()
    map[ch] = cls
    continue i + 2
  } else {
    map
  }
}

///|
fn build_composition_map(
  data : ReadOnlyArray[Char],
  count : Int,
) -> Map[UInt, Char] {
  let map = Map::new(capacity=count)
  for i = 0; i + 2 < data.length(); {
    let starter = data[i]
    let combining = data[i + 1]
    let composed = data[i + 2]
    map[compose_key(starter, combining)] = composed
    continue i + 3
  } else {
    map
  }
}

///|
let canon_decomp_map : Map[Char, Array[Char]] = build_decomp_map(
  canon_decomp_data,
  DECOMP_CANON_COUNT,
)

///|
let compat_decomp_map : Map[Char, Array[Char]] = build_decomp_map(
  compat_decomp_data,
  DECOMP_COMPAT_COUNT,
)

///|
let combining_class_map : Map[Char, Int] = build_combining_class_map(
  combining_class_data,
  COMBINING_CLASS_COUNT,
)

///|
let composition_map : Map[UInt, Char] = build_composition_map(
  composition_data,
  COMPOSITION_COUNT,
)

///|
fn combining_class(ch : Char) -> Int {
  match combining_class_map.get(ch) {
    Some(cls) => cls
    None => 0
  }
}

///|
fn compose_key(starter : Char, combining : Char) -> UInt {
  (starter.to_uint() << 21) | combining.to_uint()
}

///|
fn is_hangul_syllable(ch : Char) -> Bool {
  let s_index = ch.to_int() - HANGUL_S_BASE
  s_index >= 0 && s_index < HANGUL_S_COUNT
}

///|
fn hangul_decompose(ch : Char, compat : Bool, out : Array[Char]) -> Bool {
  if !is_hangul_syllable(ch) {
    return false
  }
  let s_index = ch.to_int() - HANGUL_S_BASE
  let l_index = s_index / HANGUL_N_COUNT
  let v_index = s_index % HANGUL_N_COUNT / HANGUL_T_COUNT
  let t_index = s_index % HANGUL_T_COUNT
  let l = (HANGUL_L_BASE + l_index).unsafe_to_char()
  let v = (HANGUL_V_BASE + v_index).unsafe_to_char()
  decompose_char(l, compat, out)
  decompose_char(v, compat, out)
  if t_index != 0 {
    let t = (HANGUL_T_BASE + t_index).unsafe_to_char()
    decompose_char(t, compat, out)
  }
  true
}

///|
fn decompose_char(ch : Char, compat : Bool, out : Array[Char]) -> Unit {
  if hangul_decompose(ch, compat, out) {
    return
  }
  let mapping = if compat {
    match compat_decomp_map.get(ch) {
      Some(seq) => Some(seq)
      None => canon_decomp_map.get(ch)
    }
  } else {
    canon_decomp_map.get(ch)
  }
  match mapping {
    Some(seq) =>
      for item in seq {
        decompose_char(item, compat, out)
      }
    None => out.push(ch)
  }
}

///|
fn decompose_string(s : String, compat : Bool) -> Array[Char] {
  let out : Array[Char] = []
  for ch in s {
    decompose_char(ch, compat, out)
  }
  out
}

///|
fn insert_sorted_by_ccc(segment : Array[Char], ch : Char, ccc : Int) -> Unit {
  let start = segment.length()
  segment.push(ch)
  let insert_at = for i = start; i > 0; {
    let prev = segment[i - 1]
    if combining_class(prev) <= ccc {
      break i
    }
    segment[i] = prev
    continue i - 1
  } else {
    0
  }
  segment[insert_at] = ch
}

///|
fn canonical_order(chars : Array[Char]) -> Array[Char] {
  let out : Array[Char] = []
  let mut segment : Array[Char] = []
  for ch in chars {
    let ccc = combining_class(ch)
    if ccc == 0 {
      if segment.length() > 0 {
        for item in segment {
          out.push(item)
        }
        segment = []
      }
      segment.push(ch)
    } else if segment.length() == 0 {
      segment.push(ch)
    } else {
      insert_sorted_by_ccc(segment, ch, ccc)
    }
  }
  for item in segment {
    out.push(item)
  }
  out
}

///|
fn is_hangul_l_jamo(ch : Char) -> Bool {
  let value = ch.to_int()
  value >= HANGUL_L_BASE && value < HANGUL_L_BASE + HANGUL_L_COUNT
}

///|
fn is_hangul_v_jamo(ch : Char) -> Bool {
  let value = ch.to_int()
  value >= HANGUL_V_BASE && value < HANGUL_V_BASE + HANGUL_V_COUNT
}

///|
fn is_hangul_t_jamo(ch : Char) -> Bool {
  let value = ch.to_int()
  value > HANGUL_T_BASE && value < HANGUL_T_BASE + HANGUL_T_COUNT
}

///|
fn is_hangul_lv_syllable(ch : Char) -> Bool {
  if !is_hangul_syllable(ch) {
    return false
  }
  let s_index = ch.to_int() - HANGUL_S_BASE
  s_index % HANGUL_T_COUNT == 0
}

///|
fn hangul_compose(starter : Char, combining : Char) -> Char? {
  if is_hangul_l_jamo(starter) && is_hangul_v_jamo(combining) {
    let l_index = starter.to_int() - HANGUL_L_BASE
    let v_index = combining.to_int() - HANGUL_V_BASE
    let s = HANGUL_S_BASE +
      (l_index * HANGUL_V_COUNT + v_index) * HANGUL_T_COUNT
    return Some(s.unsafe_to_char())
  }
  if is_hangul_lv_syllable(starter) && is_hangul_t_jamo(combining) {
    let s_index = starter.to_int() - HANGUL_S_BASE
    let t_index = combining.to_int() - HANGUL_T_BASE
    return Some((HANGUL_S_BASE + s_index + t_index).unsafe_to_char())
  }
  None
}

///|
fn try_compose(
  starter : Char,
  combining : Char,
  last_ccc : Int,
  ccc : Int,
) -> Char? {
  if last_ccc != 0 && last_ccc >= ccc {
    return None
  }
  match hangul_compose(starter, combining) {
    Some(composed) => return Some(composed)
    None => ()
  }
  composition_map.get(compose_key(starter, combining))
}

///|
fn compose(chars : Array[Char]) -> Array[Char] {
  let out : Array[Char] = []
  let mut starter_index = -1
  let mut last_ccc = 0
  for ch in chars {
    let ccc = combining_class(ch)
    if starter_index >= 0 {
      let starter = out[starter_index]
      match try_compose(starter, ch, last_ccc, ccc) {
        Some(composed) => {
          out[starter_index] = composed
          continue
        }
        None => ()
      }
    }
    if ccc == 0 {
      starter_index = out.length()
      last_ccc = 0
    } else {
      last_ccc = ccc
    }
    out.push(ch)
  }
  out
}

///|
fn normalize_string(s : String, compat : Bool, compose_output : Bool) -> String {
  let decomposed = decompose_string(s, compat)
  let ordered = canonical_order(decomposed)
  let result = if compose_output { compose(ordered) } else { ordered }
  let buf = @buffer.new()
  for ch in result {
    buf.write_char(ch)
  }
  buf.to_string()
}

///|
/// Normalize a string to Unicode NFD.
///
/// # Example
/// ```mbt check
/// test "normalize nfd" {
///   inspect(
///     unicode_string("\u{00e9}").normalize_nfd().into_string(),
///     content="e\u{301}",
///   )
/// }
/// ```
pub fn UnicodeString::normalize_nfd(self : UnicodeString) -> UnicodeString {
  { value: normalize_string(self.value, false, false) }
}

///|
/// Normalize a string to Unicode NFC.
///
/// # Example
/// ```mbt check
/// test "normalize nfc" {
///   inspect(
///     unicode_string("e\u{301}").normalize_nfc().into_string(),
///     content="\u{00e9}",
///   )
/// }
/// ```
pub fn UnicodeString::normalize_nfc(self : UnicodeString) -> UnicodeString {
  { value: normalize_string(self.value, false, true) }
}

///|
/// Normalize a string to Unicode NFKD.
///
/// # Example
/// ```mbt check
/// test "normalize nfkd" {
///   inspect(
///     unicode_string("\u{212b}").normalize_nfkd().into_string(),
///     content="A\u{30a}",
///   )
/// }
/// ```
pub fn UnicodeString::normalize_nfkd(self : UnicodeString) -> UnicodeString {
  { value: normalize_string(self.value, true, false) }
}

///|
/// Normalize a string to Unicode NFKC.
///
/// # Example
/// ```mbt check
/// test "normalize nfkc" {
///   inspect(
///     unicode_string("\u{212b}").normalize_nfkc().into_string(),
///     content="\u{00c5}",
///   )
/// }
/// ```
pub fn UnicodeString::normalize_nfkc(self : UnicodeString) -> UnicodeString {
  { value: normalize_string(self.value, true, true) }
}
