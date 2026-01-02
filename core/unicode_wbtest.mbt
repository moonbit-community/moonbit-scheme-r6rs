///|
test "unicode internals" {
  inspect(unicode_category_has('\u{01C5}', "Lt"), content="true")
  inspect(unicode_category_has('A', "Z"), content="false")
  match unicode_pick_equivalent_category(['\u{01C5}'], "Lt") {
    Some('\u{01C5}') => ()
    Some(_) => ()
    None => fail("expected titlecase match")
  }
  match unicode_pick_equivalent_category(['\u{01C5}'], "Z") {
    None => ()
    Some(_) => fail("expected no match")
  }
  match unicode_min_equivalent([], 'x') {
    'x' => ()
    _ => fail("expected fallback")
  }
  match unicode_min_equivalent(['b', 'a'], 'x') {
    'a' => ()
    _ => fail("expected min")
  }
}

///|
test "panic unicode case folding odd data" {
  ignore(build_case_folding("a"[:]))
}

///|
test "unicode normalization internals" {
  let segment : Array[Char] = []
  insert_sorted_by_ccc(segment, 'a', 1)
  inspect(segment.length(), content="1")
  match try_compose('a', 'b', 1, 1) {
    None => ()
    Some(_) => fail("expected no composition")
  }
  inspect(is_hangul_t_jamo('\u{11A8}'), content="true")
  inspect(is_hangul_lv_syllable('\u{AC00}'), content="true")
}
