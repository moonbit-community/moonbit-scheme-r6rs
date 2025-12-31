# Refactoring Skills

## Fast impact analysis with moon ide
- `moon ide find-references <symbol>` to locate all call sites before changing APIs.
- `moon ide peek-def <symbol>` to confirm the owning type/module and signature.
- `moon ide outline <dir|file>` to scan package structure without opening files.

Example:
```bash
moon ide find-references reader_next
moon ide peek-def Reader
moon ide outline lexer
```

## Example: convert a free function to a method
Goal: move `reader_next` to an OO-style method and update call sites.

Before:
```mbt
pub fn reader_next(r : Reader) -> Char? {
  ...
}

match reader_next(r) {
  ...
}
```

After:
```mbt
pub fn Reader::next(self : Reader) -> Char? {
  ...
}

match r.next() {
  ...
}
```

Then regenerate API summaries:
```bash
moon info
```

## API discovery and validation
- Use `moon doc "<Type::method>"` to confirm the method shows up as expected.
- Use `moon check` or `moon test` after refactors that touch parsing or lexing.

Example:
```bash
moon doc "Reader::next"
moon check
```

## Low-friction edits
- Prefer `apply_patch` for single-file edits and small, precise refactors.
- Use `rg` to do quick sanity checks when you do not need full symbol context.

## Chaining style after method refactors
- Convert related free functions into `Type::method` so call sites can chain with `..`.
- Update docs and tests that referenced old `@pkg.fn` helpers.

Example:
```mbt
// Before
skip_ws_and_comments(r)
match reader_peek(r) { ... }

// After
match r..skip_ws_and_comments().peek() { ... }
```

Follow-up checks:
```bash
moon ide find-references @lexer.reader_peek
moon info
```

## Private helper methods on Reader
- Convert `Reader`-only helpers to private methods to keep lookahead and escape logic local to the reader API.

Example:
```mbt
fn Reader::peek_offset(self : Reader, offset : Int) -> Char? {
  self.chars.get(self.pos + offset)
}

if self.peek_offset(1) == Some('|') {
  self.skip_block_comment()
}
```

Tooling:
```bash
moon ide find-references read_hex_escape
```

## Foreign types and methods
- You cannot define methods on types from other packages (e.g., `@core.Port`).
- Keep helper functions or introduce a local wrapper type if you need chaining.

Example failure mode:
```mbt
pub fn Port::write(self : Port, text : String) -> Unit { ... }
// error: Cannot define method write for foreign type @dii/scheme-r6rs/core.Port
```

## Reduce counter boilerplate
- Centralize `Ref[Int]` increment logic in a private helper, then reuse across `next_*` functions.
- Apply the same helper to eval counters (closure/parameter/promise/winder) to keep patterns consistent.

## Docstring test scoping
- Doctests run in the package context, but core types may need explicit `@core.` prefixes.
- Use fully qualified names like `@core.Datum` and `@core.Value` in examples to avoid "type not found".

Quick check:
```bash
moon check
```

## Docstring tests for stateful APIs
- Keep examples self-contained so they do not rely on shared state between tests.
- Prefer minimal inputs (short strings, small data) to keep doctests fast.

## Facade-level docs
- Add small examples on re-exported APIs so module users see usage without diving into subpackages.

## Boolean API examples
- Use `inspect(..., content="true"|"false")` in doctests to make expectations explicit.

## Error-path doctests
- Prefer `try ... catch ... noraise` for error-path doctests to avoid lint warnings.

## Match simplification
- Group enum variants with `|` patterns when they map to the same output to reduce duplication.
- For tuple matches, only group variants when the bound variables share the same type.

## Recursive parent traversal
- Prefer simple recursion over `mut` state when walking a parent chain.

Example:
```mbt
pub fn record_type_is_a(actual : RecordType, target : RecordType) -> Bool {
  if actual.id == target.id {
    true
  } else {
    match actual.parent {
      Some(parent) => record_type_is_a(parent, target)
      None => false
    }
  }
}
```

## Indexing helpers
- Extract tiny helpers (like "last frame") to avoid repeated index math and keep mutations localized.

## Constructor helpers
- Factor repeated "pair of symbol + expr" patterns into a small helper to keep syntax builders consistent.

## Map iteration simplification
- Iterate with `for key, value in map` to avoid extra `get`/`keys` loops.

## Deep clone with map
- Combine `Array::map` and `Map::map` to copy nested env frames without manual `mut` loops.

Example:
```mbt
pub fn env_clone(env : Env) -> Env {
  env.map((frame) =>
    frame.map((_, binding) => Binding::{ id: binding.id, value: binding.value })
  )
}
```

## Functional loops
- Prefer `for i, v in array` over manual `while` counters when possible to reduce `mut`.
  - Useful for delimiter scans like `@` or `/` in numeric tokens.

Example:
```mbt
for i = literal_idx + 1; i < parts.length(); {
  let rule_parts = datum_list_to_array(parts[i])
  ...
  continue i + 1
} else {
  ()
}
```

Example:
```mbt
for i = 0; i < clauses.length(); {
  let rule_parts = datum_list_to_array(clauses[i])
  ...
  continue i + 1
} else {
  ()
}
```

## Division-based counters
- Replace `while n > 0` loops with a functional `for` that carries `(n, count)` state.

Example:
```mbt
for n = value, count = 0; n.compare_int(0) > 0; {
  continue n / two, count + 1
} else {
  count
}
```

## Multi-state loops
- Use functional `for` with multiple state variables to replace `mut` accumulators in small numeric loops.

Example:
```mbt
for i = 0, starter_index = -1, last_ccc = 0; i < chars.length(); {
  let ch = chars[i]
  let ccc = combining_class(ch)
  if starter_index >= 0 {
    let starter = out[starter_index]
    match try_compose(starter, ch, last_ccc, ccc) {
      Some(composed) => {
        out[starter_index] = composed
        continue i + 1, starter_index, last_ccc
      }
      None => ()
    }
  }
  let next_starter = if ccc == 0 { out.length() } else { starter_index }
  let next_ccc = if ccc == 0 { 0 } else { ccc }
  out.push(ch)
  continue i + 1, next_starter, next_ccc
} else {
  out
}
```

## Shared constants
- Lift repeated lookup tables (like radix digit arrays) to a single `let` to avoid duplication.

Example:
```mbt
let base = bigint_from_int(radix)
let next_acc = acc * base + bigint_from_int(digit)
```

## Insertion index scans
- Use a functional `for` with `break` to compute the insertion point while shifting elements.

Example:
```mbt
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
```

## Segment flush loops
- Use a functional `for` with an array state when you need to flush segments into an output array.

Example:
```mbt
let out : Array[Char] = []
let segment = for i = 0, segment : Array[Char] = []; i < chars.length(); {
  let ch = chars[i]
  let ccc = combining_class(ch)
  if ccc == 0 {
    if segment.length() > 0 {
      for item in segment {
        out.push(item)
      }
    }
    let next_segment : Array[Char] = []
    next_segment.push(ch)
    continue i + 1, next_segment
  } else if segment.length() == 0 {
    let next_segment : Array[Char] = []
    next_segment.push(ch)
    continue i + 1, next_segment
  } else {
    insert_sorted_by_ccc(segment, ch, ccc)
    continue i + 1, segment
  }
} else {
  segment
}
for item in segment {
  out.push(item)
}
```

## Reverse assembly
- Extract a small helper for reversing `Array[Char]` into a `String` to cut repeated loops.

## Build lists with functional state
- Build list datums from arrays using a functional `for` instead of `mut` tail updates.
- When dotted tails or early exits are possible, wrap list building in a helper and `return` early.

Example:
```mbt
pub fn list_from_array(items : Array[Datum]) -> Datum {
  for i = items.length(), tail = Datum::Nil; i > 0; {
    let idx = i - 1
    continue i - 1, pair_new(items[idx], tail)
  } else {
    tail
  }
}
```

Example:
```mbt
fn list_from_items(items : Array[Datum], base : Datum) -> Datum {
  for i = items.length(), tail = base; i > 0; {
    let idx = i - 1
    continue i - 1, pair_new(items[idx], tail)
  } else {
    tail
  }
}
```

## Reader consumption
- Use `Reader::next()` (with `ignore(...)` if needed) to advance instead of direct `pos` mutation.
- Extract small reader steps (like line-comment skipping) into `Reader::` methods so call sites can chain.

Example:
```mbt
fn Reader::skip_line_comment(self : Reader) -> Unit {
  while true {
    match self.next() {
      Some('\n') => break
      Some(_) => continue
      None => break
    }
  }
}
```

## Lookahead helpers
- Add a small `reader_peek_offset` helper to centralize bounded lookahead when scanning delimiters.

## Reader access
- Prefer `Reader::peek()` over manual `pos` bounds checks when only looking ahead one char.

## First-token handling
- Replace manual index counters with a `first` flag when parsing optional leading signs.

Example:
```mbt
if first && (ch == '+' || ch == '-') {
  let next_sign = if ch == '-' { Float::from_int(-1) } else { Float::from_int(1) }
  continue i + 1, next_sign, int_part, frac_part, frac_div, seen_dot,
    seen_digit, false
}
```

## Optional marker flags
- Compute flags with a `match` that can consume the optional marker inline.

Example:
```mbt
let splicing = match r.peek() {
  Some('@') => {
    ignore(r.next())
    true
  }
  _ => false
}
```

## Boolean chaining
- Replace stepwise `ok = ok && ...` with a single boolean expression to reduce mut.

## Direct array iteration
- Prefer `for item in array` to avoid index counters when only values matter.

Example:
```mbt
for frame in env {
  for key in frame.keys() {
    match frame.get(key) {
      Some(binding) => exports[key] = binding
      None => ()
    }
  }
}
```

## Array equality scans
- Use a functional `for` with `break false` for early exits instead of `mut` indices.

Example:
```mbt
for i = 0; i < left.length(); {
  if !binding_equal(left[i], right[i]) {
    break false
  }
  continue i + 1
} else {
  true
}
```

## Predicate-driven loops
- Move `while true` + `match` exit checks into the `for` predicate when possible.
- Use `else` to return the final state cleanly.

Example:
```mbt
let exp = for exp = 0, value = bigint_from_int(1);
  bigint_to_int_option(value) is Some(_); {
    continue exp + 1, value * two
  } else {
    exp
  }
```

## Terminator scans
- Use a functional `for` with a `break` value to scan until a terminator without `mut`.
- Carry the accumulator and any flags as loop state.

Example:
```mbt
for acc = 0, has_digit = false; true; {
  match self.next() {
    None => raise @core.ParseError("unterminated string")
    Some(';') =>
      match acc.to_char() {
        Some(ch) => break ch
        None => raise @core.ParseError("invalid hex escape")
      }
    Some(ch) =>
      match digit_value(ch) {
        Some(digit) if digit < 16 => continue acc * 16 + digit, true
        _ => raise @core.ParseError("invalid hex escape")
      }
  }
} else {
  raise @core.ParseError("invalid hex escape")
}
```

## Min/max accumulation
- Use a functional `for` to carry the current best value when scanning arrays.
- Carry extra flags in the loop state (for example, tracking inexact values).

Example:
```mbt
let current = value_as_fixnum_int(args[0])
let current = for i = 1, current = current; i < args.length(); {
  let cur = value_as_fixnum_int(args[i])
  let next = if cur > current { cur } else { current }
  continue i + 1, next
} else {
  current
}
```

Example:
```mbt
let (best, has_inexact) = for i = 1, best = best, has_inexact = has_inexact; i < args.length(); {
  let cur = value_as_number(args[i])
  let next_best = if num_less(best, cur) { cur } else { best }
  let next_has_inexact = has_inexact || cur is Datum::Float(_)
  continue i + 1, next_best, next_has_inexact
} else {
  (best, has_inexact)
}
```

## String conversion helper
- Use a single `string_to_chars` helper for repeated String → Array[Char] conversions.
- Prefer slicing the char array for prefixes instead of manual `get_char` loops.

## Streamed digit accumulation
- Accumulate numeric values during parsing instead of collecting digits into an array first.

Example:
```mbt
let (acc, frac_len, has_digit) = for i = start,
  acc = bigint_from_int(0), frac_len = 0, seen_dot = false, has_digit = false;
  i < chars.length(); {
    let ch = chars[i]
    if ch == '.' {
      if seen_dot { return None }
      continue i + 1, acc, frac_len, true, has_digit
    }
    if ch.is_ascii_digit() {
      let next_acc = acc * ten + bigint_from_int(ch.to_int() - '0'.to_int())
      let next_frac = if seen_dot { frac_len + 1 } else { frac_len }
      continue i + 1, next_acc, next_frac, seen_dot, true
    }
    return None
} else {
  (acc, frac_len, has_digit)
}
```

## Radix string conversion
- Build digits by dividing in a functional loop and carrying `(n, chars)` state.
- Keep sign handling outside the loop to avoid extra `mut` variables.

Example:
```mbt
let chars = for n = n, chars = []; n > 0; {
  let rem = n % radix
  chars.push(radix_digits[rem])
  continue n / radix, chars
} else {
  chars
}
```

## Bitwise accumulation loops
- Carry `(a, b, bit, result)` in a functional loop to avoid four `mut` vars.
- Update the accumulated result with a derived `next_result` each iteration.

Example:
```mbt
for a = left,
  b = right,
  bit = bigint_from_int(1),
  result = bigint_from_int(0);
  !bigint_is_zero(a) || !bigint_is_zero(b); {
    let out_one = ...
    let next_result = if out_one { result + bit } else { result }
    continue a / two, b / two, bit * two, next_result
  } else {
    result
  }
```

## Conditional offset accumulation
- When building a value from conditional bits, fold `(i, acc)` in a functional loop.
- Compute the offset inside the loop to keep the mutation-free pattern obvious.

Example:
```mbt
let reversed = for i = 0, reversed = bigint_from_int(0); i < width; {
  let next =
    if datum_bitwise_bit_set(field, i) {
      reversed + bigint_pow2(width - 1 - i)
    } else {
      reversed
    }
  continue i + 1, next
} else {
  reversed
}
```

## Endian accumulation
- Fold byte arrays into integers with a functional loop to remove `mut` counters.
- Carry a `factor` for little-endian accumulation.

Example:
```mbt
let result = for i = 0, result = bigint_from_int(0), factor = bigint_from_int(1);
  i < size; {
    let next = result + bigint_from_int(items[start + i]) * factor
    continue i + 1, next, factor * base
  } else {
    result
  }
```

## Rule scans with early return
- Replace index-based `while` loops with `for` + `continue` when matching a rule list.
- Keep `return` for the match case and `continue i + 1` for the fallback.

Example:
```mbt
for i = 0; i < rules.rules.length(); {
  let rule = rules.rules[i]
  if matches(rule) {
    return expand(rule)
  }
  continue i + 1
}
```

## Repeat binding accumulation
- Use a functional `for` when collecting per-item bindings, returning early on mismatch.

Example:
```mbt
for i = 0; i < count; {
  let iter_bindings : Map[String, BindingVal] = {}
  if !match_pattern(repeat_pat, inputs[start + i], ...) {
    return None
  }
  // merge iter_bindings into repeated
  continue i + 1
}
```

## Prefix tag scanning
- Use functional `for` with `continue` to advance by fixed steps (like `#`-prefixed numeric tags).

Example:
```mbt
let (start, radix, exactness) = for i = 0, radix = base_radix,
  exactness = default_exactness; i + 1 < len && chars[i] == '#'; {
    let tag = chars[i + 1]
    match tag {
      'b' | 'B' => continue i + 2, 2, exactness
      'e' | 'E' => continue i + 2, radix, Some(tag)
      _ => return None
    }
  } else {
    (i, radix, exactness)
  }
```

## Depth-driven loops
- Replace `mut depth` while loops with functional `for` state updates when scanning nested constructs.

## Conditional stride loops
- Use functional `for` with `continue i + 2` when the loop step depends on lookahead.

Example:
```mbt
for i = 0; i < items.length(); {
  if i + 1 < items.length() && datum_is_symbol(items[i + 1], ellipsis) {
    ...
    continue i + 2
  }
  ...
  continue i + 1
} else {
  ()
}
```

Example:
```mbt
for depth = 1; depth > 0; {
  match (self.peek(), self.peek_offset(1)) {
    (None, _) => raise @core.ParseError("unterminated block comment")
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
```

## Reverse scans
- Isolate right-to-left split detection into a helper to keep main parsers linear.

## Looping on map lookups
- Replace `mut` cursor variables with a functional loop that advances via `continue` on map lookups.

Example:
```mbt
let out : Array[Char] = [ch]
for cur = ch; true; {
  match case_folding.get(cur) {
    Some(next) => {
      if out.contains(next) { break }
      out.push(next)
      continue next
    }
    None => break
  }
} else {
  ()
}
```

## Suffix digit scans
- Use functional `for` with `break` to find the first non-digit from the end without `mut`.

Example:
```mbt
let i = for idx = len; idx > 0; {
  let ch = chars[idx - 1]
  if ch >= '0' && ch <= '9' {
    continue idx - 1
  }
  break idx
} else {
  0
}
```

## Unique delimiter lookup
- Extract a helper like `find_unique_char` when multiple parsers need the same delimiter scan logic.

Example:
```mbt
fn find_unique_char(chars : Array[Char], target : Char) -> Int? {
  let found = for i = 0, found = -1; i < chars.length(); {
    let ch = chars[i]
    if ch == target {
      if found != -1 {
        return None
      }
      continue i + 1, i
    }
    continue i + 1, found
  } else {
    found
  }
  if found == -1 { None } else { Some(found) }
}
```

## Tuple destructuring
- Replace `mut` temporaries with a single `let (a, b) = match ...` when branching sets both.

Example:
```mbt
let (sign, start) = match chars[0] {
  '+' => (1, 1)
  '-' => (-1, 1)
  _ => (1, 0)
}
```

## First match scans
- Use a functional `for` with `break` to grab the first matching index without `mut`.

Example:
```mbt
let idx = for i = 0; i < chars.length(); {
  let ch = chars[i]
  if ch == 'e' || ch == 'E' {
    break i
  }
  continue i + 1
} else {
  -1
}
```

## Looping until None
- Use a functional `for` with `continue` to repeatedly transform a value until a lookup returns `None`.

Example:
```mbt
fn strip_all_gensym_suffixes(name : String) -> String {
  for cur = name; true; {
    match strip_gensym_suffix(cur) {
      Some(next) => continue next
      None => break cur
    }
  } else {
    name
  }
}
```

## Multi-flag scans
- Use a functional `for` with tuple state to compute multiple booleans in a single pass.

Example:
```mbt
let (has_dot, has_exp, has_slash) = for i = 0, has_dot = false, has_exp = false,
  has_slash = false; i < tok.length(); {
    match tok.get_char(i) {
      Some(ch) =>
        continue i + 1, has_dot || ch == '.',
          has_exp || ch == 'e' || ch == 'E', has_slash || ch == '/'
      None => break (has_dot, has_exp, has_slash)
    }
  } else {
    (has_dot, has_exp, has_slash)
  }
```

## List traversal with functional state
- Replace `mut cur` list loops with a functional `for` and `break` to return values.

Example:
```mbt
fn list_member(mode : EqualityMode, item : Value, list : Datum) -> Value raise EvalError {
  for cur = list; true; {
    match cur {
      Datum::Nil => break bool_value(false)
      Datum::Pair(car, cdr) => {
        if equality_match(mode, item, value_from_datum(car.val)) {
          break Value::Datum(cur)
        }
        continue cdr.val
      }
      _ => raise @core.EvalError("type error: proper list expected")
    }
  } else {
    bool_value(false)
  }
}
```

## Looping with external accumulators
- Keep arrays or maps outside the loop and update them while advancing loop state with `continue`.

Example:
```mbt
let seen : Array[Int] = []
for cur = value; true; {
  match cur {
    Datum::Label(id, cell) => {
      if seen.contains(id) {
        break cur
      }
      seen.push(id)
      continue cell.val
    }
    _ => break cur
  }
} else {
  value
}
```

Example:
```mbt
fn next_counter_id(counter : Ref[Int]) -> Int {
  let id = counter.val
  counter.val = id + 1
  id
}

pub fn next_binding_id() -> Int {
  next_counter_id(binding_counter)
}
```

## Wrapper methods for foreign types
- Use wrapper types (`UnicodeChar`, `UnicodeString`) when you need chaining on foreign types.
- Convert free functions to wrapper methods and update call sites to chaining style, then trim old imports.
- Use `moon ide find-references @core.unicode_string_foldcase` to enumerate call sites before swapping to wrappers.
- Regenerate public API summaries with `moon info` after API conversions.

Example:
```mbt
pub struct UnicodeString {
  value : String
}

pub fn unicode_string(s : String) -> UnicodeString {
  { value: s }
}

pub fn UnicodeString::foldcase(self : UnicodeString) -> UnicodeString {
  ...
}

let folded =
  unicode_string("e\u{301}").normalize_nfc().foldcase().into_string()
```

Quick checks:
```bash
moon ide find-references @core.unicode_string_foldcase
moon info
```

## Chained comparisons without mut
- Use functional `for` loops with `(index, prev)` state to replace `mut` when comparing adjacent values.
- Add small helpers (like `foldcase_*_if`) to keep case-insensitive logic centralized.

Example:
```mbt
fn foldcase_char_if(ch : Char, case_insensitive : Bool) -> Char {
  if case_insensitive { unicode_char(ch).foldcase() } else { ch }
}

fn compare_chain_char(args : Array[Value], mode : CompareMode, case_insensitive : Bool) -> Bool raise EvalError {
  if args.length() <= 1 {
    return true
  }
  let prev = foldcase_char_if(value_as_char(args[0]), case_insensitive)
  for i = 1, prev = prev; i < args.length(); {
    let cur = foldcase_char_if(value_as_char(args[i]), case_insensitive)
    if !compare_ok(mode, prev.to_int().compare(cur.to_int())) {
      return false
    }
    continue i + 1, cur
  } else {
    true
  }
}
```

Example:
```mbt
let prev = value_as_number(args[0])
let ok = for i = 1, prev = prev; i < args.length(); {
  let cur = value_as_number(args[i])
  if !num_less(prev, cur) {
    break false
  }
  continue i + 1, cur
} else {
  true
}
```

## Counter wrappers for OO-style ids
- Wrap repeated `Ref[Int]` counters in a small struct with a `next()` method to keep call sites uniform.
- Keep the wrapper private to avoid API churn while still enabling method-style calls.

Example:
```mbt
priv struct Counter {
  cell : Ref[Int]
}

fn Counter::new() -> Counter {
  { cell: Ref::new(0) }
}

fn Counter::next(self : Counter) -> Int {
  let id = self.cell.val
  self.cell.val = id + 1
  id
}

let port_counter = Counter::new()

fn next_port_id() -> Int {
  port_counter.next()
}
```

## Array helpers for less mut
- Use `Array::map` to build arrays without manual `push`.
- Use `Array::copy`, `Array::contains`, and `Array::filter` instead of manual loops.

Example:
```mbt
let fields = values.map((value) => Ref::new(value))

fn scopes_with_added(scopes : Array[Int], scope : Int) -> Array[Int] {
  if scopes.contains(scope) {
    scopes.copy()
  } else {
    let next = scopes.copy()
    next.push(scope)
    next
  }
}
```

## Step-wise loops
- Use functional `for` with `continue` to advance by fixed strides (like packed unicode tables).

Example:
```mbt
for i = 0; i + 2 < data.length(); {
  let starter = data[i]
  let combining = data[i + 1]
  let composed = data[i + 2]
  map[compose_key(starter, combining)] = composed
  continue i + 3
} else {
  map
}
```

## Vector remapping in macros
- Use `Array::map` to rebuild `Datum::Vector` transformations without mutable buffers.

Example:
```mbt
Datum::Vector(items) =>
  Datum::Vector(
    items.map((item) => rename_proc_datum(item, def_env, call_ctx, renames, captures))
  )
```

## Snapshot arrays cleanly
- Use `Array::copy` to snapshot mutable stacks without manual push loops.

Example:
```mbt
let winds_copy = wind_stack.val.copy()
```

## Map over parsed lists
- Convert list datums once, then map to avoid duplicate traversal logic.

Example:
```mbt
pub fn datum_list_to_value_array(list : Datum) -> Array[Value] raise EvalError {
  datum_list_to_array(list).map((item) => value_from_datum(item))
}
```

## Validating map transforms
- `Array::map` can raise; use it to validate elements while transforming.

Example:
```mbt
let bytes = items.map((item) =>
  match item {
    Datum::Int(n) => {
      if n < 0 || n > 255 {
        raise @core.ParseError("bytevector element out of range")
      }
      n
    }
    _ => raise @core.ParseError("bytevector element must be integer")
  }
)
```

## Fast exponent loops without mut
- Use functional `for` state to implement exponentiation by squaring without mutable variables.

Example:
```mbt
fn pow_int(base : Int, exp : Int) -> Int {
  for result = 1, b = base, e = exp; e > 0; {
    let next_result = if e % 2 != 0 { result * b } else { result }
    continue next_result, b * b, e / 2
  } else {
    result
  }
}
```

## Lowercase strings with map
- Convert to a `Char` array and map lowercasing in one expression.

Example:
```mbt
fn ascii_lower_string(value : String) -> String {
  String::from_array(value.to_array().map((ch) => ch.to_ascii_lowercase()))
}
```

## Digit scans with functional indices
- Use index-based functional `for` loops to accumulate values without `mut`.

Example:
```mbt
let acc = for i = 1, acc = 0; i < end; {
  let ch = chars[i]
  if !ch.is_ascii_digit() {
    return None
  }
  continue i + 1, acc * 10 + (ch.to_int() - '0'.to_int())
} else {
  acc
}
```

## Hex digit parsing helper
- Extract a helper that parses hex digits with functional `for` state, then reuse it in char literal parsing.

Example:
```mbt
fn parse_hex_digits(chars : Array[Char]) -> Int? {
  let (acc, has_digit) = for i = 0, acc = 0, has_digit = false;
    i < chars.length(); {
      match digit_value(chars[i]) {
        Some(digit) if digit < 16 => continue i + 1, acc * 16 + digit, true
        _ => return None
      }
    } else {
      (acc, has_digit)
    }
  if has_digit { Some(acc) } else { None }
}

match parse_hex_digits(hex_chars) {
  Some(value) => value.to_char()
  None => None
}
```

Tooling:
```bash
moon ide outline parser
```

## Reverse buffers with Array::rev
- Use `Array::rev` instead of manual reverse-iter builds.

Example:
```mbt
fn chars_rev_to_string(chars : Array[Char]) -> String {
  String::from_array(chars.rev())
}
```

## String indexing via to_array
- Convert to `Array[Char]` when you need char indices or counts without manual counters.

Example:
```mbt
fn string_to_utf8_bytes(s : String, start : Int, end : Int) -> Array[Int] raise EvalError {
  let chars = s.to_array()
  let len = chars.length()
  if start < 0 || end < start || end > len {
    raise @core.EvalError("index out of range")
  }
  let bytes : Array[Int] = []
  for i, ch in chars {
    if i >= start && i < end {
      utf8_encode_char(ch.to_int(), bytes)
    }
  }
  bytes
}
```

## Reader-friendly indexing helpers
- Use `String::to_array()` for fast Char arrays instead of manual pushes.
- Replace manual bounds checks with `Array::get` and reuse peek helpers.
- Demonstrate `..` chaining on mutating reader methods in docs/tests.

Example:
```mbt
pub fn make_reader(src : String) -> Reader {
  let chars = src.to_array()
  { chars, pos: 0, fold_case: false, labels: Map::new() }
}

fn reader_peek_offset(r : Reader, offset : Int) -> Char? {
  r.chars.get(r.pos + offset)
}

let token =
  make_reader(" ;c\nfoo")
    ..skip_ws_and_comments()
    .read_token()
```

## Functional gcd loops
- Replace mutable swap loops with functional `for` state to avoid `mut` counters.
- Apply the same pattern for BigInt gcd to keep numeric helpers consistent.

Example:
```mbt
fn gcd(a : Int, b : Int) -> Int {
  for x = int_abs(a), y = int_abs(b); y != 0; {
    continue y, x % y
  } else {
    x
  }
}
```

## Binary search with loop state
- Track `(low, high, best)` in a functional `for` loop to avoid mutable bounds.

Example:
```mbt
for low = 0, high = n, ans = 0; low <= high; {
  let mid = (low + high) / 2
  if mid == 0 {
    continue 1, high, 0
  }
  let div = n / mid
  if mid <= div {
    continue mid + 1, high, mid
  } else {
    continue low, mid - 1, ans
  }
} else {
  (ans, n - ans * ans)
}
```

## Continued fraction loops
- Carry `(p0, q0, p1, q1, frac)` through a functional loop to avoid many `mut` bindings.
- Use `break (p1, q1)` for early exit conditions.

Example:
```mbt
let (p1, q1) = for iter = 0, p0 = 1, q0 = 0, p1 = a0, q1 = 1, frac = frac0;
  iter < 64; {
    let approx = Float::from_int(p1) / Float::from_int(q1)
    let diff = if x >= approx { x - approx } else { approx - x }
    if diff <= tol || frac == 0.0 {
      break (p1, q1)
    }
    let frac_inv = Float::from_int(1) / frac
    let a = float_floor_int(frac_inv)
    let p2 = a * p1 + p0
    let q2 = a * q1 + q0
    if q2 == 0 {
      break (p1, q1)
    }
    let frac_next = frac_inv - Float::from_int(a)
    continue iter + 1, p1, q1, p2, q2, frac_next
  } else {
    (p1, q1)
  }
```

## Facade docs for re-exports
- Re-exported symbols do not inherit docstrings, so add wrapper examples or README tests.
- Keep facade examples short and focused on public entrypoints.

Example:
```mbt
///|
/// # Example
/// ```mbt check
/// test "facade eval" {
///   let value = eval_program("(+ 1 2)")
///   inspect(value_to_string(value), content="3")
/// }
/// ```
pub fn eval_program(src : String) -> Value raise {
  @eval.eval_program(src)
}
```

## Functional comparison chains
- When comparing a sequence of numeric arguments, use a functional `for` with early `break` to avoid `mut`.
- Carry the previous value as loop state to model `<`, `>`, `<=`, `>=` checks cleanly.

Example:
```mbt
fn flonum_increasing(args : Array[Value]) -> Bool {
  if args.length() <= 1 {
    true
  } else {
    let first = value_as_flonum(args[0])
    for i = 1, prev = first; i < args.length(); {
      let cur = value_as_flonum(args[i])
      if prev >= cur {
        break false
      }
      continue i + 1, cur
    } else {
      true
    }
  }
}
```
