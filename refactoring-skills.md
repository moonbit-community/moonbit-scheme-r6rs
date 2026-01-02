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

## Split large files by feature
- Move related `///|` blocks into focused files within the same package to keep huge modules readable.
- Keep `///|` separators attached to each moved block and avoid stray trailing separators.
- Use `moon ide outline <dir>` to locate candidate clusters before splitting.

Example:
```mbt
// eval/stdlib.mbt
fn stdlib_source() -> String { ... }
fn load_stdlib(env : Env) -> Unit raise { ... }
```

## Shrink runtime APIs by relocating eval-only helpers
- If a helper is only used by eval, move it into the eval package as a private helper and replace `@runtime.*` call sites.
- Update README doctests that referenced the removed runtime APIs.
- Regenerate `pkg.generated.mbti` with `moon info` to confirm the API surface shrank.

Example:
```mbt
// eval/datum_helpers.mbt
fn datum_list_to_array(list : @core.Datum) -> Array[@core.Datum] raise @core.EvalError {
  ...
}
```

Tooling:
```bash
moon ide find-references datum_list_to_array
moon info
```

## Table-driven env bindings
- Replace long `env_define` chains with a data table of `(name, Primitive)` and a small helper loop.
- Keep special cases (like `Cxr(chain)`) in dedicated helpers so the list stays declarative.

Example:
```mbt
let primitive_bindings : Array[(String, Primitive)] = [
  ("+", Add),
  ("cons", Cons),
]

fn define_primitives(env : Env, defs : Array[(String, Primitive)]) -> Unit {
  for i = 0; i < defs.length(); {
    // invariant : i >= 0 && i <= defs.length()
    // decreases : defs.length() - i
    // assert : i <= defs.length()
    let (name, prim) = defs[i]
    env_define(env, name, Primitive(prim))
    continue i + 1
  } else {
    ()
  }
}
```

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

## Minimize construction surface with pub vs pub(all)
- `pub` structs/enums can be pattern matched outside the package but cannot be constructed there; use this when only internal packages should create values.
- Add explicit constructors like `Type::new(...)` in the defining package and update record literals in other packages to call them.
- Use `rg -o "@core\\.[A-Za-z0-9_]+::\\{" -g "*.mbt" --glob "!core/*"` to find record-literal construction outside the package.
- Use `moon ide find-references "PortKind::OutputString"` (or `WinderKind::Proc`) to locate enum-variant construction before tightening `pub(all)` to `pub`.
- Before changing a struct to `pub`, run `moon ide find-references "SyntaxObject"` (or the type name) to make sure every record literal is covered by a constructor.

Example:
```mbt
// core/spec.mbt
pub struct Closure { ... }
pub fn Closure::new(id : Int, params : Array[String], rest : String?, body : Array[Datum], env : Env) -> Closure {
  { id, params, rest, body, env }
}

// eval
let cl = @core.Closure::new(id, params, rest, body, env)
```

## Array view pattern matching
- Match arrays directly; the compiler lowers to ArrayView, so `..rest` is a view (call `rest.to_array()` when you need an `Array`).
- Use `..` in the middle to split prefix/suffix without manual length checks.
- String/StringView follow the same pattern-matching rules as Array/ArrayView, but matches are Unicode-aware (character-based), so do not assume byte indexing semantics.
- For simple prefix/suffix checks, pattern match on `String`/`StringView` directly instead of converting to `Array[Char]`.
- Use `s[:]` to get a `StringView` for pattern matching and slicing without allocating.
- Indexing a `StringView` yields `UInt16` UTF-16 code units; use `view[i]` for efficiency when Unicode correctness is not required, or `for ch in view` to get `Char` values when you need Unicode-aware behavior.
- Slicing a `String`/`StringView` can raise on invalid UTF-16 boundaries; use `try? s[start:end]` (or `try? view.sub(...)`) and fall back on `None`/error as needed.
- When splitting tokens, prefer `view.sub(...).to_string()` to avoid `String::from_array(chars.sub(...).to_array())` allocations; keep the `try/catch` in place for boundary safety.
- When scanning `StringView` prefixes by index, compare `view[i].to_int()` against `Char` code points; map back to a `Char` with a small `if`/`match` (or unwrap `tag.to_char()`) when you need to store the tag.
- Prefer `s.char_length()` over `s.to_array().length()` when you need Unicode character counts without allocating.
- When parsing tokens, prefer `['+', ..rest]` / `['#', first, ..middle, last]` to replace length guards and index reads; keep digit loops in a small helper for reuse.
- In tight loops with a mutable `Array`, use singleton patterns like `['#']` instead of `length() == 1 && arr[0] == '#'`.
- Replace `length() <= 1` guards with `match arr { [] | [_] => ... }` to keep arity checks in the pattern.
- For chain comparisons, match `[first, ..rest]` to remove index math and keep the loop over `rest`.
- Apply the same pattern for numeric/fixnum/flonum comparison primitives to unify arity handling and simplify loops.
- For functions that need the last argument (like `apply`/`append`), match `[..prefix, last]` instead of `length() - 1` indexing.
- For non-raising helpers (like stack/env accessors), match `[.., last]` and `panic()` on `[]` to keep signatures while making the invariant explicit.
- Replace `parts[0]` + `sub(start=1)` with `[head, ..rest]` matches when decoding list-like forms.
- When `rest` is an ArrayView, iterate it directly (`for item in rest { ... }` or `for i = 0; i < rest.length(); { ... }`) and only call `to_array()` when an API demands an `Array`.
- If you switch a functional loop to `for item in rest`, remove invariant/decreases comments because `for-in` loops do not need specs.
- Prefer `ArrayView[T]` parameters for read-only helpers; callers can pass `Array[T]` without conversion.

## Detailed notes from recent refactors
- **ArrayView parameters:** Accept `ArrayView[T]` for read-only helpers to avoid conversions at call sites. This keeps APIs flexible and reduces allocations. Example:
```mbt
fn parse_version_items(items : ArrayView[@core.Datum]) -> Array[Int] raise { ... }
// Callers can pass `datum_list_to_array(expr)` or a view like `rest` directly.
```
- **Pattern-match instead of indexing:** Replace `arr[arr.length() - 1]` and `parts[0]` with `[.., last]` or `[head, ..rest]`. It encodes invariants in the pattern and makes invalid shapes explicit.
```mbt
match parts {
  [] => raise @core.EvalError("invalid")
  [head, ..rest] => eval_sequence_state(rest.to_array(), env, kont, handlers)
}
```
- **ArrayView loops:** When a match yields `..rest`, treat it as a view and iterate without conversion. This removes temporary arrays while keeping functional loops.
```mbt
match args {
  [first, ..rest] => {
    let prev = value_as_number(first)
    for i = 0, prev = prev; i < rest.length(); { ... }
  }
}
```
- **Last-arg extraction:** Use `[..prefix, last]` to split the tail once, then loop over `prefix` directly. This avoids `length()-1` math and clarifies intent.
```mbt
match args {
  [..prefix, last] => {
    for item in prefix { ... }
    let tail = value_to_datum_element(last)
    ...
  }
}
```

Example:
```mbt
match chars {
  ['#', first, ..middle, last] => ...
  ['+', ..rest] => parse_decimal_digits(rest.to_array())
  [..prefix, last] => ...
  _ => None
}
```

## Simplify constructor prefixes in patterns
- When the matched type is known, drop the `Type::` prefix in patterns to reduce noise.
- Keep explicit qualifiers in expressions where the constructor type is not obvious.
- For value predicates over `Array[@core.Value]`, switch `[@core.Value::Datum(...)]` to `[Datum(...)]` and keep `@core.Value::Datum(...)` in constructors.
- Use a quick scan to list pattern-only `@core.Value::Datum` hits before editing a whole package.
- Apply the same pattern-only replacement in `*.mbt.md` so doc tests match the new style.

Example:
```mbt
match datum {
  Pair(a, b) => ...
  Nil => ...
}

match args {
  [Datum(Symbol(name))] => ...
  _ => ...
}

let v = @core.Value::Datum(Symbol("x"))
```

Scan example:
```bash
python3 - <<'PY'
from pathlib import Path
for path in sorted(Path('eval').glob('builtins_*.mbt')):
    lines = path.read_text().splitlines()
    for i, line in enumerate(lines, 1):
        if '=>' in line and '@core.Value::Datum' in line:
            before, _ = line.split('=>', 1)
            if '@core.Value::Datum' in before:
                print(f\"{path}:{i}: {line.strip()}\")
        elif line.lstrip().startswith('|') and '@core.Value::Datum' in line:
            print(f\"{path}:{i}: {line.strip()}\")
PY
```

## Batch remove Datum constructor prefixes safely
- Use a file-local replacement for `@core.Datum::` when the file already has strong type context.
- Re-run `moon check` to catch any ambiguous constructor sites immediately.
- For large files, only touch pattern lines (before `=>` or `|` continuations) so constructor expressions keep their explicit qualifiers.

Example:
```bash
python3 - <<'PY'
from pathlib import Path
path = Path('eval/macro.mbt')
path.write_text(path.read_text().replace('@core.Datum::', ''))
PY
moon check
```

Pattern-only example:
```bash
python3 - <<'PY'
from pathlib import Path
path = Path('eval/builtins_numeric.mbt')
lines = path.read_text().splitlines()
for i, line in enumerate(lines):
    if '=>' in line:
        before, after = line.split('=>', 1)
        lines[i] = before.replace('@core.Value::', '').replace('@core.Datum::', '') + '=>' + after
    elif line.lstrip().startswith('|'):
        lines[i] = line.replace('@core.Value::', '').replace('@core.Datum::', '')
path.write_text('\\n'.join(lines) + '\\n')
PY
```

## When to keep Datum prefixes
- Constructors that share names with built-in types (`Int`, `Float`, `Bool`, `String`, `Char`) often need `@core.Datum::` in expressions.
- Patterns on a known `@core.Datum` can still drop the prefix for readability.
- In parser/runtime code, prefer a targeted pattern-only edit instead of a full-file replace.

Example:
```mbt
match datum {
  Int(n) => n
  _ => 0
}

let zero = @core.Datum::Int(0)
```

## Audit exported APIs for minimal surface
- Scrape `pkg.generated.mbti` to list exported functions and search for `@pkg.fn` usages outside the package.
- For packages that only export free functions, a simple text scan can confirm whether any exports are unused; methods need `moon ide find-references` because call sites may be unqualified.

Example (free functions only):
```bash
python3 - <<'PY'
from pathlib import Path
import re

pkg = Path('runtime/pkg.generated.mbti').read_text().splitlines()
fn_names = []
for line in pkg:
    line = line.strip()
    if line.startswith('pub fn '):
        fn_names.append(line[len('pub fn '):].split('(')[0].strip())

files = [p for p in Path('.').rglob('*.mbt') if 'runtime' not in p.parts]
files += [p for p in Path('.').rglob('*.mbt.md') if 'runtime' not in p.parts]
text = ''.join(p.read_text() for p in files)

unused = [name for name in fn_names if f'@runtime.{name}' not in text]
print('unused:', unused)
PY
```
- For methods, use `moon ide find-references Type::method` instead of text search.
- Instance calls like `binding.accessor()` will not show up as `@core.RecordFieldBinding::accessor` in text searches.

Example:
```bash
python3 - <<'PY'
from pathlib import Path
pkg = 'runtime'
mbti = Path(pkg) / 'pkg.generated.mbti'
pub = [line.split('pub fn ',1)[1].split('(')[0]\n       for line in mbti.read_text().splitlines()\n       if line.strip().startswith('pub fn ')]\nroot = Path('.')\ntext = '\\n'.join(p.read_text() for p in root.rglob('*.mbt')\n                 if pkg not in p.parts and '.mooncakes' not in p.parts)\nprint([name for name in pub if f'@{pkg}.{name}' not in text])\nPY
moon ide find-references 'Reader::read_token'
moon ide find-references 'RecordFieldBinding::accessor'
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
- For lexer-style helpers, add `mbt check` examples in the package README instead of testing private helpers directly.
- For unicode helpers, add a small compatibility-normalization example to widen coverage without changing APIs.
- For counters like `gensym`, assert prefix/inequality instead of exact suffixes to avoid ordering dependence.
- Add README tests for refactored primitives (e.g., `map`, `bytevector-copy`) to catch regressions early.
- Keep the top-level README tests small and end-to-end (e.g., `eval_program` + `value_to_string`).
- Add spec tests for multi-arity primitives so each branch stays covered after refactors.

## Wrapper roundtrip doctests
- When a public wrapper has an `into_*` method, add a tiny roundtrip doctest so the API is documented and exercised.

Example:
```mbt
///|
/// ```mbt check
/// test "unicode string into_string" {
///   let wrapped = @core.UnicodeString::new("Hi")
///   inspect(wrapped.into_string(), content="Hi")
/// }
/// ```
pub fn UnicodeString::into_string(self : UnicodeString) -> String {
  self.value
}
```

## Facade-level docs
- Add small examples on re-exported APIs so module users see usage without diving into subpackages.

## Boolean API examples
- Use `inspect(..., content="true"|"false")` in doctests to make expectations explicit.

## Error-path doctests
- Prefer `try ... catch ... noraise` for error-path doctests to avoid lint warnings.
- For parser-style `raise` functions, `try?` plus `Err(_)` checks keep docs short.

Example:
```mbt
test "parse errors" {
  let result = try? parse_program("#| unterminated")
  inspect(result is Err(_), content="true")
}
```

## Guardrails for qualifier sweeps
- After auto-qualifying, scan for nested prefixes like `@core.Value::@core.Primitive::` and fix them with targeted replacements.
- Keep local enum constructors local; use `MachineState::Eval` instead of `@core.Primitive::Eval`.
- For core enums with overlapping variant names (e.g., `Set`), qualify with the enum type (`@core.HashtableOp::Set`) to avoid clashes with `@core.Kont::Set`.
- When `Datum` wraps a `Value`, keep the nested constructors explicit (`@core.Datum::Value(@core.Value::SyntaxObject(...))`) so qualifier sweeps do not invert the enum.
- After removing `using`, update doctests and README examples to use explicit `@core.Value::Datum(@core.Datum::Int(...))`.

Example:
```mbt
// Before (broken)
@core.Primitive::Eval(expr, env, kont, handlers)
@core.Kont::Set(value)

// After
MachineState::Eval(expr, env, kont, handlers)
@core.HashtableOp::Set(value)
```

Quick checks:
```bash
rg "@core\\.Value::@core\\.Primitive::" eval
moon check
```

## Match simplification
- Group enum variants with `|` patterns when they map to the same output to reduce duplication.
- For tuple matches, only group variants when the bound variables share the same type.
- Use string prefix patterns like `[..\"x__gs\", .._rest]` in tests to avoid helper functions.

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
- Centralize slice validation (e.g., `check_slice_range(start, end, len)`) to keep error messages consistent and reduce boilerplate.

## Constructor helpers
- Factor repeated "pair of symbol + expr" patterns into a small helper to keep syntax builders consistent.

## Map iteration simplification
- Iterate with `for key, value in map` to avoid extra `get`/`keys` loops.
- Use `map.is_empty()` for quick emptiness checks instead of scanning `keys()`.

## Imports hygiene
- Remove unused type imports from `using @core { ... }` blocks when `moon` warns about them.

Example:
```mbt
using @core {
  type Port,
  // type PortKind, // unused
}
```

## Deep clone with map
- Combine `Array::map` and `Map::map` to copy nested env frames without manual `mut` loops.

Example:
```mbt
pub fn env_clone(env : Env) -> Env {
  env.map((frame) =>
    frame.map((_, binding) => Binding::new(binding.id(), binding.value()))
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

## Array arity pattern matching
- Use array patterns to enforce fixed arity and avoid extra indexing or nested length checks.
- Keep the fallback `_` branch so you can report `arity_mismatch` with `args.length()`.
- For variadic primitives, capture the first required arguments and iterate the rest to avoid manual index math.
- Preserve existing error strings by reusing `args.length()` in formatted messages when tests assert exact text.
- For `append`-style APIs, match `[]` and `[single]` first, then keep the "all but last" loop for the tail case.
- Use `[value]` patterns for 1-arity primitives to remove repeated `args[0]` indexing.
- For `(x, y, size?)` APIs, split into `[x, y]` and `[x, y, size]` branches so validation stays local.
- For wide-arity constructors, use a single `[a, b, c, d, e, f]` pattern to avoid indexing mistakes.
- Extract `value_as_number(value)` once after matching `[value]` to keep numeric ops consistent and reduce repeated lookups.
- For arithmetic with special 0/1 cases, match `[]`, `[value]`, and `_` to keep identity rules explicit.

Example:
```mbt
match args {
  [expr_value, env_value] => {
    let expr = value_as_datum(expr_value)
    let eval_env = value_as_eval_env(env_value)
    MachineState::Eval(expr, eval_env.env, kont, handlers)
  }
  _ => raise arity_mismatch(2, args.length())
}
```

Variadic example:
```mbt
match args {
  [proc_value, first_list, ..rest] => {
    let lists : Array[Datum] = []
    lists.push(value_as_datum(first_list))
    for item in rest {
      // invariant : rest.length() >= 0
      // TODO(decreases) : loop index not exposed; possible bug
      // assert : rest.length() >= 0
      lists.push(value_as_datum(item))
    }
    map_step(proc_value, lists, [], false, kont, handlers)
  }
  _ => raise arity_mismatch(2, args.length())
}
```

Optional-arity example:
```mbt
match args {
  [vec_value] => copy_slice(vec_value, start=0, end=None)
  [vec_value, start_value] => copy_slice(vec_value, start=value_as_int_index(start_value), end=None)
  [vec_value, start_value, end_value] =>
    copy_slice(
      vec_value,
      start=value_as_int_index(start_value),
      end=Some(value_as_int_index(end_value)),
    )
  _ =>
    raise @core.EvalError(
      "arity mismatch: expected 1 to 3 got \{args.length()}",
    )
}
```

## Loop specs as comments
- Write formal, type-checkable expressions in `// invariant : ...`, `// decreases : ...`, and `// assert : ...`.
- When the loop index is not exposed, add a TODO for decreases to flag potential issues.
 - For helper scanners (like hex digit accumulators), include both index bounds and accumulator non-negativity.

Example:
```mbt
for i = 0; i < items.length(); {
  // invariant : i >= 0 && i <= items.length()
  // decreases : items.length() - i
  // assert : i <= items.length()
  ...
  continue i + 1
}
```

## Split primitive dispatches
- Move large `match prim` blocks into per-domain `apply_*_primitive` helpers that return `Value?`.
- Keep `apply_primitive` as a small dispatcher and leave a `apply_primitive_core` for the remaining primitives.

Example:
```mbt
fn apply_primitive(prim : Primitive, args : Array[Value]) -> Value raise EvalError {
  match apply_char_string_primitive(prim, args) {
    Some(value) => value
    None => apply_primitive_core(prim, args)
  }
}
```

## Copy-range helpers
- Extract `check_copy_range(start, end, from_len, at, to_len)` to reuse bounds checks in copy primitives.

Example:
```mbt
let count = check_copy_range(start, end, from_items.length(), at, to_items.length())
```

## Large match extraction
- For huge primitive blocks, keep the original `match prim` as a `*_core` function returning `Value`.
- Wrap it with a small `apply_*_primitive` that returns `Value?` and only handles the listed variants.
- Use a short `python3` snippet to extract `Primitive::` names when building the wrapper list.

Example:
```mbt
fn apply_numeric_primitive(prim : Primitive, args : Array[Value]) -> Value? raise EvalError {
  match prim {
    Primitive::Add
    | Primitive::Sub
    => Some(apply_numeric_primitive_core(prim, args))
    _ => None
  }
}
```

## Slice-by-sentinel extraction
- When a match block is contiguous, use sentinel primitives to slice the block into a new file.
- Good sentinels are the first/next primitive labels like `Primitive::RecordP` and `Primitive::MakeEqHashtable`.

## Predicate grouping
- Collect related predicate/equality primitives into a dedicated dispatcher to keep `apply_primitive_core` focused on IO/control.

## Arity + type via array patterns
- Use array patterns to combine arity checks with nested type matches, reducing `match args.length()` + `match args[0]` nesting.

Example:
```mbt
match args {
  [Datum(Symbol(_))] => bool_value(true)
  [_] => bool_value(false)
  _ => raise arity_mismatch(1, args.length())
}
```

Tip: For fixed multi-arity calls, use `[a, b]`, `[a, b, c]` patterns and reuse the local names for `value_as_*` conversions.

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

Example:
```mbt
let forms : Array[Datum] = [Datum::Symbol("begin")]
for i = 2; i < items.length(); {
  forms.push(expand_template_indexed(items[i], ...))
  continue i + 1
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

## Index-based array transforms
- When you need in-place updates, use `for i = 0; i < len; { ... continue i + 1 }` to avoid `mut` counters.

Example:
```mbt
for i = 0; i < members.length(); {
  members[i] = left.members[i] || right.members[i]
  continue i + 1
}
```

## Prefix scans
- Use `break i` to return the first mismatch index in a bounded `for` loop.

Example:
```mbt
let prefix = for i = 0; i < current.length() && i < target.length(); {
  if current[i].id != target[i].id {
    break i
  }
  continue i + 1
} else {
  i
}
```

Example:
```mbt
let cmp = for i = 0; i < min_len; {
  if left[i] < right[i] {
    break -1
  }
  if left[i] > right[i] {
    break 1
  }
  continue i + 1
} else {
  0
}
```

## Reverse index scans
- Use a descending `for` with `idx = i - 1` when walking from the end.

Example:
```mbt
for i = current.length(); i > prefix; {
  let idx = i - 1
  actions.push(WindAction::After(current[idx]))
  continue i - 1
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

## Any/All scans
- Use `break false` for all-of checks and `break true` for any-of checks.

Example:
```mbt
let ok = for i = 0; i < items.length(); {
  if !pred(items[i]) {
    break false
  }
  continue i + 1
} else {
  true
}
```

## Find scans
- Use `break Some(value)` to return the first match from a loop.

Example:
```mbt
let found =
  for i = 0; i < items.length(); {
    let item = items[i]
    if pred(item) {
      break Some(item)
    }
    continue i + 1
  } else {
    None
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

Example:
```mbt
for cur = r..skip_ws_and_comments().peek(); cur is Some(_); {
  let expr = read_expr(r)
  exprs.push(expr)
  continue r..skip_ws_and_comments().peek()
} else {
  ()
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
match args {
  [first, ..rest] => {
    let current = value_as_fixnum_int(first)
    let current = for i = 0, current = current; i < rest.length(); {
      let cur = value_as_fixnum_int(rest[i])
      let next = if cur > current { cur } else { current }
      continue i + 1, next
    } else {
      current
    }
    current
  }
  _ => raise arity_mismatch(1, args.length())
}
```

Example:
```mbt
match args {
  [first, ..rest] => {
    let best = value_as_number(first)
    let has_inexact = best is Float(_)
    let (best, has_inexact) =
      for i = 0, best = best, has_inexact = has_inexact; i < rest.length(); {
        let cur = value_as_number(rest[i])
        let next_best = if num_less(best, cur) { cur } else { best }
        let next_has_inexact = has_inexact || cur is Float(_)
        continue i + 1, next_best, next_has_inexact
      } else {
        (best, has_inexact)
      }
    (best, has_inexact)
  }
  _ => raise arity_mismatch(1, args.length())
}
```

## Sentinel short-circuit
- Use `break value` to return a sentinel (like NaN) while scanning.

Example:
```mbt
let current = for i = 1, current = current; i < args.length(); {
  let cur = value_as_flonum(args[i])
  if float_is_nan(cur) {
    break cur
  }
  let next = if cur > current { cur } else { current }
  continue i + 1, next
} else {
  current
}
```

## String conversion helper
- Use a single `string_to_chars` helper for repeated String → Array[Char] conversions.
- Prefer slicing the char array for prefixes instead of manual `get_char` loops.

## Safe string indexing
- Use `get_char` inside a functional `for` to handle UTF-16 boundaries explicitly.

Example:
```mbt
let items : Array[Datum] = []
for i = 0; i < s.length(); {
  match s.get_char(i) {
    Some(ch) => items.push(Datum::Char(ch))
    None => raise @core.EvalError("index out of range")
  }
  continue i + 1
}
```

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

## Binary op folds
- Fold argument arrays with a functional `for` to avoid `mut` accumulators.

Example:
```mbt
let acc = for i = 1, acc = first; i < args.length(); {
  let next = value_as_exact_integer(args[i])
  continue i + 1, datum_bitwise_binop(acc, next, BitOp::And)
} else {
  acc
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
      Nil => break bool_value(false)
      Pair(car, cdr) => {
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
    Label(id, cell) => {
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
- Use `moon ide find-references "@core.UnicodeString::new"` to enumerate call sites before swapping to wrappers.
- Prefer `@core.UnicodeChar::new` and `@core.UnicodeString::new` at call sites to avoid `using` for constructors.
- Regenerate public API summaries with `moon info` after API conversions.

Example:
```mbt
pub struct UnicodeString {
  value : String
}

pub fn UnicodeString::new(s : String) -> UnicodeString {
  { value: s }
}

pub fn UnicodeString::foldcase(self : UnicodeString) -> UnicodeString {
  ...
}

let folded =
  @core.UnicodeString::new("e\u{301}").normalize_nfc().foldcase().into_string()
```

Quick checks:
```bash
moon ide find-references "@core.UnicodeString::new"
moon info
```

## Chained comparisons without mut
- Use functional `for` loops with `(index, prev)` state to replace `mut` when comparing adjacent values.
- Add small helpers (like `foldcase_*_if`) to keep case-insensitive logic centralized.

Example:
```mbt
fn foldcase_char_if(ch : Char, case_insensitive : Bool) -> Char {
  if case_insensitive { @core.UnicodeChar::new(ch).foldcase() } else { ch }
}

fn compare_chain_char(args : Array[Value], mode : CompareMode, case_insensitive : Bool) -> Bool raise EvalError {
  match args {
    [] | [_] => true
    [first, ..rest] => {
      let prev = foldcase_char_if(value_as_char(first), case_insensitive)
      for i = 0, prev = prev; i < rest.length(); {
        let cur = foldcase_char_if(value_as_char(rest[i]), case_insensitive)
        if !compare_ok(mode, prev.to_int().compare(cur.to_int())) {
          return false
        }
        continue i + 1, cur
      } else {
        true
      }
    }
  }
}
```

Example:
```mbt
match args {
  [] | [_] => true
  [first, ..rest] => {
    let prev = value_as_number(first)
    let ok = for i = 0, prev = prev; i < rest.length(); {
      let cur = value_as_number(rest[i])
      if !num_less(prev, cur) {
        break false
      }
      continue i + 1, cur
    } else {
      true
    }
    ok
  }
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
Vector(items) =>
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

## Copy + append helpers
- Use `values.copy()` + `push` to extend arrays instead of manual loops.

Example:
```mbt
fn values_with_push(values : Array[Value], extra : Value) -> Array[Value] {
  let next = values.copy()
  next.push(extra)
  next
}
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
    Int(n) => {
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
pub fn Reader::new(src : String) -> Reader {
  let chars = src.to_array()
  { chars, pos: 0, fold_case: false, labels: Map::new() }
}

fn reader_peek_offset(r : Reader, offset : Int) -> Char? {
  r.chars.get(r.pos + offset)
}

let token =
  Reader::new(" ;c\nfoo")
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

## Use Option loop state instead of sentinels
- Keep loop state in `Option` (`Int?`, `String?`) instead of sentinel values to avoid extra checks.
- Match on the `Option` state inside the loop to handle duplicates or early exits.

Example:
```mbt
fn find_unique_char(chars : Array[Char], target : Char) -> Int? {
  let found : Int? =
    for i = 0, found = None; i < chars.length(); {
      // invariant : i >= 0 && i <= chars.length()
      // invariant : found is None || (found is Some(idx) && idx < i)
      // decreases : chars.length() - i
      let ch = chars[i]
      if ch == target {
        match found {
          Some(_) => return None
          None => continue i + 1, Some(i)
        }
      }
      continue i + 1, found
    } else {
      found
    }
  found
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
  match args {
    [] | [_] => true
    [first, ..rest] =>
      for i = 0, prev = value_as_flonum(first); i < rest.length(); {
        let cur = value_as_flonum(rest[i])
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

## Stateful scanner loops
- When a scanner tracks multiple flags, carry them through a `for` state tuple and `break` with the current flags for post-loop validation.
- Use a tuple return to preserve `in_bar`/`started` semantics while removing `mut` variables.

Example:
```mbt
let (in_bar, started) =
  for in_bar = false, started = false, fold_case = self.fold_case; true; {
    match self.peek() {
      Some(ch) => {
        if !in_bar && is_delim(ch) {
          break (in_bar, started)
        }
        let started = true
        ignore(self.next())
        let fold_case = if !in_bar && chars.is_empty() && ch == '#' {
          if self.peek() == Some('\\') { false } else { fold_case }
        } else {
          fold_case
        }
        if in_bar && ch == '|' {
          continue false, started, fold_case
        }
        continue in_bar, started, fold_case
      }
      _ => break (in_bar, started)
    }
  } else {
    (false, false)
  }
if in_bar {
  raise @core.ParseError("unterminated identifier")
}
if !started {
  raise @core.ParseError("expected token")
}
```

## Peek-driven skip loops
- Hold the latest `peek()` value in the `for` state and update it with `continue self.peek()` to avoid `while true`.
- Keep the branch logic identical; only the loop mechanics change.

Example:
```mbt
for cur = self.peek(); true; {
  match cur {
    Some(ch) if ch.is_ascii_whitespace() => {
      ignore(self.next())
      continue self.peek()
    }
    Some(';') => {
      ignore(self.next())
      self.skip_line_comment()
      continue self.peek()
    }
    _ => break
  }
} else {
  ()
}
```

## Parser list scans
- Use `break` with a value to replace multiple `return` sites in list parsing loops.
- Drive the loop with `continue r..skip_ws_and_comments().peek()` to preserve chaining style.

Example:
```mbt
let list = for cur = r..skip_ws_and_comments().peek(); true; {
  match cur {
    Some(')') => {
      ignore(r.next())
      break list_from_items(items, Datum::Nil)
    }
    Some('.') if !r.is_ellipsis_start() => {
      ignore(r.next())
      let tail = read_expr(r)
      break list_from_items(items, tail)
    }
    _ => {
      items.push(read_expr(r))
      continue r..skip_ws_and_comments().peek()
    }
  }
} else {
  Datum::Nil
}
```

## Conditional tuple replacement
- Replace `mut` assignments across branches with a tuple result from an `if` expression.
- Rebind the final value for sign or normalization without mutation.

Example:
```mbt
let base_num = bigint_from_int(mantissa_bits.reinterpret_as_int())
let one = bigint_from_int(1)
let (num, den) = if exp_adjusted >= 0 {
  let pow = pow_bigint_int(bigint_from_int(2), exp_adjusted)
  (base_num * pow, one)
} else {
  let pow = pow_bigint_int(bigint_from_int(2), -exp_adjusted)
  (base_num, pow)
}
let num = if sign_bit != 0U { -num } else { num }
```

## Normalize with rebind
- When a single value needs adjustment, rebind it with `let` instead of using `mut`.

Example:
```mbt
let shift = count % width
let shift = if shift < 0 { shift + width } else { shift }
```

## Multi-accumulator folds
- Carry `(use_big, acc_int, acc_big)` through a `for` loop to avoid multiple `mut` variables.
- Use `continue` to update all state whenever the numeric mode changes.

Example:
```mbt
let (use_big, acc_int, acc_big) =
  for i = 0,
    use_big = false,
    acc_int = 0,
    acc_big = bigint_from_int(0);
    i < args.length(); {
    let value = value_as_exact_integer(args[i])
    match value {
      Int(n) =>
        if use_big {
          let next_acc_big = bigint_gcd(acc_big, bigint_from_int(n))
          continue i + 1, true, acc_int, next_acc_big
        } else {
          let next_acc_int = gcd(acc_int, n)
          continue i + 1, false, next_acc_int, acc_big
        }
      BigInt(n) => {
        let base_big = if use_big { acc_big } else { bigint_from_int(acc_int) }
        let next_acc_big = bigint_gcd(base_big, n)
        continue i + 1, true, acc_int, next_acc_big
      }
      _ => raise @core.EvalError("type error: integer expected")
    }
  } else {
    (use_big, acc_int, acc_big)
  }
```

## Best-match selection loops
- Keep `best : Option[T]` in the `for` state and update it when a candidate wins.
- Use `continue` to skip disqualified entries without mutating.

Example:
```mbt
let best = for i = 0, best = None; i < items.length(); {
  let entry = items[i]
  if !entry.is_candidate() {
    continue i + 1, best
  }
  let next_best = match best {
    None => Some(entry)
    Some(current) => if entry.score > current.score { Some(entry) } else { Some(current) }
  }
  continue i + 1, next_best
} else {
  best
}
```

## Dafny-style loop specs (comments)
- Annotate loops with `// invariant : ...`, `// decreases : ...`, and `// assert : ...` inside the loop body.
- If a measure is not expressible, add `// TODO(decreases) : ...` to flag the risk.
- For BigInt state, use helper measures like `bigint_bit_length_nonneg` to keep the decreases clause in `Int`.
- In nested loops, add a separate spec block per loop; for `map.keys()` iteration, use `keys().length()` in the invariant.
- For `for item in array` loops, use the container length in invariant/assert and keep a TODO decreases when the index is hidden.
- For early-return loops (e.g., emptiness checks), place the spec block before the return to keep it visible.
- Use a tiny scan script to find `for` loops without `// invariant` and work file-by-file.

Example:
```bash
python3 - <<'PY'
from pathlib import Path
for path in Path("eval").rglob("*.mbt"):
  lines = path.read_text().splitlines()
  for i, line in enumerate(lines):
    if "for " in line and "{" in line and not line.lstrip().startswith("//"):
      if not any("// invariant" in w for w in lines[i+1:i+6]):
        print(f"{path}:{i+1}: {line.strip()}")
PY
```

Example:
```mbt
for cur = list; true; {
  // invariant : items.length() >= 0
  // TODO(decreases) : list length not explicit; possible bug
  // assert :
  //   match cur {
  //     Pair(_, _) | Nil => true
  //     _ => false
  //   }
  match cur {
    Nil => break
    Pair(a, b) => {
      items.push(a.val)
      continue b.val
    }
    _ => raise @core.EvalError("type error: proper list expected")
  }
} else {
  ()
}
```

## Encapsulate reader state with small methods
- Prefer tiny public helpers (e.g. `peek_next`, `advance`) over direct field access.
- This makes call sites clearer and reduces repeated bounds logic.
- Use `moon ide find-references Reader` to update sites in one pass.
- When you need to return the receiver, wrap `..` chains in a block (`{ r..set_*(); r }`) to avoid deprecated `x..f()` value usage.

Example:
```mbt
// Before: direct field access
if r.pos + 1 < r.chars.length() && r.chars[r.pos + 1] == ';' {
  r.pos = r.pos + 2
  let _ = read_expr(r)
  read_expr(r)
}

// After: encapsulated methods
match r.peek_next() {
  Some(';') => {
    r.advance(2)
    let _ = read_expr(r)
    read_expr(r)
  }
  _ => ...
}
```

Example:
```mbt
// Before: direct field access
r.fold_case = true
match r.labels.get(label) { ... }

// After: accessors on Reader
r.set_fold_case(true)
match r.label_get(label) { ... }
```

Example:
```mbt
let r = {
  let r = Reader::new(src)
  r..set_fold_case(true)
  r
}
```

## Make struct fields opaque
- Use `pub struct` (instead of `pub(all)`) when callers should not construct or mutate fields directly.
- Provide a constructor helper like `Reader::new` and methods for the allowed operations.
- For core bindings, add accessors (`Binding::id`, `Binding::value`) and update env helpers before switching to `pub struct`.
- For record field bindings, expose `RecordFieldBinding::new` plus accessors and update eval call sites to avoid direct field reads.

Example:
```mbt
pub struct Reader {
  chars : Array[Char]
  mut pos : Int
  mut fold_case : Bool
  labels : Map[Int, Ref[Datum]]
}

pub fn Reader::new(src : String) -> Reader {
  ...
}
```

Example:
```mbt
// Before
frame[name] = Binding::{ id: binding.id, value }
match binding.value { ... }

// After
frame[name] = Binding::new(binding.id(), value)
match binding.value() { ... }
```

Example:
```mbt
// Before
bindings.push(@core.RecordFieldBinding::{
  accessor: accessor_name,
  index: idx,
  mutator: mutator_name,
})
@runtime.env_define(env, binding.accessor, ...)

// After
bindings.push(@core.RecordFieldBinding::new(accessor_name, idx, mutator_name))
@runtime.env_define(env, binding.accessor(), ...)
```

Example:
```mbt
// Before: wrapper type with a single field
pub struct Library {
  exports : Map[String, Binding]
}

pub fn Library::exports(self : Library) -> Map[String, Binding] {
  self.exports
}

// After: registry stores exports directly, smaller public surface
let library_registry : Ref[Map[String, Map[String, Binding]]] = { ref: {} }

pub fn register_library(name : String, exports : Map[String, Binding]) -> Unit {
  library_registry[][name] = exports
}

pub fn lookup_library(name : String) -> Map[String, Binding]? {
  library_registry[].get(name)
}

match lookup_library("doc/runtime-lib") {
  Some(exports) => exports.get("x")
  None => None
}
```

## README mbt check for new APIs
- Add a tiny `mbt check` snippet when exposing a new helper or method.
- This keeps docs and behavior in sync and exercises the call site.
- When touching global registries, use a unique name to avoid cross-test coupling.

Example:
```mbt
let r = Reader::new("ABC")
r.set_fold_case(true)
inspect(r.read_token(), content="abc")
```

Example:
```mbt
let binding = Binding::new(
  next_binding_id(),
  @core.Value::Datum(@core.Datum::Int(1)),
)
register_library("doc/runtime-lib", { "x": binding })
```

## Internal methods on type aliases
- Use private `Type::method` helpers to encapsulate repeated logic without changing the public API.
- Chaining with `..` keeps small constructors readable.

Example:
```mbt
fn Env::last_frame(self : Env) -> Map[String, Binding] {
  self[self.length() - 1]
}

pub fn env_define(env : Env, name : String, value : Value) -> Unit {
  let frame = env.last_frame()
  frame[name] = Binding::new(next_binding_id(), value)
}

pub fn env_new() -> Env {
  let env : Env = []
  env..push({})
  env
}
```

## Nested pattern matches for Option + struct fields
- Use nested `match` blocks when you need to inspect a struct field.
- This keeps the field-specific logic local to the successful branch.

Example:
```mbt
match env.get_binding(name) {
  Some(binding) =>
    match binding.value() {
      Macro(transformer) => Some(transformer)
      _ => None
    }
  _ => None
}
```

## Combine match arms with or-patterns
- Use `|` in patterns when two cases share the same branch.
- This keeps condition checks in one place.

Example:
```mbt
match value {
  Record(record) | Datum(Record(record)) =>
    if record_type_is_a(record.record_type, base) {
      Some(record)
    } else {
      None
    }
  _ => None
}
```

## Use Option::map for simple projections
- Prefer `opt.map(...)` over a two-branch `match` when only transforming `Some`.

Example:
```mbt
pub fn env_binding_id_optional(env : Env, name : String) -> Int? {
  env.get_binding(name).map((binding) => binding.id())
}
```

## Drop constructor prefixes in patterns
- In `match` patterns, you can omit `TypePath::` when the scrutinee type is known.
- Keep prefixes in expressions unless the type is explicit; otherwise the compiler treats the constructor name as an unbound value.

Example:
```mbt
match datum {
  Int(n) => n
  Pair(_, _) | Nil => 0
  _ => 0
}

// Expression form: keep prefix or add a type annotation.
let value = Datum::Int(1)
let typed : Datum = Int(1)
```

Example:
```mbt
let base : Datum = match datum_unlabel(base) {
  Symbol(name) => Symbol(name)
  _ => base
}
```

## Minimize public helpers
- Use `moon ide find-references` to confirm a helper is package-internal.
- Move any public README examples to docstring tests before making the helper private.
- If only one downstream package needs it, add a local helper there to avoid exporting it.
- Convert truly unused public helpers to private to shrink `pkg.generated.mbti`.
- After trimming exports, run `moon info` to refresh `pkg.generated.mbti` and prune imports.

Example:
```mbt
fn digit_value(ch : Char) -> Int? {
  match ch {
    '0'..='9' => Some(ch.to_int() - '0'.to_int())
    'a'..='f' => Some(ch.to_int() - 'a'.to_int() + 10)
    'A'..='F' => Some(ch.to_int() - 'A'.to_int() + 10)
    _ => None
  }
}
```

Example:
```mbt
fn env_lookup_binding_optional(env : Env, name : String) -> Binding? {
  env.get_binding(name)
}
```

Example:
```mbt
using @lexer { type Reader }

fn digit_value(ch : Char) -> Int? {
  match ch {
    '0'..='9' => Some(ch.to_int() - '0'.to_int())
    'a'..='f' => Some(ch.to_int() - 'a'.to_int() + 10)
    'A'..='F' => Some(ch.to_int() - 'A'.to_int() + 10)
    _ => None
  }
}
```

Example:
```mbt
// runtime before (exported helper)
pub fn enum_set_members_all_false(universe : Array[String]) -> Array[Bool] { ... }

// eval after (package-private helper)
fn enum_set_members_all_false(universe : Array[String]) -> Array[Bool] { ... }
```

Example:
```mbt
// runtime before (exported helper)
pub fn value_as_datum(value : Value) -> Datum raise EvalError { ... }

// eval after (package-private helper)
fn value_as_datum(value : Value) -> Datum raise EvalError { ... }
```

Example:
```mbt
// runtime before (exported helper)
pub fn bool_value(value : Bool) -> Value { ... }

// eval after (package-private helper)
fn bool_value(value : Bool) -> Value { ... }
```

## Fold variants into optional parameters
- Replace `fn name_with_options(...)` with a single public entrypoint + optional parameters.
- Update call sites with `moon ide find-references` before removing the old API.

Example:
```mbt
pub fn parse_program(src : String, fold_case? : Bool = false) -> Array[Datum] raise ParseError {
  ...
}

let forms = parse_program("ABC", fold_case=true)
```

Example:
```mbt
// Before: duplicate public entrypoints
pub fn parse_number_token(tok : String) -> Datum? { ... }
pub fn parse_number_token_with_radix(tok : String, radix : Int) -> Datum? { ... }

// After: optional parameter keeps one public entrypoint
pub fn parse_number_token(tok : String, radix? : Int) -> Datum? { ... }
let parsed = parse_number_token("ff", radix=16)
```

## Replace nested loops with push_iter/map
- Use `Array::push_iter` to append a whole array in one call.
- Use `Array::map` to build new arrays without a mutable accumulator.
- Extract a tiny helper when multiple branches repeat the same map.

Example:
```mbt
let components : Array[Record] = []
for arg in args {
  let parts = condition_components(arg, base)
  components.push_iter(parts.iter())
}

let items : Array[Datum] =
  components.map((record) => Datum::Record(record))
```

Example:
```mbt
fn clone_hashtable_entries(
  entries : Array[HashtableEntry],
) -> Array[HashtableEntry] {
  entries.map((entry) =>
    HashtableEntry::{
      key: entry.key,
      value: Ref::new(entry.value.val),
    }
  )
}
```

## Build strings via map/join/repeat
- Map `args` to `Char` or `String` arrays and use `String::from_array` or `Array::join`.
- For repeated chars, create a single-char string and `repeat`.

Example:
```mbt
let chars : Array[Char] = args.map((arg) => value_as_char(arg))
let s = String::from_array(chars)

let items : Array[String] = args.map((arg) => value_as_string(arg))
let appended = items.join("")

let repeated = String::from_array([fill]).repeat(len)
```

## Prefer array literals for singletons
- Use `[value]` instead of allocating and pushing manually.

Example:
```mbt
fn value_to_values(value : Value) -> Array[Value] {
  match value {
    Values(values) => values
    _ => [value]
  }
}
```

## Use Map::map for key-preserving transforms
- `Map::map` keeps keys and rewrites values in one pass.

Example:
```mbt
fn bindings_clone(
  bindings : Map[String, BindingVal],
) -> Map[String, BindingVal] {
  bindings.map((_, value) => binding_clone(value))
}
```

## Use Map::iter2 for side-effectful passes
- Iterate key/value pairs without extra lookups when mutating other structures.

Example:
```mbt
bindings.iter2().each((name, value) =>
  env_define(fenv, name, value_from_datum(binding_to_datum(value)))
)
```

## Filter iterators into arrays
- `Iter::filter(...).to_array()` keeps iteration lazy until the collection step.

Example:
```mbt
let literals : Array[String] =
  literal_set
    .keys()
    .filter((key) => !pattern_vars.contains(key))
    .to_array()
```

## Use Iter::each to avoid manual loops
- Works well with `Map::keys()` when you only need the keys.

Example:
```mbt
names.keys().each((name) => result[name] = Seq([]))
```

## Declare error types in raising lambdas
- When a closure can raise, use `fn (...) raise ErrorType { ... }` instead of `(...) =>`.

Example:
```mbt
let renames : Array[(String, String)] =
  items.map(fn (item) raise EvalError {
    let parts = datum_list_to_array(item)
    if parts.length() != 2 {
      raise @core.EvalError("invalid import set")
    }
    (parse_symbol(parts[0]), parse_symbol(parts[1]))
  })
```

## Small state helpers on private structs
- Add `State::new()` and tiny accessors to keep state logic focused in one place.
- Use `..` chaining for short builder-style sequences.

Example:
```mbt
fn PrinterState::new() -> PrinterState {
  { labels: {} }
}

fn PrinterState::label_flags(self : PrinterState, id : Int) -> Int {
  match self.labels.get(id) {
    Some(value) => value
    None => 0
  }
}

let buf = @buffer.new()
buf..write_string("#\\")..write_char(ch)
```

## Hide internal lookup tables
- Keep large data tables private unless they are part of the API; verify usage with `moon ide find-references` or `rg`.

Example:
```mbt
// before
pub let general_category_ranges : Map[String, ReadOnlyArray[Char]] = { ... }

// after
let general_category_ranges : Map[String, ReadOnlyArray[Char]] = { ... }
```

## Prefer Type::new over free constructor helpers
- Move `make_*` constructors onto the type to shrink the public API and keep call sites OO-friendly.

Example:
```mbt
// before
pub fn make_reader(src : String) -> Reader { ... }
let r = make_reader("foo")

// after
pub fn Reader::new(src : String) -> Reader { ... }
let r = Reader::new("foo")
```

## Pull numeric helpers into eval to slim runtime
- If a runtime helper is only used by eval, move it into `eval/builtins_helpers.mbt`, drop it from `using @runtime`, and remove the exported function.

Example:
```mbt
// before (eval/imports.mbt)
using @runtime { gcd, normalize_rat, bigint_from_int, ... }

// after (eval/builtins_helpers.mbt)
fn gcd(a : Int, b : Int) -> Int { ... }
fn normalize_rat(num : Int, den : Int) -> Datum? { ... }
```

## Hide id counters behind constructors
- Keep `next_*_id` private; expose semantic constructors and reuse existing helpers like `make_hashtable` for copies.

Example:
```mbt
// before
export_map[name] = Binding::new(next_binding_id(), SyntaxKeyword(name))
Hashtable(Hashtable::{ id: next_hashtable_id(), mutable, equiv, hash, entries })

// after
export_map[name] = make_binding(SyntaxKeyword(name))
let cloned = make_hashtable(table.equiv, table.hash, mutable)
cloned.entries.val = clone_hashtable_entries(table.entries.val)
Hashtable(cloned)
```

## Shrink reader APIs after checking references
- Use `moon ide find-references` to confirm a public method is only used internally, then replace call sites and make it private.

Example:
```mbt
// before
moon ide find-references "Reader::advance"
r.advance(2)

// after
ignore(r.next())
ignore(r.next())
fn Reader::advance(self : Reader, count : Int) -> Unit { ... }
```

## Drop redundant TypePath::Constr
- Prefer unqualified constructors in patterns when the scrutinee type is known; keep qualifiers in expressions unless the constructor is in scope (same package or explicitly imported).

Example:
```mbt
// before
match datum {
  Datum::Int(n) => n
  Datum::Rat(n, d) => n / d
  _ => 0
}

// after
match datum {
  Int(n) => n
  Rat(n, d) => n / d
  _ => 0
}
```

## Add specs only for functional `for` loops
- Skip invariant/decreases/assert comments for `for item in items { ... }` loops; keep specs for state-update `for i = 0, ...; ... { ... }` loops.
- If a decreases clause cannot be expressed for a functional loop, leave a `TODO(decreases)` note there only.
- For token scans, keep index-bound invariants (`i >= 0 && i <= len`) plus `decreases : len - i`.
- For loops driven by a growing measure (e.g., BigInt until out-of-range), add `TODO(decreases)` and assert monotonic growth instead.

Example:
```mbt
// before
for item in items {
  // invariant : items.length() >= 0
  // TODO(decreases) : loop index not exposed; possible bug
  // assert : items.length() >= 0
  consume(item)
}

// after
for item in items {
  consume(item)
}

// keep specs for functional loops
for i = 0, j = xs.length(); i < j; {
  // invariant : i >= 0 && i <= j && j <= xs.length()
  // decreases : j - i
  // assert : i <= j
  ...
} else {
  ...
}
```

Example:
```mbt
for i = 0, has_dot = false; i < tok.length(); {
  // invariant : i >= 0 && i <= tok.length()
  // decreases : tok.length() - i
  // assert : i <= tok.length()
  ...
}

for exp = 0, value = bigint_from_int(1);
  bigint_to_int_option(value) is Some(_); {
  // invariant : exp >= 0
  // invariant : value.compare_int(1) >= 0
  // TODO(decreases) : value grows; bound to Int range not explicit
  // assert : exp >= 0
  ...
}
```

## Prefer explicit package qualifiers over `using`
- Replace `using @pkg { type Foo }` with explicit `@pkg.Foo` in signatures and `@pkg.Foo::Constr` at construction/pattern sites, and update docstring/README tests to match.
- For facade packages, remove `pub using` re-exports to keep the public API minimal; use `moon doc "@pkg.fn"` to confirm signatures, then update call sites to use `@pkg.fn` explicitly.

Example:
```mbt
// before
using @core { type Datum }
fn parse_number_token(tok : String) -> Datum? { ... }
match parse_number_token("10") {
  Some(Int(10)) => ()
  _ => fail("expected integer")
}

// after
fn parse_number_token(tok : String) -> @core.Datum? { ... }
match parse_number_token("10") {
  Some(@core.Datum::Int(10)) => ()
  _ => fail("expected integer")
}
```

Example (facade):
```mbt
// before
pub using @parser { parse_program }
let forms = parse_program("(+ 1 2)")

// after
let forms = @parser.parse_program("(+ 1 2)")
```

## Audit public API before narrowing
- Use `moon ide find-references` on each `pub` symbol to confirm whether it is used outside the package before making it private.
- If a runtime helper is only used by eval, move it into an eval helper file and drop the runtime export; replace `@runtime.fn` with `fn`.

Example:
```bash
moon ide find-references "Reader::peek_next"
moon ide find-references "Reader::label_get"
```

Example:
```bash
moon ide find-references "condition_base_type"
```

```mbt
// before
let base = @runtime.condition_base_type()

// after
let base = condition_base_type()
```

## Use tuple pattern matches to collapse duplicated branches
- Extract repeated parse branches into a helper and use `match (kind, flag)` to keep the decision logic in one place.
- This keeps comma/unquote parsing in one helper and reduces drift between syntax and datum variants.

Example:
```mbt
enum CommaKind {
  Unquote
  Unsyntax
}

fn read_comma_form(r : @lexer.Reader, kind : CommaKind) -> @core.Datum raise @core.ParseError {
  let splicing = match r.peek() {
    Some('@') => {
      ignore(r.next())
      true
    }
    _ => false
  }
  let expr = read_expr(r)
  match (kind, splicing) {
    (Unquote, true) => make_unquote_splicing(expr)
    (Unquote, false) => make_unquote(expr)
    (Unsyntax, true) => make_unsyntax_splicing(expr)
    (Unsyntax, false) => make_unsyntax(expr)
  }
}
```

## Prefer array pattern matches over length and index checks
- Match on `Array` directly; the compiler will treat it as an `ArrayView` for pattern matching.
- This keeps parsing helpers concise and removes manual `length()`/index logic.
- For state machines that store arrays (like kont stacks), use `[head, ..tail]` and call `tail.to_array()` only when the next state requires an `Array`.
- If the old code used `sub(start=1)` (which panics on empty), add an explicit `[] => panic()` branch to preserve the same failure behavior.

Example:
```mbt
// before
let parts = datum_list_to_array(binding)
if parts.length() != 2 {
  raise @core.EvalError("invalid binding")
}
let name = parse_symbol(parts[0])
let value = parts[1]

// after
match datum_list_to_array(binding) {
  [name_expr, value_expr] => {
    let name = parse_symbol(name_expr)
    let value = value_expr
    ...
  }
  _ => raise @core.EvalError("invalid binding")
}
```

## Use rest patterns to encode arity and special markers
- `[head, arg]` matches exactly two items; `[head, arg, ..]` matches two or more.
- Pair with guards to handle markers like `=>` without manual length checks.
- Use middle rests like `[..prefix, last]` or `[head, ..mid, tail]` to split out tails while validating prefixes.
- If a form needs “at least N”, match `[head, ..rest]` and guard on `rest.length() >= N - 1` to keep the arity check local.
- For “N required + variadic tail” forms, pattern match `[a, b, ..rest]` and pass `rest.to_array()` (or iterate the view directly) instead of `args.sub(start=2)`.

Example:
```mbt
match parts {
  [_] => state = MachineState::Apply(test_value, next, handlers)
  [_, arrow, proc] if @runtime.symbol_name(arrow) is Some("=>") =>
    state = MachineState::Eval(proc, env, @core.Kont::CondArrow(test_value, next), handlers)
  [_, arrow, ..] if @runtime.symbol_name(arrow) is Some("=>") =>
    raise @core.EvalError("invalid cond")
  _ => {
    let body = parts.sub(start=1).to_array()
    state = eval_sequence_state(body, env, next, handlers)
  }
}
```

Example (split last element):
```mbt
match datum_list_to_array(expr) {
  [] => raise @core.EvalError("invalid library name")
  [..prefix, last] => {
    for part in prefix {
      match part {
        Symbol(_) => ()
        _ => raise @core.EvalError("invalid library name")
      }
    }
    match last {
      Symbol(_) => ()
      Pair(_, _) | Nil => ()
      _ => raise @core.EvalError("invalid library name")
    }
  }
}
```

## Use StringView emptiness patterns to drop sentinel flags
- Match `[]` vs `_` on `StringView` to handle empty input and avoid `has_digit` locals.
- Keep the digit loop simple; return `None` on the first non-digit.

Example:
```mbt
fn parse_decimal_digits(chars : StringView) -> Int? {
  match chars {
    [] => None
    _ => {
      let mut acc = 0
      for ch in chars {
        if !ch.is_ascii_digit() {
          return None
        }
        acc = acc * 10 + (ch.to_int() - '0'.to_int())
      }
      Some(acc)
    }
  }
}
```

## Parse StringView prefixes with rest patterns
- Use `['#', tag, ..rest]` to consume tagged prefixes without indexing.
- Return the remaining `StringView` and carry state (radix/exactness) through recursion.
- Pattern-match `[]` on the remaining view to reject prefix-only tokens.

Example:
```mbt
fn parse_number_prefix(view : StringView) -> (StringView, Int, Char?)? {
  fn step(view : StringView, radix : Int, exactness : Char?) -> (StringView, Int, Char?)? {
    match view {
      ['#', tag, ..rest] =>
        match tag {
          'b' | 'B' => step(rest, 2, exactness)
          'e' | 'E' =>
            match exactness {
              Some(_) => None
              None => step(rest, radix, Some(tag))
            }
          _ => None
        }
      _ => Some((view, radix, exactness))
    }
  }
  step(view, 10, None)
}
```

## Parser error coverage tips
- `;` starts a line comment, so avoid `#\x;` in tests; prefer `#\xZZ` or `#\` when exercising invalid-char paths.
- Label definitions are only recognized when the token ends with `=`, so use whitespace like `#1= #1= 1` to trigger duplicate-label errors.

Example:
```mbt
let duplicate_label = try? parse_program("#1= #1= 1")
inspect(duplicate_label is Err(_), content="true")
```

## Polar complex tokens
- `r@theta` parsing goes through `parse_complex_token`, which converts both sides to floats before calling `make_complex_datum`.
- When the imaginary part is zero, `make_complex_datum` collapses the result to a real value, so expect `Float` not `Complex`.

Example:
```mbt
match parse_number_token("1@0") {
  Some(Float(f)) if f == 1.0 => ()
  _ => fail("expected 1.0")
}
```

## Runtime coverage notes
- Use `@core.SyntaxObject::new` with known scopes to hit `syntax_add_scope` branches (same scope vs new scope).
- Call `reset_record_type_registry()` before registering aliases in tests to keep registries isolated.

Example:
```mbt
reset_record_type_registry()
let record_type = @core.RecordType::new(50, "doc/alias", None, false, false, None, [])
let ctor_desc = @core.RecordConstructorDescriptor::new(50, record_type, None, None)
let desc = @core.RecordTypeDescriptor::new(50, record_type, ctor_desc)
register_record_type_alias("doc/alias", desc)
```

## Audit exports before shrinking a package API
- Use a quick script to find exported runtime helpers that are only used inside eval.
- Move those helpers into eval (as `fn`), then remove the runtime export and run `moon info` to refresh `pkg.generated.mbti`.

Example audit snippet:
```bash
python3 - <<'PY'
import re
from pathlib import Path
root = Path('.')
mbti = (root / 'runtime' / 'pkg.generated.mbti').read_text()
fn_names = re.findall(r'^pub fn\\s+([A-Za-z0-9_]+)\\(', mbti, flags=re.M)
text = ''.join(
    p.read_text() for p in root.rglob('*.mbt')
    if not str(p).startswith('runtime/')
)
unused = [n for n in fn_names if f'@runtime.{n}' not in text]
print('\\n'.join(unused))
PY
```

## White-box tests for internal branches
- Add `*_wbtest.mbt` files inside the package to call private helpers directly.
- Prefer small `test` blocks that only check the error/branch you need, using `try?` for expected errors.

Example:
```mbt
test "apply primitive core internal errors" {
  let result = try? apply_primitive_core(@core.Primitive::Apply, [])
  inspect(result is Err(_), content="true")
}
```

## Unicode normalization coverage
- Use Hangul syllables to exercise decomposition/composition, e.g. NFC of `\u{1100}\u{1161}\u{11A8}` → `\u{AC01}`.
- Use compatibility-decomposable characters like `\u{2460}` to hit the NFKD compat map.

## Cover printer branches with constructor-built values
- Use core constructors (RecordType::new/Record::new/Condition::new) to exercise
  `Datum::Record` and `Datum::Condition` without touching registries.
- Build a cyclic `Label` via `Ref` to hit the `#n#` branch in the printer.

Example:
```mbt
let cell = Ref::new(@core.Datum::Nil)
let label = @core.Datum::Label(1, cell)
cell.val = label
inspect(value_to_string(@core.Value::Datum(label)), content="#1=#1#")
```

## Coverage-guided black-box tests
- Use `moon coverage analyze -p <pkg>` to find uncovered branches, then add `README.mbt.md` tests that exercise public APIs.
- Prefer `try?` + `inspect(result is Err(_))` for error paths to avoid panics while keeping tests black-box.

Example:
```mbt
test "parse number edge cases" {
  match parse_number_token("0/5") {
    Some(Int(0)) => ()
    _ => fail("expected 0")
  }
  match parse_number_token("#i1/2") {
    Some(Float(f)) if f == 0.5 => ()
    _ => fail("expected inexact")
  }
  match parse_number_token("1e") {
    None => ()
    _ => fail("expected None")
  }
}

test "bytevector arity errors" {
  let result = try? eval_program("(bytevector-copy)")
  inspect(result is Err(_), content="true")
}
```

Notes:
- For parser coverage, use `#e`/`#i` prefixes and mixed radices (like `#i#x10`) to drive exactness and inexact conversion branches.
- Use large integers or large rationals (`1000000000000/1000000000001`) to exercise BigInt/BigRat code paths.
- For eval primitive coverage, add black-box tests that return lists of booleans/ints, so multiple branches are asserted in one eval call.
- Use `(char->integer (char-upcase ...))` and `(string->utf8 "hi" 1)` style calls to cover char/string and optional-arg paths without relying on platform-specific outputs.
- For list/vector coverage, include both normal flows and arity errors; `vector-copy!` with 3/4/5 args hits all copy branches in a single test block.
- For numeric predicates, mix exact/inexact literals and complex constructors (`make-rectangular`) to hit exact?/inexact?/zero?/finite?/nan? branches in one test.

## Eval control-flow coverage tips
- Use labeled expressions like `#1=(+ 1 2)` to exercise the evaluator's `Label` unwrapping without changing runtime behavior.
- For `cond`/`case`/`guard` arrow clauses, include one valid `=>` path and one invalid extra-argument path to cover both happy-path and error branches.
- Use `parameterize` with multiple bindings and a converter `(make-parameter 1 (lambda (x) ...))` to hit ParameterizeParam/Value/Convert branches, plus a `try?` case for non-parameter errors.
- A custom equivalence `(lambda (a b) (eq? a b))` in `make-hashtable` forces `HashtableFindResult` to take both false and true paths when scanning entries.
- For UTF-16 boundary errors, use a hex escape like `"\\x1F600;"` and request slices at index `1` to trigger surrogate-boundary failures without adding non-ASCII text.
- `string-copy!` and `string-fill!` each have 3+ arity variants; add 3-arg/4-arg calls to cover the buffer and partial-fill branches.
- `guard` only traps Scheme-raised conditions; primitive `EvalError` failures (like invalid record descriptors) must be caught with `try? eval_program` and setup repeated per program.
- `eval_program` resets the record type registry, so tests that rely on existing UIDs must build the base descriptor and the mismatched call in the same program string.
- To make a package internal (Go-style), move it under `<parent>/internal/<pkg>` and update `moon.pkg.json` imports to the new path; keep aliases so call sites don’t churn, then run `moon info` to regenerate `pkg.generated.mbti`.
- When internalizing a concrete package, update every importing `moon.pkg.json` entry to the new internal path (alias stays the same), then run `moon ide find-references` or `rg` to confirm no old path remains.
- Update public-facing README examples to use facade functions instead of `@internal/...` packages so docs reinforce the minimal API surface.
- Replace nested `match` chains with `is` pattern checks when you only need a single constructor, which keeps intent clear and removes one level of indentation.
- Flatten double-dispatch matches by matching on tuples (e.g. `(pat, inp)`) and use `is` guards with precomputed head names to avoid repeated `symbol_name` calls.
- Use nested patterns like `Values([single])` to replace a `match` inside a `match` when destructuring a wrapper around an array.
- Prefer `if opt is Some(x)` for Option checks inside loops to keep the happy path linear and avoid extra nested `match` blocks.
- When the same exactness conversion logic appears multiple times, extract a helper (like `apply_inexact_exactness`) to collapse repeated nested matches.

Tooling example:
```bash
moon ide outline runtime
moon ide find-references value_to_string
git mv runtime internal/runtime
moon info
moon check
moon test
```

Example:
```mbt
test "cond/case arrow coverage" {
  inspect(@runtime.value_to_string(eval_program(
    "(cond ((+ 1 2) => (lambda (x) (+ x 1))) (else 0))",
  )), content="4")
  let bad = try? eval_program("(case 2 ((2) => (lambda (x) x) 1) (else 0))")
  inspect(bad is Err(_), content="true")
}
```

Pattern match in `is` example:
```mbt
let (car, cdr) = if @runtime.datum_unlabel(datum) is Pair(a, b) {
  (a.val, b.val)
} else {
  raise @core.EvalError("type error: pair expected")
}
```

Tuple + guard example:
```mbt
let head_name = match parts {
  [head, ..] => @runtime.symbol_name(head)
  _ => None
}
match (pat, inp) {
  (Bool(a), Bool(b)) => a == b
  (Bool(_), _) => false
  _ => false
}
match parts {
  [_, arg] if head_name is Some("parent") => parse_symbol(arg)
  _ => ()
}
```

Array destructuring example:
```mbt
match value {
  Values([single]) => single
  Values(_) => raise @core.EvalError("multiple values in single-value context")
  _ => value
}
```

Exactness helper example:
```mbt
fn apply_inexact_exactness(exactness : Char?, datum : @core.Datum) -> @core.Datum? {
  match exactness {
    Some('i') | Some('I') =>
      match datum_to_float(datum) {
        Some(f) => Some(@core.Datum::Float(f))
        None => None
      }
    _ => Some(datum)
  }
}
```

## Coverage-driven refactor/test loop
- `moon coverage analyze -p eval -- -f summary` focuses on a single package; tests must live in that package (e.g., `eval/README.mbt.md`) to affect the numbers.
- `moon coverage analyze -p eval -- -f caret -F eval/builtins_numeric.mbt` pinpoints exact uncovered branches to target with tests.
- After moving package paths, run `moon clean` before `moon coverage analyze -- -f summary` to clear stale file paths.

Example: add error-branch coverage with `try?` and confirm with caret output:
```mbt
test "hashtable arity errors" {
  let err_arity = try? eval_program("(hashtable?)")
  inspect(err_arity is Err(_), content="true")
}
```

## Macro-path test scaffolding
- Use `syntax->datum` to assert results while still exercising `quasisyntax`/`syntax` paths.
- Prefer `try? eval_program(...)` for arity and exception branches so tests stay black-box.

Example:
```mbt
test "quasisyntax splicing" {
  let value =
    eval_program(
      "(syntax->datum (quasisyntax ((unsyntax-splicing (syntax (1 2))) 3)))",
    )
  inspect(@runtime.value_to_string(value), content="(1 2 3)")
}
```

## Identifier scope probes
- Compare identifiers built from `(datum->syntax #f ...)` vs `(syntax ...)` to hit scope-equality branches.

Example:
```mbt
test "identifier scopes" {
  let value =
    eval_program(
      "(begin (define a (datum->syntax #f 'x)) (define b (syntax x)) (list (free-identifier=? a b) (bound-identifier=? a b)))",
    )
  inspect(@runtime.value_to_string(value), content="(#f #f)")
}
```

## Reuse token helpers with nested matches
- Slice `StringView` with `try` and feed it into shared helpers (like `parse_decimal_digits`) to reduce duplicated loops and `mut`.

Example:
```mbt
fn parse_label_token(tok : String) -> (Bool, Int)? {
  let view = tok[:]
  match view {
    ['#', .., last] =>
      match last {
        '=' | '#' => {
          let digits = try view[1:view.length() - 1] catch {
            _ => return None
          } noraise { view => view }
          parse_decimal_digits(digits).map((acc) => (last == '=', acc))
        }
        _ => None
      }
    _ => None
  }
}
```

## Minimal API audit for internal packages
- Use `moon ide outline <dir>` to list exported symbols, then `rg "@pkg\\." -g '*.mbt'` to confirm which ones are referenced across packages before removing `pub`.
- After narrowing the surface, run `moon info` so `pkg.generated.mbti` reflects the new public API.

Example: inline vector/bytevector dispatch and drop helper methods from the lexer package.
```mbt
match r.peek_next() {
  Some('(') => {
    ignore(r.next())
    ignore(r.next())
    read_vector_literal(r)
  }
  _ => {
    let tok = r.read_token()
    match tok {
      "#vu8" =>
        match r.peek() {
          Some('(') => {
            ignore(r.next())
            read_bytevector_literal(r)
          }
          _ => token_to_datum(tok)
        }
      _ => token_to_datum(tok)
    }
  }
}
```

## Coverage without fragile float assertions
- When exercising complex-number branches, assert with `(complex? ...)` to avoid exact float string comparisons while still covering complex math paths.
- Use a minimal BigInt literal (like `2147483648`) and arithmetic to force BigInt branches without huge exponent costs.

Example:
```mbt
test "complex math coverage" {
  let flags =
    eval_program(
      "(list (complex? (sin (make-rectangular 1 1))) (complex? (exp (make-rectangular 1 1))))",
    )
  inspect(@runtime.value_to_string(flags), content="(#t #t)")
}
```

## Coverage from arity mismatch probes
- Use a tiny `expect_err` helper with `try? eval_program` to cover arity mismatch branches for fixnum/flonum primitives in one place.
- Pair it with a single "sign/zero" test for `fxdiv/fxmod` to exercise the adjustment logic and division-by-zero guards.

Example:
```mbt
test "numeric arity mismatch coverage" {
  let expect_err = (expr : String) => {
    let err = try? eval_program(expr)
    guard err is Err(_) else { fail(expr) }
  }
  expect_err("(fxnot)")
  expect_err("(flsqrt)")
  expect_err("(fldiv-and-mod 1.0)")
}
```

## Macro transformer coverage
- Procedure-style macros (`define-syntax` with `lambda` + `syntax-case`) hit `rename_proc_datum` paths that `syntax-rules` does not.
- Use vector/complex literals (and `datum->syntax`) in macro outputs to cover vector/complex rename + scope-walk branches.

Example:
```mbt
test "macro transformer vector" {
  let value =
    eval_program(
      "(begin (define-syntax make-vec (lambda (stx) (syntax-case stx () ((_ x) #'#(x x))))) (make-vec 5))",
    )
  inspect(@runtime.value_to_string(value), content="#(5 5)")
}
```
