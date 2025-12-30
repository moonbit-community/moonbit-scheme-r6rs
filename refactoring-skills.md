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

## Indexing helpers
- Extract tiny helpers (like "last frame") to avoid repeated index math and keep mutations localized.

## Constructor helpers
- Factor repeated "pair of symbol + expr" patterns into a small helper to keep syntax builders consistent.

## Map iteration simplification
- Iterate with `for key, value in map` to avoid extra `get`/`keys` loops.

## Functional loops
- Prefer `for i, v in array` over manual `while` counters when possible to reduce `mut`.
  - Useful for delimiter scans like `@` or `/` in numeric tokens.

## Multi-state loops
- Use functional `for` with multiple state variables to replace `mut` accumulators in small numeric loops.

## Shared constants
- Lift repeated lookup tables (like radix digit arrays) to a single `let` to avoid duplication.

## Reverse assembly
- Extract a small helper for reversing `Array[Char]` into a `String` to cut repeated loops.

## Reader consumption
- Use `Reader::next()` (with `ignore(...)` if needed) to advance instead of direct `pos` mutation.

## Lookahead helpers
- Add a small `reader_peek_offset` helper to centralize bounded lookahead when scanning delimiters.

## First-token handling
- Replace manual index counters with a `first` flag when parsing optional leading signs.

## String conversion helper
- Use a single `string_to_chars` helper for repeated String → Array[Char] conversions.
- Prefer slicing the char array for prefixes instead of manual `get_char` loops.

## Streamed digit accumulation
- Accumulate numeric values during parsing instead of collecting digits into an array first.

## Prefix tag scanning
- Use functional `for` with `continue` to advance by fixed steps (like `#`-prefixed numeric tags).

## Reverse scans
- Isolate right-to-left split detection into a helper to keep main parsers linear.

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
