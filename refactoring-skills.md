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
