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
