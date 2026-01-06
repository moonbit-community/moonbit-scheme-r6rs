---
name: moonbit-refactoring
description: MoonBit-specific refactoring workflow and patterns for minimizing public APIs, modularization, method chaining, pattern matching (ArrayView/StringView), Dafny-style loop specs, and black-box tests/coverage. Use when refactoring MoonBit packages or improving docs/tests without regressions.
---

# MoonBit Refactoring Skill

## Goals
- Preserve behavior and public contracts unless explicitly changed.
- Minimize the public API with `pub` only where required.
- Prefer declarative, pattern-matching style over incidental mutation.
- Use ArrayView/StringView to avoid copies and simplify control flow.
- Add docs/tests alongside refactors to prevent regressions.

## Workflow
- Use `moon doc` to discover APIs before coding.
- Use `moon ide outline` and `moon ide find-references` to inventory and update call sites.
- Inspect `pkg.generated.mbti` (via `moon info`) to confirm the public surface.
- Apply one refactor theme at a time, then update tests/docs.
- Run `moon check` after each refactor; run `moon test` before finishing.
- Record a reusable learning example.

## Minimize public API and modularize
- Remove `pub` from helpers; keep only necessary exports.
- Prefer constructors like `Type::new` instead of public literal construction.
- Move helpers into `internal/` packages to block external imports.
- Split large files by feature; MoonBit packages are per-directory, not per-file.

Example:
```mbt
// Before: public construction everywhere
pub struct Closure { ... }

// After: controlled construction
pub struct Closure { ... }
pub fn Closure::new(id : Int, params : Array[String]) -> Closure { { id, params } }
```

## Convert free functions to methods and chain
- Move behavior onto the owning type to improve discoverability.
- Use `..` for fluent, mutating chains when it reads clearly.

Example:
```mbt
// Before
fn reader_next(r : Reader) -> Char? { ... }
let ch = reader_next(r)

// After
fn Reader::next(self : Reader) -> Char? { ... }
let ch = r.next()
```

Example (chaining):
```mbt
buf..write_string("#\\")..write_char(ch)
```

## Prefer explicit qualification over `use`
- Call imported packages with `@alias.fn` instead of `use` when it improves clarity.
- Keep call sites explicit during wide refactors.

Example:
```mbt
let n = @parser.parse_number(token)
```

## Simplify constructors when type is known
- Drop `TypePath::Constr` when the surrounding type is already known.

Example:
```mbt
match tree {
  Leaf(x) => x
  Node(left~, x, right~) => left.sum() + x + right.sum()
}
```

## Use nested pattern matching and `is`
- Replace indexing with structural patterns.
- Use nested patterns to encode invariants.
- Use `is` patterns inside `if`/`guard` to keep branches concise.

Example:
```mbt
match token {
  Some(Ident([.."@", ..rest])) => handle_at(rest)
  Some(Ident(name)) => handle_ident(name)
  None => ()
}
```

## Pattern match ArrayView and Array directly
- Pattern match arrays with `[..]`; the compiler inserts ArrayView as needed.
- Use `..` in the middle to match prefix and suffix at once.

Example:
```mbt
match items {
  [] => ()
  [head, ..tail] => handle(head, tail)
  [..prefix, mid, ..suffix] => handle_mid(prefix, mid, suffix)
}
```

## Pattern match String and StringView directly
- Avoid converting to `Array[Char]`; match strings directly.
- Use `for ch in s` for Unicode-aware iteration.
- Indexing a String/StringView yields `UInt16` code units.

Example:
```mbt
match s {
  "" => ()
  [.."let", ..rest] => handle_let(rest)
  _ => ()
}
```

## Loop specs (Dafny-style comments)
- Add specs for functional-state `for` loops.
- Skip invariants for simple `for x in xs` loops.
- Add TODO when a decreases clause is unclear (possible bug).

Example:
```mbt
for i = 0, acc = 0; i < xs.length(); {
  // invariant : 0 <= i && i <= xs.length()
  // invariant : acc == sum(xs[:i])
  // decreases : xs.length() - i
  acc = acc + xs[i]
  i = i + 1
} else { acc }
```

Example (TODO):
```mbt
// TODO(invariant) : explain loop termination for this branch
```

## Tests and docs
- Prefer black-box tests in `*_test.mbt` or `*.mbt.md`.
- Add docstring tests with `mbt check` for public APIs.

Example:
```mbt
///|
/// Return the last element of a non-empty array.
///
/// # Example
/// ```mbt check
/// test {
///   inspect(last([1, 2, 3]), content="3")
/// }
/// ```
pub fn last(xs : Array[Int]) -> Int { ... }
```

## Coverage-driven refactors
- Use coverage to target missing branches through public APIs.
- Prefer small, focused tests over white-box checks.

Commands:
```bash
moon coverage analyze -- -f summary
moon coverage analyze -- -f caret -F path/to/file.mbt
```

## Record learnings
- Write one reusable example per refactor in `refactoring-skills.md`.

Template:
```
## YYYY-MM-DD: Title
- Problem: <what was unclear>
- Change: <what was refactored>
- Result: <impact on API/tests/coverage>
- Example:
<before/after or isolated snippet>
```

Entry:
```
## 2026-01-05: Prefer range for simple index loops
- Problem: Simple increment loops obscured intent with `continue i + 1`.
- Change: Convert `for i = start; i < end; { ... continue i + 1 }` to `for i in start..<end { ... }`.
- Result: Clearer iteration and fewer loop-spec comments for trivial loops.
- Example:
// Before
for i = 0; i < len; {
  items.push(fill)
  continue i + 1
}
// After
for i in 0..<len {
  items.push(fill)
}
```

Entry:
```
## 2026-01-06: Return direct values for fixed primitive families
- Problem: Option-based dispatch hid the fact that a primitive family is closed.
- Change: Guard on the primitive kind, then return `@core.Value` directly instead of `@core.Value?`.
- Result: Cleaner match bodies and less `Some(...)` boilerplate.
- Example:
// Before
fn apply_pair_list_primitive(...) -> @core.Value? { ... Some(@core.Value::Void) ... }
// After
fn apply_pair_list_primitive(...) -> @core.Value { ... @core.Value::Void ... }
```

Entry:
```
## 2026-01-06: Drop redundant type paths in pattern matches
- Problem: Fully qualified enum variants reduced readability when the type is already known.
- Change: Use unqualified variants in `match` on a known enum type.
- Result: Shorter, clearer patterns without changing behavior.
- Example:
// Before
match prim { @core.Primitive::Vector => ... }
// After
match prim { Vector => ... }
```

Entry:
```
## 2026-01-06: Target coverage with end-to-end error cases
- Problem: Coverage gaps were mostly in parser/eval error branches.
- Change: Add black-box `eval_program` tests that exercise invalid forms.
- Result: Eval coverage reached 90% without touching internals.
- Example:
let bad = try? eval_program("(import (1))")
```

Entry:
```
## 2026-01-06: Prefer unqualified primitive variants in matches
- Problem: Long `@core.Primitive::` prefixes made large matches noisy.
- Change: Drop type paths when the matched value is already `@core.Primitive`.
- Result: Cleaner dispatch tables without changing behavior.
- Example:
// Before
match prim { @core.Primitive::CharEq => ... }
// After
match prim { CharEq => ... }
```

Entry:
```
## 2026-01-06: Convert simple index loops to range loops
- Problem: Manual `continue i + 1` obscured iteration intent in string helpers.
- Change: Use `for i in start..<end { ... }` for pure iteration loops.
- Result: Fewer invariants and simpler control flow.
- Example:
// Before
for i = 0; i < len; { ... continue i + 1 }
// After
for i in 0..<len { ... }
```

## Moon IDE commands
```bash
moon doc "<query>"
moon ide outline <dir|file>
moon ide find-references <symbol>
moon ide peek-def <symbol>
moon check
moon test
moon info
```
