# Copilot Instructions for moonbit-scheme-r6rs

## Project Overview
An R6RS Scheme interpreter written in **MoonBit**, implementing core Scheme features including a reader, evaluator, macro expansion system (`syntax-rules`), and runtime. The project uses MoonBit's build system (`moon`), not traditional package managers.

## Architecture

### Core Components (Phase 1 Scope)
- **[spec.mbt](spec.mbt)**: Type definitions for `Datum` (reader data), `Value` (runtime values), `Env` (lexical environment), `Primitive` (built-in functions), and `SyntaxRules` (macro transformers)
- **[reader.mbt](reader.mbt)**: Parser that converts Scheme source strings to `Datum` structures (s-expressions)
- **[eval.mbt](eval.mbt)**: Evaluator with `eval_expr()` and special form handlers (`eval_if`, `eval_lambda`, `eval_define`, etc.)
- **[env.mbt](env.mbt)**: Environment management (`env_new`, `env_extend`, `env_lookup`, `env_define`, `env_set`) using `Array[Map[String, Value]]` for lexical scoping
- **[macro.mbt](macro.mbt)**: Macro expansion engine implementing `syntax-rules` with ellipsis (`...`) support for pattern matching and template expansion
- **[builtins.mbt](builtins.mbt)**: Primitive procedures (`+`, `-`, `*`, `=`, `<`, `cons`, `car`, `cdr`, etc.) via `apply_primitive()`
- **[printer.mbt](printer.mbt)**: `value_to_string()` and `datum_to_string()` for output formatting (booleans as `#t`/`#f`, empty list as `()`)
- **[runtime.mbt](runtime.mbt)**: Helper functions for type conversions (`value_as_int`, `value_as_datum`) and list operations (`datum_list_to_array`)

### Data Flow
```
Scheme source → reader.mbt (parse) → Datum tree
              → eval.mbt (eval_expr) → macro expansion (if applicable) → Value
              → printer.mbt (value_to_string) → output
```

## Key Conventions

### MoonBit-Specific Patterns
- **Doc comments**: Use `///|` prefix (note the pipe character)
- **Error handling**: Use `raise` keyword with custom error types (`ParseError`, `EvalError`)
- **Match expressions**: Exhaustive pattern matching required; use guard clauses like `if` in patterns
- **Array operations**: `.push()`, `.length()`, `.copy()`, `.sub(start=n)`, `.rev_iter()` methods
- **Mutable state**: Declare with `mut` keyword (e.g., `let mut i = 0`)
- **String interpolation**: Use `\{expr}` syntax

### Environment Model
- Environments are stacks of frames: `type Env = Array[Map[String, Value]]`
- Lookups search from innermost (last) frame to outermost (first)
- `env_extend()` creates new scope by copying array and adding frame
- Macros stored as `Value::Macro` in environment, checked before procedure application

### Macro System
- **Pattern matching**: Recursive matching in [macro.mbt](macro.mbt#L59-L119) handles literals, wildcards (`_`), ellipsis (`...`), and nested structures
- **Ellipsis semantics**: `...` in patterns/templates creates bindings as `Array[Datum]`; handled by `match_ellipsis()` and `expand_ellipsis()`
- **No hygiene**: Variables captured directly; no renaming (Phase 1 limitation)

### Testing Patterns
- Tests use `inspect()` with `content=` parameter for assertions
- Test files organized by difficulty: `spec_easy_test.mbt`, `spec_mid_test.mbt`, `spec_difficult_test.mbt`, plus separate macro tests
- Test naming: `test "r6rs <category>: <description>"`

## Development Workflow

### Building and Testing
```bash
moon build          # Compile project
moon test           # Run all tests
moon check          # Type-check without building
moon run            # Run main package (if exists)
moon clean          # Remove target/ directory
```

### Adding New Features
1. Update type definitions in [spec.mbt](spec.mbt) if needed
2. Implement primitive in [builtins.mbt](builtins.mbt) via `apply_primitive()` match arm
3. Add special form handler in [eval.mbt](eval.mbt) via `eval_list()` match arm
4. Add test cases following existing patterns in `spec_*_test.mbt` files

### Common Pitfalls
- **String comparison in patterns**: Use `name == "symbol"` not `is`
- **List construction**: Build backwards with `rev_iter()` for efficiency (see [runtime.mbt](runtime.mbt#L74-L79))
- **Error context**: Include variable names in `EvalError` messages (e.g., `"unbound variable: \{name}"`)
- **Arity checking**: Handle variadic cases explicitly (see `Primitive::Sub` with 0/1/n args in [builtins.mbt](builtins.mbt#L16-L32))

## Out of Scope (Phase 1)
Characters, vectors, bytevectors, records, ports, exceptions, full numeric tower, hygiene in macros. Stick to exact integers, booleans, symbols, strings, pairs/lists, and implemented special forms.
