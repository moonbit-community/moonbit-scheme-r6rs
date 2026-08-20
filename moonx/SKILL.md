---
name: scheme
description: Evaluate R6RS Scheme expressions and programs with the sandboxed bobzhang/scheme MoonBit CLI. Use when an agent needs to run or validate Scheme code, calculate a result with Scheme, execute a .scm file, or process Scheme source from stdin.
---

# scheme

Run the published interpreter through `moonx`; arguments after the package
coordinate go directly to the command.

## Evaluate source

Use `--eval` for source supplied as one argument:

```sh
moonx bobzhang/scheme --eval '(+ 1 2)'
```

Use stdin for multiline or generated source:

```sh
printf '%s\n' '(map (lambda (x) (* x x)) (list 1 2 3))' |
  moonx bobzhang/scheme -
```

Run a file when the Scheme source already exists in the working directory:

```sh
moonx bobzhang/scheme program.scm
```

If neither `--eval` nor a file is supplied, the command reads stdin. Use
`moonx bobzhang/scheme --help` to inspect the current options. If a newly
published version is missing from the local registry index, resolve it once
with `moonx bobzhang/scheme@latest --help`.

## Handle results

- Read the evaluated value from stdout.
- Treat a nonzero exit status as an evaluation, input, or usage failure and
  surface stderr to the user.
- Prefer `--eval` for short expressions and stdin for source containing shell
  metacharacters or multiple lines.
