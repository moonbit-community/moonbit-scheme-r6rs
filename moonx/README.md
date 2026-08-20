# Scheme for moonx

Run R6RS Scheme expressions and programs without installing a separate binary:

```sh
moonx bobzhang/scheme --eval '(+ 1 2)'
printf '%s\n' '(map (lambda (x) (* x x)) (list 1 2 3))' | moonx bobzhang/scheme -
moonx bobzhang/scheme program.scm
```

Arguments after `bobzhang/scheme` are passed directly to the interpreter. The
default `moonx` WebAssembly target runs the command in a local WASI sandbox.

Options:

- `-e`, `--eval EXPR`: evaluate the supplied Scheme source.
- `-`: read Scheme source from stdin.
- `-h`, `--help`: show command help.
- `--version`: show the command version.

If neither `--eval` nor a file is supplied, the command reads from stdin.
