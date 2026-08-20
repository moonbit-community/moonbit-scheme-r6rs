# `scheme-r6rs` CLI (native)

This directory is a separate MoonBit module providing a small native CLI for
`bobzhang/scheme-r6rs`.

## Run

```bash
moon -C cmd run main -- --help
moon -C cmd run main -- --version
moon -C cmd run main -- --eval "(+ 1 2)"
moon -C cmd run main -- program.scm
printf '(+ 10 32)\n' | moon -C cmd run main -- -
```
