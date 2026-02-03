# `scheme-r6rs` CLI (native)

This directory is a separate MoonBit module providing a small native CLI for
`bobzhang/scheme-r6rs`.

## Run

```bash
moon run -C cmd main -- --help
moon run -C cmd main -- --version
moon run -C cmd main -- --eval "(+ 1 2)"
moon run -C cmd main -- program.scm
printf '(+ 10 32)\n' | moon run -C cmd main -- -
```

