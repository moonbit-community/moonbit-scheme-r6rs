///|
test "reader: datum and block comments" {
  let program : String =
    #|(begin
    #|  (list 1 #;2 3 #| drop |# 4))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(1 3 4)",
  )
}

///|
test "reader: nested block comments" {
  let program : String =
    #|(begin
    #|  (list 1 #| outer #| inner |# outer |# 2))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(1 2)",
  )
}

///|
test "reader: delimited identifiers" {
  let program : String =
    #|(begin
    #|  (symbol->string '|a b|))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="\"a b\"",
  )
}

///|
test "reader: character literals" {
  let program : String =
    #|(begin
    #|  (list
    #|    (char->integer #\x41)
    #|    (char->integer #\space)
    #|    (char->integer #\alarm)
    #|    (char->integer #\escape)
    #|    (char->integer #\delete)
    #|    (char->integer #\null)
    #|    (char->integer #\return)))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(65 32 7 27 127 0 13)",
  )
}

///|
test "reader: string escapes" {
  let program : String =
    #|(begin
    #|  (map char->integer (string->list "A\x41;\n\t\v\f\r\b\a")))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(65 65 10 9 11 12 13 8 7)",
  )
}

///|
test "reader: fold-case directives" {
  let program : String =
    #|(begin
    #|  #!fold-case
    #|  (define a (symbol->string 'AbC))
    #|  (define b (symbol->string '|AbC|))
    #|  #!no-fold-case
    #|  (list a b (symbol->string 'AbC)))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(\"abc\" \"AbC\" \"AbC\")",
  )
}

///|
test "reader: fold-case unicode identifiers" {
  let program : String =
    #|(begin
    #|  #!fold-case
    #|  (define \x03A9; 42)
    #|  \x03C9;)
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="42",
  )
}

///|
test "reader: identifier hex escapes" {
  let program : String =
    #|(begin
    #|  (symbol->string '|a\x41;|))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="\"aA\"",
  )
}

///|
test "reader: string line continuation" {
  let program : String =
    #|(begin
    #|  (map char->integer (string->list "a\
    #|    b")))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(97 98)",
  )
}

///|
test "reader: datum labels" {
  let program : String =
    #|(begin
    #|  (define x '#1=(a b))
    #|  (define y '#1#)
    #|  (set-car! y 'z)
    #|  x)
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="#1=(z b)",
  )
}

///|
test "reader: datum labels cycle" {
  let program : String =
    #|(begin
    #|  (define x '#1=(a . #1#))
    #|  x)
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="#1=(a . #1#)",
  )
}
