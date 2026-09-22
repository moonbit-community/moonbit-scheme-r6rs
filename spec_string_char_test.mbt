///|
test "r6rs char and string primitives" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|#\a
        ),
      ),
    ),
    content="#\\a",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|#\space
        ),
      ),
    ),
    content="#\\space",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|#\newline
        ),
      ),
    ),
    content="#\\newline",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char? #\a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char? "a")
        ),
      ),
    ),
    content="#f",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string? "hi")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string #\a #\b)
        ),
      ),
    ),
    content="\"ab\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(make-string 3 #\x)
        ),
      ),
    ),
    content="\"xxx\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(make-string 0)
        ),
      ),
    ),
    content="\"\"",
  )
  let set_program =
    #|(let ((s (string #\a #\b #\c)))
    #|  (string-set! s 1 #\x)
    #|  s)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(set_program)),
    content="\"axc\"",
  )
  let fill_program =
    #|(let ((s (string #\a #\b #\c #\d)))
    #|  (string-fill! s #\x 1 3)
    #|  s)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(fill_program)),
    content="\"axxd\"",
  )
  let copy_program =
    #|(let ((to (string #\a #\b #\c #\d))
    #|      (from (string #\x #\y #\z)))
    #|  (string-copy! to 1 from 0 2)
    #|  to)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(copy_program)),
    content="\"axyd\"",
  )
  let overlap_program =
    #|(let ((s (string #\a #\b #\c #\d)))
    #|  (string-copy! s 1 s 0 3)
    #|  s)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(overlap_program)),
    content="\"aabc\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-length "hi")
        ),
      ),
    ),
    content="2",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-append "a" "b" "c")
        ),
      ),
    ),
    content="\"abc\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-append)
        ),
      ),
    ),
    content="\"\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-ref "hi" 0)
        ),
      ),
    ),
    content="#\\h",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-ref "hi" 1)
        ),
      ),
    ),
    content="#\\i",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-copy "hi")
        ),
      ),
    ),
    content="\"hi\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-copy "hello" 2)
        ),
      ),
    ),
    content="\"llo\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-copy "hello" 1 4)
        ),
      ),
    ),
    content="\"ell\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(substring "hello" 1 4)
        ),
      ),
    ),
    content="\"ell\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string->list "hi")
        ),
      ),
    ),
    content="(#\\h #\\i)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string->list "hello" 1)
        ),
      ),
    ),
    content="(#\\e #\\l #\\l #\\o)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string->list "hello" 1 4)
        ),
      ),
    ),
    content="(#\\e #\\l #\\l)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list->string '(#\h #\i))
        ),
      ),
    ),
    content="\"hi\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(list->string '())
        ),
      ),
    ),
    content="\"\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-map char-upcase "ab")
        ),
      ),
    ),
    content="\"AB\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-map char-downcase "AB")
        ),
      ),
    ),
    content="\"ab\"",
  )
  let for_each_program =
    #|(let ((n 0))
    #|  (string-for-each (lambda (ch) (set! n (+ n 1))) "abc")
    #|  n)
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(for_each_program)),
    content="3",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(symbol->string 'hello)
        ),
      ),
    ),
    content="\"hello\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string->symbol "hello")
        ),
      ),
    ),
    content="hello",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char=? #\a #\a #\a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char<? #\a #\b #\c)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char>? #\c #\b #\a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char<=? #\a #\a #\b)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char>=? #\c #\c #\b)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-ci=? #\A #\a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-ci<? #\A #\b)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char->integer #\A)
        ),
      ),
    ),
    content="65",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(integer->char 97)
        ),
      ),
    ),
    content="#\\a",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-alphabetic? #\a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-numeric? #\9)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-whitespace? #\space)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-upper-case? #\A)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-lower-case? #\a)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-upcase #\a)
        ),
      ),
    ),
    content="#\\A",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-downcase #\A)
        ),
      ),
    ),
    content="#\\a",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-foldcase #\A)
        ),
      ),
    ),
    content="#\\a",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-alphabetic? #\x4E2D)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-numeric? #\x0661)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-whitespace? #\x00A0)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-upper-case? #\x00C5)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-lower-case? #\x00E5)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char-ci=? #\x00C5 #\x00E5)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char->integer (char-upcase #\x00E5))
        ),
      ),
    ),
    content="197",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char->integer (char-downcase #\x00C5))
        ),
      ),
    ),
    content="229",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(char->integer (char-foldcase #\x00C5))
        ),
      ),
    ),
    content="229",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-ci=? "\x00C5;" "\x00E5;")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(map char->integer (string->list (string-upcase "\x00E5;")))
        ),
      ),
    ),
    content="(197)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(map char->integer (string->list (string-downcase "\x00C5;")))
        ),
      ),
    ),
    content="(229)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(map char->integer (string->list (string-foldcase "\x00C5;")))
        ),
      ),
    ),
    content="(229)",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string=? "hi" "hi")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string<? "a" "ab")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string>? "b" "a")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string<=? "a" "a" "b")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string>=? "c" "c" "b")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-ci=? "A" "a")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-ci<? "a" "B")
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-upcase "Abc")
        ),
      ),
    ),
    content="\"ABC\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-downcase "AbC")
        ),
      ),
    ),
    content="\"abc\"",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(string-foldcase "AbC")
        ),
      ),
    ),
    content="\"abc\"",
  )
}

///|
test "r6rs string-ref: errors" {
  try
    @scheme-r6rs.eval_program(
      (
        #|(string-ref "a" 1)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for out of range")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(list->string '(#\a 1))
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: char expected\")")
  } noraise {
    _ => fail("expected EvalError for non-char list->string")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(string-copy "a" 1 0)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid string-copy range")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(substring "hi" 1 3)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid substring range")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(string-map char-upcase "a" "bb")
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"string length mismatch\")")
  } noraise {
    _ => fail("expected EvalError for string length mismatch")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(make-string -1)
      ),
    )
  catch {
    err =>
      inspect(
        err,
        content="EvalError(\"type error: exact nonnegative integer expected\")",
      )
  } noraise {
    _ => fail("expected EvalError for invalid make-string length")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(string-set! "a" 1 #\b)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for out of range string-set!")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(string-fill! "a" #\b 1 0)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for invalid string-fill! range")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(string-copy! "a" 0 "bc")
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"index out of range\")")
  } noraise {
    _ => fail("expected EvalError for out of range string-copy!")
  }
}

///|
test "r6rs char/string compare: errors" {
  try
    @scheme-r6rs.eval_program(
      (
        #|(char=? #\a "a")
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: char expected\")")
  } noraise {
    _ => fail("expected EvalError for non-char")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(string<? "a" #\b)
      ),
    )
  catch {
    err => inspect(err, content="EvalError(\"type error: string expected\")")
  } noraise {
    _ => fail("expected EvalError for non-string")
  }
  try
    @scheme-r6rs.eval_program(
      (
        #|(integer->char -1)
      ),
    )
  catch {
    err =>
      inspect(
        err,
        content="EvalError(\"type error: valid scalar value expected\")",
      )
  } noraise {
    _ => fail("expected EvalError for invalid integer->char")
  }
}

///|
test "r6rs char/string compare: arity" {
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(char=?)")),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(char=? #\\a)")),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(string=?)")),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program("(string=? \"a\")")),
    content="#t",
  )
}

///|
test "r6rs symbol/string conversion: errors" {
  expect_error(() => @scheme-r6rs.eval_program("(symbol->string 1)"))
  expect_error(() => @scheme-r6rs.eval_program("(string->symbol 1)"))
}

///|
test "r6rs strings: large index errors" {
  expect_error(() => {
    @scheme-r6rs.eval_program(
      "(string-ref \"a\" 123456789012345678901234567890)",
    )
  })
}
