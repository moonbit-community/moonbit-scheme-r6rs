///|
test "r6rs char and string primitives" {
  inspect(
    value_to_string(eval_program(
      (
        #|#\a
      )
    )),
    content="#\\a",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|#\space
      )
    )),
    content="#\\space",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|#\newline
      )
    )),
    content="#\\newline",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char? #\a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char? "a")
      )
    )),
    content="#f",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string? "hi")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string #\a #\b)
      )
    )),
    content="\"ab\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(make-string 3 #\x)
      )
    )),
    content="\"xxx\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(make-string 0)
      )
    )),
    content="\"\"",
  )
  let set_program = (
    (
      #|(let ((s (string #\a #\b #\c)))
      #|  (string-set! s 1 #\x)
      #|  s)
    )
  )
  inspect(value_to_string(eval_program(set_program)), content="\"axc\"")
  let fill_program = (
    (
      #|(let ((s (string #\a #\b #\c #\d)))
      #|  (string-fill! s #\x 1 3)
      #|  s)
    )
  )
  inspect(value_to_string(eval_program(fill_program)), content="\"axxd\"")
  let copy_program = (
    (
      #|(let ((to (string #\a #\b #\c #\d))
      #|      (from (string #\x #\y #\z)))
      #|  (string-copy! to 1 from 0 2)
      #|  to)
    )
  )
  inspect(value_to_string(eval_program(copy_program)), content="\"axyd\"")
  let overlap_program = (
    (
      #|(let ((s (string #\a #\b #\c #\d)))
      #|  (string-copy! s 1 s 0 3)
      #|  s)
    )
  )
  inspect(value_to_string(eval_program(overlap_program)), content="\"aabc\"")
  inspect(
    value_to_string(eval_program(
      (
        #|(string-length "hi")
      )
    )),
    content="2",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-append "a" "b" "c")
      )
    )),
    content="\"abc\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-append)
      )
    )),
    content="\"\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-ref "hi" 0)
      )
    )),
    content="#\\h",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-ref "hi" 1)
      )
    )),
    content="#\\i",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-copy "hi")
      )
    )),
    content="\"hi\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-copy "hello" 2)
      )
    )),
    content="\"llo\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-copy "hello" 1 4)
      )
    )),
    content="\"ell\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(substring "hello" 1 4)
      )
    )),
    content="\"ell\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->list "hi")
      )
    )),
    content="(#\\h #\\i)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(list->string '(#\h #\i))
      )
    )),
    content="\"hi\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(list->string '())
      )
    )),
    content="\"\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-map char-upcase "ab")
      )
    )),
    content="\"AB\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-map char-downcase "AB")
      )
    )),
    content="\"ab\"",
  )
  let for_each_program = (
    (
      #|(let ((n 0))
      #|  (string-for-each (lambda (ch) (set! n (+ n 1))) "abc")
      #|  n)
    )
  )
  inspect(value_to_string(eval_program(for_each_program)), content="3")
  inspect(
    value_to_string(eval_program(
      (
        #|(symbol->string 'hello)
      )
    )),
    content="\"hello\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string->symbol "hello")
      )
    )),
    content="hello",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char=? #\a #\a #\a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char<? #\a #\b #\c)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char>? #\c #\b #\a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char<=? #\a #\a #\b)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char>=? #\c #\c #\b)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-ci=? #\A #\a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-ci<? #\A #\b)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char->integer #\A)
      )
    )),
    content="65",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(integer->char 97)
      )
    )),
    content="#\\a",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-alphabetic? #\a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-numeric? #\9)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-whitespace? #\space)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-upper-case? #\A)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-lower-case? #\a)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-upcase #\a)
      )
    )),
    content="#\\A",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-downcase #\A)
      )
    )),
    content="#\\a",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-foldcase #\A)
      )
    )),
    content="#\\a",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-alphabetic? #\x4E2D)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-numeric? #\x0661)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-whitespace? #\x00A0)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-upper-case? #\x00C5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-lower-case? #\x00E5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char-ci=? #\x00C5 #\x00E5)
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char->integer (char-upcase #\x00E5))
      )
    )),
    content="197",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char->integer (char-downcase #\x00C5))
      )
    )),
    content="229",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(char->integer (char-foldcase #\x00C5))
      )
    )),
    content="229",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-ci=? "\x00C5;" "\x00E5;")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(map char->integer (string->list (string-upcase "\x00E5;")))
      )
    )),
    content="(197)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(map char->integer (string->list (string-downcase "\x00C5;")))
      )
    )),
    content="(229)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(map char->integer (string->list (string-foldcase "\x00C5;")))
      )
    )),
    content="(229)",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string=? "hi" "hi")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string<? "a" "ab")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string>? "b" "a")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string<=? "a" "a" "b")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string>=? "c" "c" "b")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-ci=? "A" "a")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-ci<? "a" "B")
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-upcase "Abc")
      )
    )),
    content="\"ABC\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-downcase "AbC")
      )
    )),
    content="\"abc\"",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(string-foldcase "AbC")
      )
    )),
    content="\"abc\"",
  )
}

///|
test "r6rs string-ref: errors" {
  let out_of_range = try? eval_program(
    (
      #|(string-ref "a" 1)
    )
  )
  match out_of_range {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for out of range")
  }

  let bad_char_list = try? eval_program(
    (
      #|(list->string '(#\a 1))
    )
  )
  match bad_char_list {
    Err(err) => inspect(err, content="EvalError(\"type error: char expected\")")
    _ => fail("expected EvalError for non-char list->string")
  }

  let bad_copy = try? eval_program(
    (
      #|(string-copy "a" 1 0)
    )
  )
  match bad_copy {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for invalid string-copy range")
  }

  let bad_substring = try? eval_program(
    (
      #|(substring "hi" 1 3)
    )
  )
  match bad_substring {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for invalid substring range")
  }

  let bad_map = try? eval_program(
    (
      #|(string-map char-upcase "a" "bb")
    )
  )
  match bad_map {
    Err(err) => inspect(err, content="EvalError(\"string length mismatch\")")
    _ => fail("expected EvalError for string length mismatch")
  }

  let bad_make = try? eval_program(
    (
      #|(make-string -1)
    )
  )
  match bad_make {
    Err(err) => inspect(err, content="EvalError(\"type error: exact nonnegative integer expected\")")
    _ => fail("expected EvalError for invalid make-string length")
  }

  let bad_set = try? eval_program(
    (
      #|(string-set! "a" 1 #\b)
    )
  )
  match bad_set {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for out of range string-set!")
  }

  let bad_fill = try? eval_program(
    (
      #|(string-fill! "a" #\b 1 0)
    )
  )
  match bad_fill {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for invalid string-fill! range")
  }

  let bad_copy_bang = try? eval_program(
    (
      #|(string-copy! "a" 0 "bc")
    )
  )
  match bad_copy_bang {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for out of range string-copy!")
  }
}

///|
test "r6rs char/string compare: errors" {
  let bad_char = try? eval_program(
    (
      #|(char=? #\a "a")
    )
  )
  match bad_char {
    Err(err) => inspect(err, content="EvalError(\"type error: char expected\")")
    _ => fail("expected EvalError for non-char")
  }

  let bad_string = try? eval_program(
    (
      #|(string<? "a" #\b)
    )
  )
  match bad_string {
    Err(err) => inspect(err, content="EvalError(\"type error: string expected\")")
    _ => fail("expected EvalError for non-string")
  }

  let bad_code = try? eval_program(
    (
      #|(integer->char -1)
    )
  )
  match bad_code {
    Err(err) => inspect(err, content="EvalError(\"type error: valid scalar value expected\")")
    _ => fail("expected EvalError for invalid integer->char")
  }
}
