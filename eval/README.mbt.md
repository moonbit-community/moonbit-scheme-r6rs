# eval

Evaluator and macro expander for Scheme R6RS.

## Entry points

- eval_program and eval_program_all evaluate full source strings
- register_include_source supplies in-memory (include ...) content

## Example

```mbt nocheck
///|
let program = "(define x 1) (+ x 2)"

///|
let result = @eval.eval_program(program)
```

```mbt check
///|
fn[T] expect_error(f : () -> T raise) -> Unit raise {
  try f() catch {
    _ => ()
  } noraise {
    _ => fail("expected expression to raise")
  }
}

///|
test "evaluate program" {
  let value = eval_program("(+ 1 2)")
  inspect(@runtime.value_to_string(value), content="3")
  let values = eval_program_all("(define x 1) (+ x 2)")
  inspect(values.length(), content="2")
  inspect(@runtime.value_to_string(values[1]), content="3")
}

///|
test "evaluate program returns last value" {
  let value = eval_program("(begin (define x 1) (+ x 4))")
  inspect(@runtime.value_to_string(value), content="5")
}

///|
test "empty program" {
  let values = eval_program_all("")
  inspect(values.length(), content="0")
}

///|
test "empty program value" {
  let value = eval_program("")
  inspect(@runtime.value_to_string(value), content="#<void>")
}

///|
test "eval error" {
  expect_error(() => eval_program("(car 1)"))
}

///|
test "numeric comparisons" {
  let value = eval_program("(and (= 1 1 1) (< 1 2 3) (>= 3 2 1))")
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "fixnum comparisons" {
  let value = eval_program("(and (fx=? 1 1 1) (fx<? 1 2 3) (fx>=? 3 2 1))")
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "fixnum comparison false cases" {
  let value = eval_program(
    "(list (fx=? 1 2) (fx<? 1 3 2) (fx>? 3 1 2) (fx<=? 3 2 1) (fx>=? 1 2 3))",
  )
  inspect(@runtime.value_to_string(value), content="(#f #f #f #f #f)")
}

///|
test "bitwise reverse bit field" {
  let value = eval_program("(bitwise-reverse-bit-field #b1011 0 4)")
  inspect(@runtime.value_to_string(value), content="13")
}

///|
test "bitwise folds" {
  let value = eval_program("(bitwise-and #b1111 #b1100 #b1010)")
  inspect(@runtime.value_to_string(value), content="8")
}

///|
test "map over lists" {
  let value = eval_program("(map + '(1 2) '(3 4))")
  inspect(@runtime.value_to_string(value), content="(4 6)")
}

///|
test "let bindings" {
  let value = eval_program("(let ((x 2) (y 3)) (+ x y))")
  inspect(@runtime.value_to_string(value), content="5")
}

///|
test "flonum min/max" {
  let value = eval_program("(list (flmax 1.0 2.5 2.0) (flmin 1.0 -1.0 0.0))")
  inspect(@runtime.value_to_string(value), content="(2.5 -1)")
}

///|
test "bytevector copy" {
  let value = eval_program("(bytevector-copy #vu8(1 2 3) 1 3)")
  inspect(@runtime.value_to_string(value), content="#vu8(2 3)")
}

///|
test "bytevector primitives" {
  let is_bv = eval_program("(bytevector? #vu8(1 2))")
  inspect(@runtime.value_to_string(is_bv), content="#t")
  let not_bv = eval_program("(bytevector? '(1 2))")
  inspect(@runtime.value_to_string(not_bv), content="#f")
  let length = eval_program("(bytevector-length #vu8(1 2 3))")
  inspect(@runtime.value_to_string(length), content="3")
  let ref_value = eval_program("(bytevector-u8-ref #vu8(10 20) 1)")
  inspect(@runtime.value_to_string(ref_value), content="20")
  let set_value = eval_program(
    "(let ((bv (make-bytevector 3 7))) (bytevector-u8-set! bv 1 9) bv)",
  )
  inspect(@runtime.value_to_string(set_value), content="#vu8(7 9 7)")
  let eq_false = eval_program("(bytevector=? #vu8(1 2) #vu8(1 2 3))")
  inspect(@runtime.value_to_string(eq_false), content="#f")
  let eq_true = eval_program("(bytevector=? #vu8(1 2) #vu8(1 2))")
  inspect(@runtime.value_to_string(eq_true), content="#t")
  let copy_all = eval_program("(bytevector-copy #vu8(1 2 3))")
  inspect(@runtime.value_to_string(copy_all), content="#vu8(1 2 3)")
  let copy_start = eval_program("(bytevector-copy #vu8(1 2 3) 1)")
  inspect(@runtime.value_to_string(copy_start), content="#vu8(2 3)")
  let copy_bang_full = eval_program(
    "(let ((dst (make-bytevector 4 0)) (src #vu8(1 2 3))) (bytevector-copy! dst 0 src) dst)",
  )
  inspect(@runtime.value_to_string(copy_bang_full), content="#vu8(1 2 3 0)")
  let copy_bang_start = eval_program(
    "(let ((dst (make-bytevector 4 0)) (src #vu8(1 2 3))) (bytevector-copy! dst 0 src 1) dst)",
  )
  inspect(@runtime.value_to_string(copy_bang_start), content="#vu8(2 3 0 0)")
  let copy_bang_slice = eval_program(
    "(let ((dst (make-bytevector 4 0)) (src #vu8(1 2 3))) (bytevector-copy! dst 1 src 0 2) dst)",
  )
  inspect(@runtime.value_to_string(copy_bang_slice), content="#vu8(0 1 2 0)")
  let fill = eval_program(
    "(let ((bv #vu8(1 2 3))) (bytevector-fill! bv 7 1 3) bv)",
  )
  inspect(@runtime.value_to_string(fill), content="#vu8(1 7 7)")
  let fill_start = eval_program(
    "(let ((bv #vu8(1 2 3))) (bytevector-fill! bv 9 1) bv)",
  )
  inspect(@runtime.value_to_string(fill_start), content="#vu8(1 9 9)")
  let append = eval_program("(bytevector-append #vu8(1 2) #vu8(3 4))")
  inspect(@runtime.value_to_string(append), content="#vu8(1 2 3 4)")
  let to_list = eval_program("(bytevector->u8-list #vu8(1 2 3))")
  inspect(@runtime.value_to_string(to_list), content="(1 2 3)")
  let to_list_slice = eval_program("(bytevector->u8-list #vu8(1 2 3 4) 1 3)")
  inspect(@runtime.value_to_string(to_list_slice), content="(2 3)")
  let from_list = eval_program("(u8-list->bytevector '(1 2 3))")
  inspect(@runtime.value_to_string(from_list), content="#vu8(1 2 3)")
}

///|
test "bytevector utf8 helpers" {
  let utf8 = eval_program("(string->utf8 \"hi\")")
  inspect(@runtime.value_to_string(utf8), content="#vu8(104 105)")
  let utf8_start = eval_program("(string->utf8 \"hi\" 1)")
  inspect(@runtime.value_to_string(utf8_start), content="#vu8(105)")
  let utf8_range = eval_program("(string->utf8 \"hi\" 0 1)")
  inspect(@runtime.value_to_string(utf8_range), content="#vu8(104)")
  let text = eval_program("(utf8->string #vu8(104 105))")
  inspect(@runtime.value_to_string(text), content="\"hi\"")
  let text_start = eval_program("(utf8->string #vu8(104 105) 1)")
  inspect(@runtime.value_to_string(text_start), content="\"i\"")
  let text_range = eval_program("(utf8->string #vu8(104 105) 0 1)")
  inspect(@runtime.value_to_string(text_range), content="\"h\"")
}

///|
test "bytevector arity errors" {
  expect_error(() => eval_program("(bytevector?)"))
  expect_error(() => eval_program("(bytevector-length)"))
  expect_error(() => eval_program("(bytevector-u8-ref #vu8(1))"))
  expect_error(() => eval_program("(bytevector-u8-set! #vu8(1) 0)"))
  expect_error(() => eval_program("(bytevector-copy)"))
  expect_error(() => eval_program("(bytevector-copy! #vu8(1) 0)"))
  expect_error(() => eval_program("(bytevector-fill! #vu8(1))"))
  expect_error(() => eval_program("(bytevector->u8-list)"))
  expect_error(() => eval_program("(u8-list->bytevector)"))
  expect_error(() => eval_program("(u8-list->bytevector '(1 a))"))
  expect_error(() => eval_program("(string->utf8)"))
  expect_error(() => eval_program("(utf8->string)"))
  expect_error(() => eval_program("(native-endianness 1)"))
  expect_error(() => eval_program("(bytevector-uint-ref #vu8(1) 0)"))
  expect_error(() => eval_program("(bytevector-sint-ref #vu8(1) 0)"))
  expect_error(() => eval_program("(bytevector-uint-set! #vu8(1) 0 0 1)"))
  expect_error(() => eval_program("(bytevector-sint-set! #vu8(1) 0 0 1)"))
}

///|
test "char and string primitives" {
  let char_ci = eval_program(
    "(list (char-ci>? #\\b #\\A) (char-ci<=? #\\a #\\A) (char-ci>=? #\\A #\\b))",
  )
  inspect(@runtime.value_to_string(char_ci), content="(#t #t #f)")
  let char_preds = eval_program(
    "(list (char? #\\a) (char? 1) (char-alphabetic? #\\a) (char-numeric? #\\9) (char-whitespace? #\\space) (char-upper-case? #\\A) (char-lower-case? #\\a))",
  )
  inspect(
    @runtime.value_to_string(char_preds),
    content="(#t #f #t #t #t #t #t)",
  )
  let char_cases = eval_program(
    "(list (char->integer (char-upcase #\\a)) (char->integer (char-downcase #\\A)) (char->integer (char-foldcase #\\A)) (char->integer (integer->char 66)))",
  )
  inspect(@runtime.value_to_string(char_cases), content="(65 97 97 66)")
  let string_ci = eval_program(
    "(list (string-ci>? \"b\" \"A\") (string-ci<=? \"a\" \"A\") (string-ci>=? \"A\" \"b\"))",
  )
  inspect(@runtime.value_to_string(string_ci), content="(#t #t #f)")
  let string_basic = eval_program(
    "(let ((s (string-copy \"hi\"))) (string-set! s 0 #\\H) (list (string? s) (string? 1) (string-length s) (string-append \"a\" \"b\") (char->integer (string-ref s 1)) s))",
  )
  inspect(
    @runtime.value_to_string(string_basic),
    content="(#t #f 2 \"ab\" 105 \"Hi\")",
  )
}

///|
test "char and string arity errors" {
  expect_error(() => eval_program("(char?)"))
  expect_error(() => eval_program("(char->integer)"))
  expect_error(() => eval_program("(integer->char)"))
  expect_error(() => eval_program("(char-alphabetic?)"))
  expect_error(() => eval_program("(char-numeric?)"))
  expect_error(() => eval_program("(char-whitespace?)"))
  expect_error(() => eval_program("(char-upper-case?)"))
  expect_error(() => eval_program("(char-lower-case?)"))
  expect_error(() => eval_program("(char-upcase)"))
  expect_error(() => eval_program("(char-downcase)"))
  expect_error(() => eval_program("(char-foldcase)"))
  expect_error(() => eval_program("(char-general-category)"))
  expect_error(() => eval_program("(make-string)"))
  expect_error(() => eval_program("(string?)"))
  expect_error(() => eval_program("(string-length)"))
  expect_error(() => eval_program("(string-ref \"a\")"))
  expect_error(() => eval_program("(string-set! \"a\" 0)"))
  expect_error(() => eval_program("(string-copy \"a\" 0 1 2)"))
  expect_error(() => eval_program("(string-copy! \"a\" 0)"))
  expect_error(() => eval_program("(string-fill! \"a\")"))
  expect_error(() => eval_program("(string-upcase)"))
  expect_error(() => eval_program("(string-downcase)"))
  expect_error(() => eval_program("(string-foldcase)"))
  expect_error(() => eval_program("(string-normalize-nfc)"))
  expect_error(() => eval_program("(string-normalize-nfd)"))
  expect_error(() => eval_program("(string-normalize-nfkc)"))
  expect_error(() => eval_program("(string-normalize-nfkd)"))
  expect_error(() => eval_program("(substring \"a\" 0)"))
}

///|
test "string copy and fill variants" {
  let copy_full = eval_program(
    "(let ((to (string #\\a #\\b #\\c)) (from (string #\\x #\\y))) (string-copy! to 1 from) to)",
  )
  inspect(@runtime.value_to_string(copy_full), content="\"axy\"")
  let copy_start = eval_program(
    "(let ((to (string #\\a #\\b #\\c #\\d)) (from (string #\\w #\\x #\\y #\\z))) (string-copy! to 0 from 1) to)",
  )
  inspect(@runtime.value_to_string(copy_start), content="\"xyzd\"")
  let fill_all = eval_program(
    "(let ((s (string #\\a #\\b #\\c))) (string-fill! s #\\x) s)",
  )
  inspect(@runtime.value_to_string(fill_all), content="\"xxx\"")
  let fill_from = eval_program(
    "(let ((s (string #\\a #\\b #\\c #\\d))) (string-fill! s #\\x 2) s)",
  )
  inspect(@runtime.value_to_string(fill_from), content="\"abxx\"")
}

///|
test "string boundary errors" {
  expect_error(() => eval_program("(string-ref \"\\x1F600;\" 1)"))
  expect_error(() => eval_program("(string-copy \"\\x1F600;\" 1)"))
  expect_error(() => eval_program("(string-copy \"\\x1F600;\" 0 1)"))
  expect_error(() => eval_program("(substring \"\\x1F600;\" 0 1)"))
}

///|
test "string surrogate boundary errors" {
  expect_error(() => eval_program("(string->list \"\\x1F600;\" 1)"))
  expect_error(() => eval_program("(string->vector \"\\x1F600;\" 1)"))
  expect_error(() => eval_program("(string->vector \"\\x1F600;\" 0 1)"))
  expect_error(() => {
    eval_program(
      "(let ((dst (string #\\a #\\b)) (src \"\\x1F600;\")) (string-copy! dst 0 src 1))",
    )
  })
  expect_error(() => {
    eval_program(
      "(let ((dst (string #\\a #\\b)) (src \"\\x1F600;\")) (string-copy! dst 0 src 1 2))",
    )
  })
}

///|
test "string copy and list error branches" {
  expect_error(() => {
    eval_program(
      "(let ((dst (string #\\a #\\b)) (src \"\\x1F600;\")) (string-copy! dst 0 src))",
    )
  })
  expect_error(() => {
    eval_program(
      "(let ((dst (string-copy \"\\x1F600;\")) (src \"ab\")) (string-copy! dst 0 src))",
    )
  })
  expect_error(() => {
    eval_program(
      "(let ((dst (string #\\a #\\b)) (src \"\\x1F600;\")) (string-copy! dst 0 src 1))",
    )
  })
  expect_error(() => {
    eval_program(
      "(let ((dst (string-copy \"\\x1F600;\")) (src \"ab\")) (string-copy! dst 0 src 0))",
    )
  })
  expect_error(() => {
    eval_program(
      "(let ((dst (string #\\a #\\b)) (src \"\\x1F600;\")) (string-copy! dst 0 src 1 2))",
    )
  })
  expect_error(() => {
    eval_program(
      "(let ((dst (string-copy \"\\x1F600;\")) (src \"ab\")) (string-copy! dst 0 src 0 1))",
    )
  })
  expect_error(() => eval_program("(string->list \"\\x1F600;\")"))
  expect_error(() => eval_program("(string->list \"\\x1F600;\" 1 2)"))
  expect_error(() => eval_program("(string->list \"a\" 0 1 2)"))
  expect_error(() => eval_program("(list->string)"))
}

///|
test "string type errors" {
  expect_error(() => eval_program("(string-length 1)"))
  expect_error(() => eval_program("(string-length (lambda (x) x))"))
  expect_error(() => eval_program("(string-set! 1 0 #\\a)"))
  expect_error(() => eval_program("(string-set! (lambda (x) x) 0 #\\a)"))
  expect_error(() => eval_program("(string-fill! (string #\\a) 1)"))
  expect_error(() => eval_program("(string-fill! (string #\\a) (lambda (x) x))"))
  expect_error(() => eval_program("(string-fill! (string #\\x1F600;) #\\a 0)"))
  expect_error(() => eval_program("(list->string (lambda (x) x))"))
}

///|
test "pair and list primitives" {
  let pair = eval_program(
    "(let ((p (cons 1 2))) (set-car! p 3) (set-cdr! p 4) p)",
  )
  inspect(@runtime.value_to_string(pair), content="(3 . 4)")
  let car_value = eval_program("(car (cons 1 2))")
  inspect(@runtime.value_to_string(car_value), content="1")
  let cdr_value = eval_program("(cdr (cons 1 2))")
  inspect(@runtime.value_to_string(cdr_value), content="2")
  let cadr_value = eval_program("(cadr '(1 2 3))")
  inspect(@runtime.value_to_string(cadr_value), content="2")
  let make_list = eval_program("(make-list 3 'x)")
  inspect(@runtime.value_to_string(make_list), content="(x x x)")
}

///|
test "pair and list arity errors" {
  expect_error(() => eval_program("(cons 1)"))
  expect_error(() => eval_program("(set-car! (cons 1 2))"))
  expect_error(() => eval_program("(set-cdr! (cons 1 2))"))
  expect_error(() => eval_program("(set-car! (lambda (x) x) 1)"))
  expect_error(() => eval_program("(set-cdr! (lambda (x) x) 1)"))
  expect_error(() => eval_program("(car)"))
  expect_error(() => eval_program("(cdr)"))
  expect_error(() => eval_program("(car (lambda (x) x))"))
  expect_error(() => eval_program("(cdr (lambda (x) x))"))
  expect_error(() => eval_program("(cadr)"))
  expect_error(() => eval_program("(cadr 1)"))
  expect_error(() => eval_program("(make-list)"))
  expect_error(() => eval_program("(make-list 1 2 3)"))
  expect_error(() => eval_program("(null?)"))
  expect_error(() => eval_program("(pair?)"))
  let list_false = eval_program("(list? (lambda (x) x))")
  inspect(@runtime.value_to_string(list_false), content="#f")
  expect_error(() => eval_program("(list?)"))
  expect_error(() => eval_program("(length (lambda (x) x))"))
  expect_error(() => eval_program("(length)"))
  expect_error(() => eval_program("(append (lambda (x) x) '(1))"))
}

///|
test "list primitive errors" {
  let expect_err = (expr : String) => expect_error(() => eval_program(expr))
  let improper = eval_program("(list? (cons 1 2))")
  inspect(@runtime.value_to_string(improper), content="#f")
  expect_err("(reverse 1)")
  expect_err("(reverse)")
  expect_err("(reverse (lambda (x) x))")
  expect_err("(list-ref)")
  expect_err("(list-ref '(1) 0 1)")
  expect_err("(list-tail)")
  expect_err("(list-tail '(1) 0 1)")
  expect_err("(list-copy)")
  expect_err("(list-copy '(1) 2)")
  expect_err("(member)")
  expect_err("(memq)")
  expect_err("(memv)")
  expect_err("(assoc)")
  expect_err("(assq)")
  expect_err("(assv)")
}

///|
test "vector primitives" {
  let vec = eval_program("(vector 1 2 3)")
  inspect(@runtime.value_to_string(vec), content="#(1 2 3)")
  let make_vec = eval_program("(make-vector 3 'a)")
  inspect(@runtime.value_to_string(make_vec), content="#(a a a)")
  let vec_p = eval_program("(vector? 1)")
  inspect(@runtime.value_to_string(vec_p), content="#f")
  let vec_len = eval_program("(vector-length #(1 2 3))")
  inspect(@runtime.value_to_string(vec_len), content="3")
  let vec_ref = eval_program("(vector-ref #(1 2 3) 1)")
  inspect(@runtime.value_to_string(vec_ref), content="2")
  let vec_set = eval_program("(let ((v (vector 1 2 3))) (vector-set! v 1 9) v)")
  inspect(@runtime.value_to_string(vec_set), content="#(1 9 3)")
  let vec_fill = eval_program(
    "(let ((v (vector 1 2 3))) (vector-fill! v 9 1) v)",
  )
  inspect(@runtime.value_to_string(vec_fill), content="#(1 9 9)")
  let vec_copy = eval_program("(vector-copy #(1 2 3) 1)")
  inspect(@runtime.value_to_string(vec_copy), content="#(2 3)")
  let vec_copy_bang = eval_program(
    "(let ((dst (make-vector 4 0)) (src #(1 2 3))) (vector-copy! dst 0 src) dst)",
  )
  inspect(@runtime.value_to_string(vec_copy_bang), content="#(1 2 3 0)")
  let vec_copy_bang_start = eval_program(
    "(let ((dst (make-vector 4 0)) (src #(1 2 3))) (vector-copy! dst 0 src 1) dst)",
  )
  inspect(@runtime.value_to_string(vec_copy_bang_start), content="#(2 3 0 0)")
  let vec_copy_bang_slice = eval_program(
    "(let ((dst (make-vector 4 0)) (src #(1 2 3))) (vector-copy! dst 1 src 0 2) dst)",
  )
  inspect(@runtime.value_to_string(vec_copy_bang_slice), content="#(0 1 2 0)")
  let vec_append = eval_program("(vector-append #(1) #(2 3))")
  inspect(@runtime.value_to_string(vec_append), content="#(1 2 3)")
  let vec_to_list = eval_program("(vector->list #(1 2 3) 1 3)")
  inspect(@runtime.value_to_string(vec_to_list), content="(2 3)")
  let list_to_vec = eval_program("(list->vector '(1 2))")
  inspect(@runtime.value_to_string(list_to_vec), content="#(1 2)")
}

///|
test "vector arity errors" {
  expect_error(() => eval_program("(vector?)"))
  expect_error(() => eval_program("(vector-length)"))
  expect_error(() => eval_program("(vector-ref #(1))"))
  expect_error(() => eval_program("(vector-set! #(1) 0)"))
  expect_error(() => eval_program("(vector-fill! #(1) 0 1 2 3)"))
  expect_error(() => eval_program("(vector-copy)"))
  expect_error(() => eval_program("(vector-copy! #(1) 0)"))
  expect_error(() => eval_program("(vector->list)"))
  expect_error(() => eval_program("(list->vector)"))
  expect_error(() => eval_program("(make-vector)"))
}

///|
test "symbol and predicate basics" {
  let preds = eval_program(
    "(list (symbol? 'a) (symbol? 1) (identifier? #'a) (identifier? 1) (syntax? #'a) (syntax? 'a) (boolean? #t) (boolean? 1) (number? 1) (number? 'a) (integer? 1) (integer? 1.2) (exact-integer? 1) (exact-integer? 1.2) (rational? 1/2) (rational? 1+2i) (real? 1.0) (real? 1+2i) (complex? 1.0) (complex? 1+2i))",
  )
  inspect(
    @runtime.value_to_string(preds),
    content="(#t #f #t #f #t #f #t #f #t #f #t #f #t #f #t #f #t #f #t #t)",
  )
  let non_datum = eval_program(
    "(let ((f (lambda (x) x))) (list (boolean? f) (number? f) (integer? f) (exact-integer? f) (rational? f) (real? f) (complex? f)))",
  )
  inspect(@runtime.value_to_string(non_datum), content="(#f #f #f #f #f #f #f)")
  let id_port = eval_program("(identifier? (current-output-port))")
  inspect(@runtime.value_to_string(id_port), content="#f")
  let hashes = eval_program(
    "(list (integer? (string-hash \"a\")) (integer? (string-ci-hash \"A\")) (integer? (symbol-hash 'a)) (integer? (equal-hash '(1 2))))",
  )
  inspect(@runtime.value_to_string(hashes), content="(#t #t #t #t)")
  let sym_text = eval_program("(symbol->string 'abc)")
  inspect(@runtime.value_to_string(sym_text), content="\"abc\"")
  let sym = eval_program("(string->symbol \"abc\")")
  inspect(@runtime.value_to_string(sym), content="abc")
  let syn_value = eval_program("(syntax->datum (datum->syntax #'x #'y))")
  inspect(@runtime.value_to_string(syn_value), content="y")
  let syn_datum = eval_program("(syntax->datum 'x)")
  inspect(@runtime.value_to_string(syn_datum), content="x")
}

///|
test "symbol and predicate arity errors" {
  expect_error(() => eval_program("(eq? 1)"))
  expect_error(() => eval_program("(eqv? 1)"))
  expect_error(() => eval_program("(equal? 1)"))
  expect_error(() => eval_program("(symbol?)"))
  expect_error(() => eval_program("(identifier?)"))
  expect_error(() => eval_program("(syntax?)"))
  expect_error(() => eval_program("(free-identifier=? #'a)"))
  expect_error(() => eval_program("(bound-identifier=? #'a)"))
  expect_error(() => eval_program("(symbol->string 1)"))
  expect_error(() => eval_program("(symbol->string)"))
  expect_error(() => eval_program("(symbol->string 'a 'b)"))
  expect_error(() => eval_program("(string->symbol 1)"))
  expect_error(() => eval_program("(string->symbol)"))
  expect_error(() => eval_program("(string->symbol \"a\" \"b\")"))
  expect_error(() => eval_program("(string-hash)"))
  expect_error(() => eval_program("(string-ci-hash)"))
  expect_error(() => eval_program("(symbol-hash)"))
  expect_error(() => eval_program("(equal-hash)"))
  expect_error(() => eval_program("(syntax->datum (lambda (x) x))"))
  expect_error(() => eval_program("(syntax->datum)"))
  expect_error(() => eval_program("(syntax->datum #'x #'y)"))
  expect_error(() => eval_program("(datum->syntax #'x (lambda (x) x))"))
  expect_error(() => eval_program("(datum->syntax #'x)"))
  expect_error(() => eval_program("(boolean?)"))
  expect_error(() => eval_program("(number?)"))
}

///|
test "identifier predicate type errors" {
  expect_error(() => eval_program("(bound-identifier=? 1 2)"))
  expect_error(() => eval_program("(bound-identifier=? (syntax 1) (syntax 2))"))
  expect_error(() => {
    eval_program("(bound-identifier=? (current-output-port) #'a)")
  })
}

///|
test "numeric predicate edges" {
  let exact_complex = eval_program(
    "(exact? (make-rectangular 1 1000000000000))",
  )
  inspect(@runtime.value_to_string(exact_complex), content="#t")
  let exact_complex_rat = eval_program("(exact? (make-rectangular 1 1/2))")
  inspect(@runtime.value_to_string(exact_complex_rat), content="#t")
  let exact_proc = eval_program("(exact? (lambda (x) x))")
  inspect(@runtime.value_to_string(exact_proc), content="#f")
  let inexact_complex = eval_program("(inexact? (make-rectangular 1 1.0))")
  inspect(@runtime.value_to_string(inexact_complex), content="#t")
  let inexact_exact = eval_program("(inexact? (make-rectangular 1 2))")
  inspect(@runtime.value_to_string(inexact_exact), content="#f")
  let zero_big = eval_program("(zero? 1000000000000)")
  inspect(@runtime.value_to_string(zero_big), content="#f")
  let zero_bigrat = eval_program("(zero? 1000000000000/3)")
  inspect(@runtime.value_to_string(zero_bigrat), content="#f")
  let zero_complex = eval_program("(zero? (make-rectangular 0 1))")
  inspect(@runtime.value_to_string(zero_complex), content="#f")
  let positive_big = eval_program("(positive? 1000000000000/3)")
  inspect(@runtime.value_to_string(positive_big), content="#t")
  let positive_float = eval_program("(positive? 1.0)")
  inspect(@runtime.value_to_string(positive_float), content="#t")
  let negative_int = eval_program("(negative? -1)")
  inspect(@runtime.value_to_string(negative_int), content="#t")
  let negative_rat = eval_program("(negative? -1/2)")
  inspect(@runtime.value_to_string(negative_rat), content="#t")
  let negative_big = eval_program("(negative? -1000000000000/3)")
  inspect(@runtime.value_to_string(negative_big), content="#t")
  let odd_complex = eval_program("(odd? (make-rectangular 3 0))")
  inspect(@runtime.value_to_string(odd_complex), content="#t")
  let even_complex = eval_program("(even? (make-rectangular 4 0))")
  inspect(@runtime.value_to_string(even_complex), content="#t")
  let finite_complex = eval_program("(finite? (make-rectangular 1 0))")
  inspect(@runtime.value_to_string(finite_complex), content="#t")
  let infinite_real = eval_program("(infinite? +inf.0)")
  inspect(@runtime.value_to_string(infinite_real), content="#t")
  let nan_real = eval_program("(nan? +nan.0)")
  inspect(@runtime.value_to_string(nan_real), content="#t")
  let proc_true = eval_program("(procedure? (lambda (x) x))")
  inspect(@runtime.value_to_string(proc_true), content="#t")
  let proc_false = eval_program("(procedure? 1)")
  inspect(@runtime.value_to_string(proc_false), content="#f")
}

///|
test "numeric comparison and division edges" {
  let comparisons = eval_program(
    "(list (= 1+2i 1+2i) (< 12345678901234567890 12345678901234567891) (> 12345678901234567891 12345678901234567890) (integer? (make-rectangular 2 0)) (exact-integer? (make-rectangular 2 0)) (rational? (make-rectangular 1/2 0)))",
  )
  inspect(@runtime.value_to_string(comparisons), content="(#t #t #t #t #t #t)")
  let float_ops = eval_program("(list (= (+ 1.0 2) 3.0) (= (* 1.0 2) 2.0))")
  inspect(@runtime.value_to_string(float_ops), content="(#t #t)")
  let misc = eval_program(
    "(list (infinite? +nan.0) (flonum? (ceiling 2.0)) (number->string 0 10))",
  )
  inspect(@runtime.value_to_string(misc), content="(#f #t \"0\")")
  expect_error(() => eval_program("(make-polar 1+2i 0)"))
  expect_error(() => eval_program("(/ 1 0)"))
  expect_error(() => eval_program("(/ 1.0 0.0)"))
  expect_error(() => eval_program("(/ 12345678901234567890 0)"))
}

///|
test "numeric predicate extra cases" {
  let value = eval_program(
    "(let ((c1 (string->number \"1+0i\")) (c2 (string->number \"2+0i\")) (cinex (string->number \"1+2.0i\"))) (list (exact? 'a) (exact? cinex) (odd? c1) (even? c2)))",
  )
  inspect(@runtime.value_to_string(value), content="(#f #f #t #t)")
  expect_error(() => eval_program("(odd? 1.0)"))
}

///|
test "numeric predicate errors" {
  expect_error(() => eval_program("(integer?)"))
  expect_error(() => eval_program("(exact-integer?)"))
  expect_error(() => eval_program("(rational?)"))
  expect_error(() => eval_program("(real?)"))
  expect_error(() => eval_program("(complex?)"))
  expect_error(() => eval_program("(exact?)"))
  expect_error(() => eval_program("(inexact?)"))
  expect_error(() => eval_program("(zero?)"))
  expect_error(() => eval_program("(positive?)"))
  expect_error(() => eval_program("(negative?)"))
  expect_error(() => eval_program("(odd?)"))
  expect_error(() => eval_program("(odd? 1.2)"))
  expect_error(() => eval_program("(odd? (make-rectangular 1 1))"))
  expect_error(() => eval_program("(odd? (make-rectangular 1.2 0))"))
  expect_error(() => eval_program("(even?)"))
  expect_error(() => eval_program("(even? 1.2)"))
  expect_error(() => eval_program("(even? (make-rectangular 1 1))"))
  expect_error(() => eval_program("(even? (make-rectangular 1.2 0))"))
  expect_error(() => eval_program("(finite?)"))
  expect_error(() => eval_program("(finite? (make-rectangular 1 1))"))
  expect_error(() => eval_program("(infinite?)"))
  expect_error(() => eval_program("(infinite? (make-rectangular 1 1))"))
  expect_error(() => eval_program("(infinite? 'a)"))
  expect_error(() => eval_program("(nan?)"))
  expect_error(() => eval_program("(nan? (make-rectangular 1 1))"))
  expect_error(() => eval_program("(nan? 'a)"))
  expect_error(() => eval_program("(procedure?)"))
}

///|
test "syntax-rules macro" {
  let program = "(begin (define-syntax add2 (syntax-rules () ((add2 x) (+ x 2)))) (add2 3))"
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="5")
}

///|
test "include source" {
  register_include_source("mem.scm", "(+ 1 2)")
  let value = eval_program("(include \"mem.scm\")")
  inspect(@runtime.value_to_string(value), content="3")
}

///|
test "include error paths" {
  expect_error(() => eval_program("(include 1)"))
  expect_error(() => eval_program("(include \"missing.scm\")"))
  register_include_source("bad.scm", "(+ 1")
  expect_error(() => eval_program("(include \"bad.scm\")"))
}

///|
test "primitive output helpers" {
  let value = eval_program(
    "(begin (write 1) (newline) (get-output-string (current-output-port)))",
  )
  inspect(@runtime.value_to_string(value), content="\"1\\n\"")
}

///|
test "display string char symbol" {
  let program =
    #|(let ((out (open-output-string)))
    #|  (display "hi " out)
    #|  (display #\a out)
    #|  (display " " out)
    #|  (display 'sym out)
    #|  (get-output-string out))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="\"hi a sym\"")
}

///|
test "primitive arity errors" {
  expect_error(() => eval_program("(display 1 2 3)"))
  expect_error(() => eval_program("(write)"))
  expect_error(() => eval_program("(newline 1 2)"))
  expect_error(() => eval_program("(open-output-string 1)"))
  expect_error(() => eval_program("(get-output-string)"))
  expect_error(() => eval_program("(current-output-port 1)"))
  expect_error(() => eval_program("(not)"))
}

///|
test "syntax helper errors" {
  expect_error(() => eval_program("(make-variable-transformer 1)"))
  expect_error(() => eval_program("(generate-temporaries '(1))"))
  expect_error(() => eval_program("(generate-temporaries (list #'(1)))"))
}

///|
test "syntax and identifier error cases" {
  let id_false = eval_program("(identifier? 1)")
  inspect(@runtime.value_to_string(id_false), content="#f")
  expect_error(() => eval_program("(syntax->datum (lambda (x) x))"))
  expect_error(() => eval_program("(datum->syntax #f (current-output-port))"))
}

///|
test "labelled expression evaluation" {
  let value = eval_program("#1=(+ 1 2)")
  inspect(@runtime.value_to_string(value), content="3")
}

///|
test "cond and case arrow" {
  let cond_value = eval_program(
    "(cond ((+ 1 2) => (lambda (x) (+ x 1))) (else 0))",
  )
  inspect(@runtime.value_to_string(cond_value), content="4")
  let case_value = eval_program(
    "(case 2 ((1) => (lambda (x) (+ x 10))) ((2) => (lambda (x) (+ x 1))) (else 0))",
  )
  inspect(@runtime.value_to_string(case_value), content="3")
}

///|
test "cond and case invalid arrow" {
  expect_error(() => {
    eval_program("(cond ((+ 1 2) => (lambda (x) x) 4) (else 0))")
  })
  expect_error(() => eval_program("(case 2 ((2) => (lambda (x) x) 1) (else 0))"))
}

///|
test "cond-expand error paths" {
  expect_error(() => eval_program("(cond-expand ())"))
  expect_error(() => eval_program("(cond-expand (else 1) (else 2))"))
  expect_error(() => eval_program("(cond-expand (foo 1) ())"))
}

///|
test "cond-expand feature coverage" {
  let and_case = eval_program(
    "(cond-expand ((and r6rs ieee-float) 1) (else 2))",
  )
  inspect(@runtime.value_to_string(and_case), content="1")
  let or_case = eval_program(
    "(cond-expand ((or exact-closed r6rs) 3) (else 4))",
  )
  inspect(@runtime.value_to_string(or_case), content="3")
  let not_case = eval_program("(cond-expand ((not exact-closed) 5) (else 6))")
  inspect(@runtime.value_to_string(not_case), content="5")
  let lib_case = eval_program(
    "(cond-expand ((library (rnrs base)) 7) (else 8))",
  )
  inspect(@runtime.value_to_string(lib_case), content="7")
  let and_empty = eval_program("(cond-expand ((and) 9) (else 0))")
  inspect(@runtime.value_to_string(and_empty), content="9")
  expect_error(() => eval_program("(cond-expand ((not) 1) (else 2))"))
}

///|
test "library version selection" {
  let program =
    #|(begin
    #|  (library (ver-select (1 0))
    #|    (export val)
    #|    (import)
    #|    (define val 10))
    #|  (library (ver-select (2 0))
    #|    (export val)
    #|    (import)
    #|    (define val 20))
    #|  (import (ver-select))
    #|  val)
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="20")
}

///|
test "library version refs or and not" {
  let program =
    #|(begin
    #|  (library (ver-or (1 0))
    #|    (export val)
    #|    (import)
    #|    (define val 1))
    #|  (library (ver-or (2 0))
    #|    (export val)
    #|    (import)
    #|    (define val 2))
    #|  (import (ver-or (or (>= (1 0)) (not (< (2 0))))))
    #|  val)
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="2")
}

///|
test "library version refs and export rename" {
  let program =
    #|(begin
    #|  (library (ver-and (1 0))
    #|    (export (rename (val ext-val)))
    #|    (import)
    #|    (define val 10))
    #|  (library (ver-and (2 0))
    #|    (export (rename (val ext-val)))
    #|    (import)
    #|    (define val 20))
    #|  (import (rename (ver-and (and (>= (1 0)) (< (2 0)))) (ext-val final-val)))
    #|  final-val)
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="10")
}

///|
test "guard invalid arrow" {
  expect_error(() => {
    eval_program(
      "(guard (ex ((begin ex) => (lambda (v) v) 1) (else 'no)) (raise 'ok))",
    )
  })
}

///|
test "parameterize multi bindings" {
  let value = eval_program(
    "(begin (define p1 (make-parameter 1 (lambda (x) (+ x 1)))) (define p2 (make-parameter 10)) (list (p1) (p2) (parameterize ((p1 4) (p2 20)) (list (p1) (p2))) (p1) (p2)))",
  )
  inspect(@runtime.value_to_string(value), content="(2 10 (5 20) 2 10)")
}

///|
test "parameterize non-parameter error" {
  expect_error(() => eval_program("(parameterize ((1 2)) 3)"))
}

///|
test "parameter arity error" {
  expect_error(() => {
    eval_program("(begin (define p (make-parameter 1)) (p 1 2))")
  })
}

///|
test "eval environment error" {
  expect_error(() => eval_program("(eval '(+ 1 2) 1)"))
}

///|
test "record protocol returns non-procedure" {
  expect_error(() => {
    eval_program(
      "(begin (define-record-type foo (make-foo a) foo? (a foo-a) (protocol (lambda (p) 1))) (make-foo 1))",
    )
  })
}

///|
test "record descriptor mismatch errors" {
  let sealed_program =
    #|(begin
    #|  (make-record-type-descriptor 'r1 #f 'u #f #f '#((immutable a)))
    #|  (make-record-type-descriptor 'r2 #f 'u #t #f '#((immutable a))))
  expect_error(() => eval_program(sealed_program))
  let parent_program =
    #|(begin
    #|  (define p1 (make-record-type-descriptor 'p1 #f #f #f #f '#()))
    #|  (define p2 (make-record-type-descriptor 'p2 #f #f #f #f '#()))
    #|  (make-record-type-descriptor 'r1 p1 'u #f #f '#((immutable a)))
    #|  (make-record-type-descriptor 'r2 p2 'u #f #f '#((immutable a))))
  expect_error(() => eval_program(parent_program))
  let opaque_program =
    #|(begin
    #|  (define p1 (make-record-type-descriptor 'p1 #f #f #f #f '#()))
    #|  (make-record-type-descriptor 'r1 p1 'u #f #f '#((immutable a)))
    #|  (make-record-type-descriptor 'r2 p1 'u #f #t '#((immutable a))))
  expect_error(() => eval_program(opaque_program))
  let fields_len_program =
    #|(begin
    #|  (make-record-type-descriptor 'r1 #f 'u #f #f '#((immutable a)))
    #|  (make-record-type-descriptor 'r2 #f 'u #f #f '#()))
  expect_error(() => eval_program(fields_len_program))
  let fields_mut_program =
    #|(begin
    #|  (make-record-type-descriptor 'r1 #f 'u #f #f '#((immutable a)))
    #|  (make-record-type-descriptor 'r2 #f 'u #f #f '#((mutable a))))
  expect_error(() => eval_program(fields_mut_program))
  let parent_missing_program =
    #|(begin
    #|  (define p1 (make-record-type-descriptor 'p1 #f #f #f #f '#()))
    #|  (make-record-type-descriptor 'r1 p1 'u #f #f '#((immutable a)))
    #|  (make-record-type-descriptor 'r2 #f 'u #f #f '#((immutable a))))
  expect_error(() => eval_program(parent_missing_program))
}

///|
test "record datum conversion helpers" {
  expect_error(() => {
    eval_program(
      "(begin (define-record-type rec (make-rec x) rec? (x rec-x)) (define r (make-rec 1)) (list->string r))",
    )
  })
  let record_keys = eval_program(
    "(begin (define-record-type rec (make-rec x) rec? (x rec-x)) (define r (make-rec 1)) (define ht (make-eq-hashtable)) (hashtable-set! ht r 1) (vector-length (hashtable-keys ht)))",
  )
  inspect(@runtime.value_to_string(record_keys), content="1")
}

///|
test "condition component type error" {
  expect_error(() => {
    eval_program(
      "(begin (define-record-type foo (make-foo a) foo? (a foo-a)) (simple-conditions (make-foo 1)))",
    )
  })
  expect_error(() => eval_program("(simple-conditions 1)"))
}

///|
test "hashtable custom equivalence scanning" {
  let value = eval_program(
    "(begin (define ht (make-hashtable (lambda (x) 0) (lambda (a b) (eq? a b)))) (hashtable-set! ht 'a 1) (hashtable-set! ht 'b 2) (hashtable-ref ht 'b 0))",
  )
  inspect(@runtime.value_to_string(value), content="2")
}

///|
test "hashtable primitives" {
  let program =
    #|(begin
    #|  (define ht (make-hashtable (lambda (x) 0) (lambda (a b) (eq? a b))))
    #|  (hashtable-set! ht 'a 1)
    #|  (hashtable-set! ht 'b 2)
    #|  (define hit (hashtable-ref ht 'a 0))
    #|  (define miss (hashtable-ref ht 'c 9))
    #|  (define contains (hashtable-contains? ht 'b))
    #|  (hashtable-update! ht 'b (lambda (x) (+ x 3)) 0)
    #|  (define upd (hashtable-ref ht 'b 0))
    #|  (define size (hashtable-size ht))
    #|  (define eqf (hashtable-equivalence-function ht))
    #|  (define hashf (hashtable-hash-function ht))
    #|  (define mut (hashtable-mutable? ht))
    #|  (define copy (hashtable-copy ht #f))
    #|  (define cleared (begin (hashtable-clear! ht) (hashtable-size ht)))
    #|  (let-values (((keys vals) (hashtable-entries copy)))
    #|    (list hit miss contains upd size mut (procedure? eqf) (procedure? hashf)
    #|          (vector-length (hashtable-keys copy))
    #|          (vector-length keys)
    #|          (vector-length vals)
    #|          cleared)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(1 9 #t 5 2 #t #t #t 2 2 2 0)",
  )
}

///|
test "hashtable error paths" {
  expect_error(() => {
    eval_program(
      "(begin (define ht (make-eq-hashtable)) (define cp (hashtable-copy ht #f)) (hashtable-clear! cp))",
    )
  })
  expect_error(() => {
    eval_program(
      "(begin (define ht (make-eq-hashtable)) (define cp (hashtable-copy ht #f)) (hashtable-clear! cp 1))",
    )
  })
  expect_error(() => {
    eval_program(
      "(begin (define ht (make-eq-hashtable)) (hashtable-update! ht 'a 1 0))",
    )
  })
}

///|
test "enum set primitives" {
  let program =
    #|(begin
    #|  (define colors (make-enumeration '(red green blue)))
    #|  (define ctor (enum-set-constructor colors))
    #|  (define idx (enum-set-indexer colors))
    #|  (define s1 (ctor '(red blue)))
    #|  (define s2 (ctor '(green)))
    #|  (define union (enum-set-union s1 s2))
    #|  (define inter (enum-set-intersection s1 s2))
    #|  (define diff (enum-set-difference s1 s2))
    #|  (define comp (enum-set-complement s1))
    #|  (define proj (enum-set-projection s1 colors))
    #|  (list (enum-set? s1)
    #|        (enum-set? 1)
    #|        (enum-set-member? 'red s1)
    #|        (enum-set-subset? s2 union)
    #|        (enum-set=? s1 s1)
    #|        (idx 'blue)
    #|        (enum-set->list union)
    #|        (enum-set->list inter)
    #|        (enum-set->list diff)
    #|        (enum-set->list comp)
    #|        (enum-set->list proj)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(#t #f #t #t #t 2 (red green blue) () (red blue) (green) (red blue))",
  )
}

///|
test "record primitives" {
  let program =
    #|(begin
    #|  (define rtd (make-record-type-descriptor 'pt #f 'uid #f #f '#((mutable x) (immutable y))))
    #|  (define rcd (make-record-constructor-descriptor rtd #f #f))
    #|  (define make-pt (record-constructor rcd))
    #|  (define pred (record-predicate rtd))
    #|  (define acc-x (record-accessor rtd 0))
    #|  (define acc-y (record-accessor rtd 1))
    #|  (define mut-x (record-mutator rtd 0))
    #|  (define pt (make-pt 1 2))
    #|  (mut-x pt 7)
    #|  (define names (record-type-field-names rtd))
    #|  (define rcd2 (record-constructor-descriptor rtd))
    #|  (list (record? pt)
    #|        (pred pt)
    #|        (record-type-descriptor? rtd)
    #|        (record-constructor-descriptor? rcd)
    #|        (record-constructor-descriptor? rcd2)
    #|        (record-type-generative? rtd)
    #|        (record-type-sealed? rtd)
    #|        (record-type-opaque? rtd)
    #|        (record-type-field-mutable? rtd 0)
    #|        (record-type-field-mutable? rtd 1)
    #|        (eq? (record-type-parent rtd) #f)
    #|        (symbol? (record-type-name rtd))
    #|        (symbol? (record-type-uid rtd))
    #|        (record-type-descriptor? (record-rtd pt))
    #|        (acc-x pt)
    #|        (acc-y pt)
    #|        (length names)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(#t #t #t #t #t #f #f #f #t #f #t #t #t #t 7 2 2)",
  )
}

///|
test "condition primitives" {
  let program =
    #|(begin
    #|  (define-condition-type &c &condition make-c c? (x c-x))
    #|  (define c1 (make-c 1))
    #|  (define c2 (make-c 2))
    #|  (define single (condition c1))
    #|  (define multi (condition c1 c2))
    #|  (define pred (condition-predicate (record-rtd c1)))
    #|  (define acc
    #|    (condition-accessor
    #|      (record-rtd c1)
    #|      (record-accessor (record-rtd c1) 0)))
    #|  (list (condition? single)
    #|        (condition? multi)
    #|        (pred single)
    #|        (pred multi)
    #|        (acc single)
    #|        (length (simple-conditions single))
    #|        (length (simple-conditions multi))))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t 1 1 2)")
}

///|
test "fixnum primitives" {
  let program =
    #|(begin
    #|  (define results
    #|    (list
    #|      (fixnum? 1)
    #|      (> (fixnum-width) 0)
    #|      (fx<=? (least-fixnum) (greatest-fixnum))
    #|      (fx=? (fx+ 1 2 3) 6)
    #|      (fx=? (fx- 10 3) 7)
    #|      (fx=? (fx* 2 3) 6)
    #|      (fx=? (fxdiv 7 2) 3)
    #|      (fx=? (fxmod 7 2) 1)
    #|      (fx=? (fxdiv0 7 2) 3)
    #|      (fx=? (fxmod0 7 2) 1)
    #|      (fx=? (fxand 6 3) 2)
    #|      (fx=? (fxior 6 3) 7)
    #|      (fx=? (fxxor 6 3) 5)
    #|      (fx=? (fxif 6 3 1) 3)
    #|      (fx=? (fxbit-count 7) 3)
    #|      (fx=? (fxlength 8) 4)
    #|      (fx=? (fxfirst-bit-set 8) 3)
    #|      (fxbit-set? 2 1)
    #|      (fx=? (fxcopy-bit 2 0 1) 3)
    #|      (fx=? (fxbit-field 6 1 3) 3)
    #|      (fx=? (fxcopy-bit-field 15 1 3 0) 9)
    #|      (fx=? (fxrotate-bit-field 14 0 3 1) 13)
    #|      (fx=? (fxreverse-bit-field 14 0 3) 11)
    #|      (fx=? (fxarithmetic-shift 1 3) 8)
    #|      (fx=? (fxarithmetic-shift-left 1 2) 4)
    #|      (fx=? (fxarithmetic-shift-right 8 2) 2)
    #|      (fx=? (fxmin 3 1 2) 1)
    #|      (fx=? (fxmax 3 1 2) 3)
    #|      (equal? (call-with-values (lambda () (fx+/carry 1 2 0)) list) '(3 0))
    #|      (equal? (call-with-values (lambda () (fx-/carry 5 3 0)) list) '(2 0))
    #|      (equal? (call-with-values (lambda () (fx*/carry 2 3 0)) list) '(6 0))))
    #|  (not (memv #f results)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "fixnum predicate edges" {
  let program =
    #|(list (fixnum? 1)
    #|      (fixnum? 10000000000000000000001)
    #|      (fixnum? (make-rectangular 1 0))
    #|      (fixnum? (make-rectangular 1 2)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #f #t #f)")
}

///|
test "flonum primitives" {
  let program =
    #|(begin
    #|  (define num (flnumerator 2.5))
    #|  (define den (fldenominator 2.5))
    #|  (define inf (fl/ 1.0 0.0))
    #|  (define nan (fl/ 0.0 0.0))
    #|  (define from-fx (fixnum->flonum 3))
    #|  (define from-real (real->flonum 3))
    #|  (define divmod-ok
    #|    (call-with-values
    #|      (lambda () (fldiv-and-mod 7.0 2.0))
    #|      (lambda (q r) (and (fl=? q 3.0) (fl=? r 1.0)))))
    #|  (define div0mod0-ok
    #|    (call-with-values
    #|      (lambda () (fldiv0-and-mod0 7.0 2.0))
    #|      (lambda (q r) (and (fl=? q 3.0) (fl=? r 1.0)))))
    #|  (define results
    #|    (list
    #|      (flonum? 1.0)
    #|      (fl=? 1.0 1.0)
    #|      (fl<? 1.0 2.0)
    #|      (fl>? 2.0 1.0)
    #|      (fl<=? 1.0 1.0)
    #|      (fl>=? 2.0 2.0)
    #|      (flinteger? 2.0)
    #|      (flzero? 0.0)
    #|      (flpositive? 1.0)
    #|      (flnegative? -1.0)
    #|      (flodd? 3.0)
    #|      (fleven? 4.0)
    #|      (flfinite? 1.0)
    #|      (flinfinite? inf)
    #|      (flnan? nan)
    #|      (fl=? (flmax 1.0 2.0) 2.0)
    #|      (fl=? (flmin 1.0 2.0) 1.0)
    #|      (fl=? (fl+ 1.0 2.0) 3.0)
    #|      (fl=? (fl* 2.0 3.0) 6.0)
    #|      (fl=? (fl- 5.0 2.0) 3.0)
    #|      (fl=? (fl/ 6.0 2.0) 3.0)
    #|      (fl=? (flabs -3.0) 3.0)
    #|      (fl=? (flfloor 2.9) 2.0)
    #|      (fl=? (flceiling 2.1) 3.0)
    #|      (fl=? (fltruncate -2.9) -2.0)
    #|      (fl=? (flround 2.4) 2.0)
    #|      (fl=? (flexp 0.0) 1.0)
    #|      (fl=? (fllog 1.0) 0.0)
    #|      (fl=? (flsin 0.0) 0.0)
    #|      (fl=? (flcos 0.0) 1.0)
    #|      (fl=? (fltan 0.0) 0.0)
    #|      (fl=? (flasin 0.0) 0.0)
    #|      (fl=? (flacos 1.0) 0.0)
    #|      (fl=? (flatan 0.0) 0.0)
    #|      (fl=? (flatan 0.0 1.0) 0.0)
    #|      (fl=? num 5.0)
    #|      (fl=? den 2.0)
    #|      (fl=? (flsqrt 4.0) 2.0)
    #|      (fl=? (flexpt 2.0 3.0) 8.0)
    #|      (fl=? from-fx 3.0)
    #|      (fl=? from-real 3.0)
    #|      divmod-ok
    #|      div0mod0-ok))
    #|  (not (memv #f results)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "flonum numerator denominator special cases" {
  let program =
    #|(let* ((inf (fl/ 1.0 0.0))
    #|       (nan (fl/ 0.0 0.0))
    #|       (num-inf (flnumerator inf))
    #|       (num-nan (flnumerator nan))
    #|       (den-inf (fldenominator inf))
    #|       (den-zero (fldenominator 0.0))
    #|       (neg-inf (fl/ 1.0 (flnumerator -0.0))))
    #|  (list (flinfinite? num-inf)
    #|        (flnan? num-nan)
    #|        (fl=? den-inf 1.0)
    #|        (fl=? den-zero 1.0)
    #|        (flnegative? neg-inf)
    #|        (flinfinite? neg-inf)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t #t #t)")
}

///|
test "numeric primitives" {
  let program =
    #|(begin
    #|  (define big (string->number "12345678901234567890"))
    #|  (define bigrat (string->number "12345678901234567890/12345678901234567891"))
    #|  (define complex (make-rectangular 1 2))
    #|  (define polar (make-polar 2 0))
    #|  (define inf (fl/ 1.0 0.0))
    #|  (define nan (fl/ 0.0 0.0))
    #|  (define sqrt-pair (call-with-values (lambda () (exact-integer-sqrt 10)) list))
    #|  (define results
    #|    (list
    #|      (number? 1)
    #|      (integer? 1)
    #|      (exact-integer? 1)
    #|      (rational? 1/2)
    #|      (real? 1)
    #|      (complex? complex)
    #|      (exact? 1/2)
    #|      (inexact? (exact->inexact 1))
    #|      (zero? 0)
    #|      (positive? 1)
    #|      (negative? -1)
    #|      (odd? 3)
    #|      (even? 4)
    #|      (finite? 1.0)
    #|      (infinite? inf)
    #|      (nan? nan)
    #|      (number? big)
    #|      (rational? bigrat)
    #|      (= (+ 1 2 3) 6)
    #|      (= (- 10 3) 7)
    #|      (= (* 2 3 4) 24)
    #|      (= (/ 8 2) 4)
    #|      (= (abs -3) 3)
    #|      (= (max 1 4 2) 4)
    #|      (= (min 1 4 2) 1)
    #|      (= (gcd 12 8) 4)
    #|      (= (lcm 6 4) 12)
    #|      (= (quotient 7 2) 3)
    #|      (= (remainder 7 2) 1)
    #|      (= (modulo -7 3) 2)
    #|      (= (numerator 6/8) 3)
    #|      (= (denominator 6/8) 4)
    #|      (equal? sqrt-pair '(3 1))
    #|      (rational? (rationalize 1.25 1/10))
    #|      (= (real-part complex) 1)
    #|      (= (imag-part complex) 2)
    #|      (real? (magnitude complex))
    #|      (= (angle polar) 0)
    #|      (= (real-part polar) 2)
    #|      (= (imag-part polar) 0)
    #|      (= (sqrt 4) 2)
    #|      (= (exp 0) 1)
    #|      (= (log 1) 0)
    #|      (= (expt 2 3) 8)
    #|      (= (sin 0) 0)
    #|      (= (cos 0) 1)
    #|      (= (tan 0) 0)
    #|      (= (asin 0) 0)
    #|      (= (acos 1) 0)
    #|      (= (atan 0) 0)
    #|      (= (bitwise-and 6 3) 2)
    #|      (= (bitwise-ior 6 3) 7)
    #|      (= (bitwise-xor 6 3) 5)
    #|      (= (bitwise-not 0) -1)
    #|      (= (bitwise-if 6 3 1) 3)
    #|      (= (arithmetic-shift 1 3) 8)
    #|      (= (bitwise-bit-count 7) 3)
    #|      (= (bitwise-length 8) 4)
    #|      (= (bitwise-first-bit-set 8) 3)
    #|      (bitwise-bit-set? 2 1)
    #|      (= (bitwise-copy-bit 2 0 1) 3)
    #|      (= (bitwise-bit-field 6 1 3) 3)
    #|      (= (bitwise-copy-bit-field 15 1 3 0) 9)
    #|      (= (bitwise-rotate-bit-field 14 0 3 1) 13)
    #|      (= (bitwise-reverse-bit-field 14 0 3) 11)
    #|      (equal? (number->string 10 2) "1010")
    #|      (= (string->number "ff" 16) 255)
    #|      (= (inexact->exact 1.5) 3/2)
    #|      (= (exact->inexact 2) 2.0)))
    #|  (not (memv #f results)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "numerator denominator and lcm modulo big" {
  let program =
    #|(let* ((big 10000000000000000000001)
    #|       (bigrat (/ big 3)))
    #|  (list (numerator big)
    #|        (denominator big)
    #|        (numerator bigrat)
    #|        (denominator bigrat)
    #|        (lcm big 6)
    #|        (modulo big 7)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(10000000000000000000001 1 10000000000000000000001 3 60000000000000000000006 5)",
  )
}

///|
test "numeric edge cases" {
  let eq_zero = eval_program("(=)")
  inspect(@runtime.value_to_string(eq_zero), content="#t")
  let lt_zero = eval_program("(<)")
  inspect(@runtime.value_to_string(lt_zero), content="#t")
  let gt_single = eval_program("(> 1)")
  inspect(@runtime.value_to_string(gt_single), content="#t")
  let le_single = eval_program("(<= 1)")
  inspect(@runtime.value_to_string(le_single), content="#t")
  let ge_single = eval_program("(>= 1)")
  inspect(@runtime.value_to_string(ge_single), content="#t")
  expect_error(() => eval_program("(-)"))
  expect_error(() => eval_program("(/)"))
  expect_error(() => eval_program("(exact-integer-sqrt -1)"))
  expect_error(() => eval_program("(number->string 10 1)"))
  expect_error(() => eval_program("(number->string 1 10 2)"))
  expect_error(() => eval_program("(string->number \"10\" 1)"))
  expect_error(() => eval_program("(string->number \"10\" 10 2)"))
  expect_error(() => eval_program("(make-rectangular 1)"))
  expect_error(() => eval_program("(make-polar 1)"))
  expect_error(() => eval_program("(real-part)"))
  expect_error(() => eval_program("(imag-part 1 2)"))
  expect_error(() => eval_program("(magnitude)"))
  expect_error(() => eval_program("(angle)"))
  expect_error(() => eval_program("(number->string (make-rectangular 1 2) 16)"))
  expect_error(() => eval_program("(inexact->exact 'a)"))
  expect_error(() => eval_program("(exact->inexact)"))
  expect_error(() => eval_program("(inexact->exact)"))
  let big_num = eval_program("(number->string 12345678901234567890 10)")
  inspect(@runtime.value_to_string(big_num), content="\"12345678901234567890\"")
  let exact_float = eval_program("(inexact? (exact->inexact 1.0))")
  inspect(@runtime.value_to_string(exact_float), content="#t")
  let exact_complex = eval_program(
    "(complex? (exact->inexact (make-rectangular 1 2)))",
  )
  inspect(@runtime.value_to_string(exact_complex), content="#t")
  let inexact_int = eval_program("(integer? (inexact->exact 1))")
  inspect(@runtime.value_to_string(inexact_int), content="#t")
  let inexact_big = eval_program(
    "(integer? (inexact->exact 12345678901234567890))",
  )
  inspect(@runtime.value_to_string(inexact_big), content="#t")
  let inexact_rat = eval_program("(rational? (inexact->exact 1/2))")
  inspect(@runtime.value_to_string(inexact_rat), content="#t")
  let inexact_bigrat = eval_program(
    "(rational? (inexact->exact 12345678901234567890/12345678901234567891))",
  )
  inspect(@runtime.value_to_string(inexact_bigrat), content="#t")
}

///|
test "expt and number->string branches" {
  let program =
    #|(let* ((big 100000000000000000000)
    #|       (big-exp 100000000000000000000)
    #|       (odd-exp (+ big-exp 1))
    #|       (neg-exp (- big-exp))
    #|       (radix-ok (number? (string->number (number->string big 16) 16))))
    #|  (list (expt 2 -3)
    #|        (expt big -1)
    #|        (expt 2/3 -2)
    #|        (complex? (expt -2.0 0.5))
    #|        (complex? (expt (make-rectangular 1 1) 2))
    #|        (expt 1 big-exp)
    #|        (expt 1 neg-exp)
    #|        (expt -1 odd-exp)
    #|        (fl=? (expt 2.0 3.0) 8.0)
    #|        (fl=? (expt 4 1/2) 2.0)
    #|        (number->string big)
    #|        (number->string 255 16)
    #|        radix-ok
    #|        (number->string 1+2i)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(1/8 1/100000000000000000000 9/4 #t #t 1 1 -1 #t #t \"100000000000000000000\" \"ff\" #f \"1+2i\")",
  )
}

///|
test "expt branch coverage extra" {
  let program =
    #|(let* ((big 10000000000000000000001)
    #|       (bigrat (/ big 3))
    #|       (big-exp 100000000000000000000)
    #|       (odd-exp (+ big-exp 1)))
    #|  (list (rational? (expt bigrat 2))
    #|        (rational? (expt bigrat -1))
    #|        (flonum? (expt -2.0 3))
    #|        (fl=? (expt 4.0 0.5) 2.0)
    #|        (complex? (expt (make-rectangular 1 1) 0.5))
    #|        (complex? (expt -4 1/2))
    #|        (complex? (expt 2 1+2i))
    #|        (fl=? (expt -1.0 odd-exp) -1.0)
    #|        (complex? (expt (make-rectangular 1 1) big-exp))))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(#t #t #t #t #t #t #t #t #t)",
  )
}

///|
test "inexact->exact float branches" {
  let program =
    #|(list (= (inexact->exact 0.0) 0)
    #|      (= (inexact->exact -0.0) 0)
    #|      (= (inexact->exact 1.0) 1)
    #|      (= (inexact->exact 1.5) 3/2))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t)")
  expect_error(() => eval_program("(inexact->exact +inf.0)"))
  expect_error(() => eval_program("(inexact->exact +nan.0)"))
}

///|
test "syntax and quasiquote errors" {
  expect_error(() => eval_program("(quote 1 2)"))
  expect_error(() => eval_program("(syntax 1 2)"))
  expect_error(() => eval_program("(quasiquote 1 2)"))
  expect_error(() => eval_program("(quasisyntax 1 2)"))
  expect_error(() => eval_program("(quasiquote (unquote-splicing 1))"))
  expect_error(() => eval_program("(quasisyntax (unsyntax-splicing 1))"))
  expect_error(() => eval_program("(syntax-case 1 ())"))
  expect_error(() => eval_program("(delay 1 2)"))
}

///|
test "quasiquote nesting" {
  let value = eval_program("(quasiquote (quasiquote #(1 2)))")
  inspect(@runtime.value_to_string(value), content="(quasiquote #(1 2))")
}

///|
test "quasisyntax nesting and splicing" {
  let nested = eval_program(
    "(syntax->datum (quasisyntax (quasisyntax #(1 2))))",
  )
  inspect(@runtime.value_to_string(nested), content="(quasisyntax #(1 2))")
  let splice = eval_program(
    "(syntax->datum (quasisyntax ((unsyntax-splicing (syntax (1 2))) 3)))",
  )
  inspect(@runtime.value_to_string(splice), content="(1 2 3)")
}

///|
test "lambda and case-lambda errors" {
  expect_error(() => eval_program("(lambda)"))
  expect_error(() => eval_program("((lambda (1) 2) 3)"))
  expect_error(() => eval_program("(case-lambda)"))
  expect_error(() => eval_program("(case-lambda (1))"))
  expect_error(() => eval_program("((case-lambda ((x) x) ((x y) y)) 1 2 3)"))
  expect_error(() => eval_program("((case-lambda ((x y) x)) 1)"))
  expect_error(() => eval_program("((lambda (x . rest) x))"))
}

///|
test "binding and parameterize errors" {
  expect_error(() => eval_program("(begin (define 1 2) 3)"))
  expect_error(() => eval_program("(define-syntax x)"))
  expect_error(() => eval_program("(let (1) 2)"))
  expect_error(() => eval_program("(let-values (1) 2)"))
  expect_error(() => eval_program("(parameterize (1) 2)"))
  expect_error(() => eval_program("(parameterize ((1 2)) 3)"))
  expect_error(() => eval_program("(let-syntax ((x 1)) x)"))
  expect_error(() => {
    eval_program(
      "(define-record-type point (make-point x) point? (x point-x) (sealed 1))",
    )
  })
}

///|
test "import set error cases" {
  expect_error(() => eval_program("(import (only (rnrs base)))"))
  expect_error(() => eval_program("(import (except (rnrs base)))"))
  expect_error(() => eval_program("(import (rename (rnrs base)))"))
  expect_error(() => eval_program("(import (prefix (rnrs base)))"))
  expect_error(() => eval_program("(import (for (rnrs base)))"))
  expect_error(() => eval_program("(import (for (rnrs base) (meta -1)))"))
  expect_error(() => eval_program("(import (for (rnrs base) (meta a)))"))
  expect_error(() => eval_program("(import (for (rnrs base) 1))"))
  expect_error(() => eval_program("(import (rename (rnrs base) (nope nope2)))"))
  expect_error(() => eval_program("(import (rename (rnrs base) nope))"))
  let ok_meta = eval_program("(begin (import (for (rnrs base) (meta 0))) #t)")
  inspect(@runtime.value_to_string(ok_meta), content="#t")
}

///|
test "exception primitives" {
  expect_error(() => eval_program("(undefined-violation 'who \"msg\")"))
  expect_error(() => eval_program("(syntax-violation 'who \"msg\" 'form)"))
  expect_error(() => {
    eval_program("(syntax-violation 'who \"msg\" 'form 'subform 1)")
  })
  expect_error(() => eval_program("(error 'who \"msg\" 'irritant)"))
  expect_error(() => {
    eval_program("(assertion-violation 'who \"msg\" 'irritant)")
  })
  expect_error(() => {
    eval_program(
      "(implementation-restriction-violation 'who \"msg\" 'irritant)",
    )
  })
  expect_error(() => {
    eval_program(
      "(with-exception-handler (lambda (c) 0) (lambda () (raise 'boom)))",
    )
  })
  let cont = eval_program(
    "(with-exception-handler (lambda (c) 5) (lambda () (raise-continuable 'boom)))",
  )
  inspect(@runtime.value_to_string(cont), content="5")
  expect_error(() => eval_program("(raise 'boom)"))
  expect_error(() => eval_program("(raise-continuable 'boom)"))
  expect_error(() => eval_program("(raise)"))
}

///|
test "primitive arity mismatch extras" {
  expect_error(() => eval_program("(call-with-values (lambda () 1))"))
  expect_error(() => eval_program("(record-constructor)"))
  expect_error(() => eval_program("(hashtable-ref)"))
  expect_error(() => eval_program("(hashtable-set!)"))
  expect_error(() => eval_program("(make-promise)"))
  expect_error(() => eval_program("(force 1)"))
  expect_error(() => eval_program("(with-exception-handler (lambda (e) e))"))
  expect_error(() => eval_program("(dynamic-wind (lambda () 1) (lambda () 2))"))
  expect_error(() => eval_program("(eval '1)"))
  expect_error(() => eval_program("(error 'who)"))
  expect_error(() => eval_program("(assertion-violation 'who)"))
  expect_error(() => eval_program("(implementation-restriction-violation 'who)"))
  expect_error(() => eval_program("(undefined-violation 'who)"))
  expect_error(() => eval_program("(syntax-violation 'who \"msg\")"))
}

///|
test "map, apply, and length errors" {
  expect_error(() => eval_program("(map)"))
  expect_error(() => eval_program("(map (lambda (x) x))"))
  expect_error(() => eval_program("(for-each)"))
  expect_error(() => eval_program("(vector-map)"))
  expect_error(() => eval_program("(vector-for-each)"))
  expect_error(() => eval_program("(string-map)"))
  expect_error(() => eval_program("(string-for-each)"))
  expect_error(() => eval_program("(apply +)"))
  expect_error(() => eval_program("(apply + 1)"))
  expect_error(() => eval_program("((+ 1 2) 3)"))
  expect_error(() => eval_program("(vector-map (lambda (a b) a) #(1 2) #(3))"))
  expect_error(() => {
    eval_program("(vector-for-each (lambda (a b) #t) #(1 2) #(3))")
  })
  expect_error(() => eval_program("(string-map (lambda (a b) a) \"ab\" \"c\")"))
  expect_error(() => {
    eval_program("(string-for-each (lambda (a b) #t) \"ab\" \"c\")")
  })
  let str_map = eval_program("(string-map (lambda (a b) a) \"ab\" \"cd\")")
  inspect(@runtime.value_to_string(str_map), content="\"ab\"")
  let str_for = eval_program("(string-for-each (lambda (a b) a) \"ab\" \"cd\")")
  inspect(@runtime.value_to_string(str_for), content="#<void>")
  let sum = eval_program(
    "(begin (define xs '(1 2)) (define ys '(3 4)) (define acc 0) (for-each (lambda (a b) (set! acc (+ acc a b))) xs ys) acc)",
  )
  inspect(@runtime.value_to_string(sum), content="10")
}

///|
test "assoc error cases" {
  let ok = eval_program("(assoc 'a '((a . 1) (b . 2)))")
  inspect(@runtime.value_to_string(ok), content="(a . 1)")
  expect_error(() => eval_program("(assoc 'a '(1 2))"))
  expect_error(() => eval_program("(assoc 'z '((a . 1) . 2))"))
}

///|
test "parameters and continuations" {
  let value = eval_program("(begin (define p (make-parameter 1)) (p 3) (p))")
  inspect(@runtime.value_to_string(value), content="3")
  let cont = eval_program(
    "(call-with-values (lambda () (call/cc (lambda (k) (k 1 2)))) list)",
  )
  inspect(@runtime.value_to_string(cont), content="(1 2)")
  expect_error(() => eval_program("(call/cc)"))
}

///|
test "hashtable immutable update" {
  expect_error(() => {
    eval_program(
      "(begin (define ht (make-eq-hashtable)) (define cp (hashtable-copy ht #f)) (hashtable-update! cp 'a (lambda (x) x) 0))",
    )
  })
}

///|
test "syntax-case expression and values errors" {
  let value = eval_program(
    "(begin (import (rnrs syntax-case)) (syntax-case 'x () ((x) 1) (_ 2)))",
  )
  inspect(@runtime.value_to_string(value), content="2")
  expect_error(() => eval_program("(if (values #t #f) 1 2)"))
}

///|
test "parameterize multiple bindings" {
  let value = eval_program(
    "(begin (define p1 (make-parameter 1)) (define p2 (make-parameter 2)) (parameterize ((p1 10) (p2 20)) (+ (p1) (p2))))",
  )
  inspect(@runtime.value_to_string(value), content="30")
}

///|
test "nested unquote and unsyntax" {
  let value = eval_program(
    "(quasiquote (quasiquote (unquote-splicing (list 1 2))))",
  )
  inspect(
    @runtime.value_to_string(value),
    content="(quasiquote (unquote-splicing (list 1 2)))",
  )
  let nested = eval_program(
    "(syntax->datum (quasisyntax (quasisyntax (unsyntax 1))))",
  )
  inspect(
    @runtime.value_to_string(nested),
    content="(quasisyntax (unsyntax 1))",
  )
}

///|
test "numeric extra branches" {
  let sqrt_big = eval_program(
    "(call-with-values (lambda () (exact-integer-sqrt 100000000000000000000)) list)",
  )
  inspect(@runtime.value_to_string(sqrt_big), content="(10000000000 0)")
  let sqrt_big_rem = eval_program(
    "(call-with-values (lambda () (exact-integer-sqrt 100000000000000000001)) list)",
  )
  inspect(@runtime.value_to_string(sqrt_big_rem), content="(10000000000 1)")
  expect_error(() => eval_program("(exact-integer-sqrt -100000000000000000000)"))
  expect_error(() => eval_program("(exact-integer-sqrt 1.0)"))
  expect_error(() => eval_program("(exact-integer-sqrt)"))
  let rat_int = eval_program("(rationalize 2 0.1)")
  inspect(@runtime.value_to_string(rat_int), content="2")
  let rat_big = eval_program("(rationalize 100000000000000000000 0.1)")
  inspect(@runtime.value_to_string(rat_big), content="100000000000000000000")
  let rat_float = eval_program("(rationalize 0.25 0.1)")
  inspect(@runtime.value_to_string(rat_float), content="1/4")
  expect_error(() => eval_program("(rationalize 1 -1.0)"))
  expect_error(() => eval_program("(rationalize 'a 1)"))
  expect_error(() => eval_program("(rationalize 1)"))
  let str_rat = eval_program("(number->string 1/2)")
  inspect(@runtime.value_to_string(str_rat), content="\"1/2\"")
  let str_float = eval_program("(number->string 1.25)")
  inspect(@runtime.value_to_string(str_float), content="\"1.25\"")
  expect_error(() => eval_program("(number->string 'a)"))
  let abs_big = eval_program("(abs -100000000000000000000)")
  inspect(@runtime.value_to_string(abs_big), content="100000000000000000000")
  let abs_rat = eval_program("(abs -3/2)")
  inspect(@runtime.value_to_string(abs_rat), content="3/2")
  let abs_bigrat = eval_program(
    "(abs -100000000000000000000/100000000000000000001)",
  )
  inspect(
    @runtime.value_to_string(abs_bigrat),
    content="100000000000000000000/100000000000000000001",
  )
  let abs_float = eval_program("(abs -1.5)")
  inspect(@runtime.value_to_string(abs_float), content="1.5")
}

///|
test "sqrt exact rational and bigrat" {
  let program =
    #|(let* ((big (expt 2 70))
    #|       (bign (expt 3 40))
    #|       (rat (sqrt 1/4))
    #|       (bigint-ok (= (sqrt big) (expt 2 35)))
    #|       (bigrat-ok (= (sqrt (/ big bign)) (/ (expt 2 35) (expt 3 20)))))
    #|  (list rat bigint-ok bigrat-ok))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(1/2 #t #t)")
}

///|
test "sqrt exact failure paths" {
  let program =
    #|(list (flonum? (sqrt 2/3))
    #|      (flonum? (sqrt 1/2))
    #|      (complex? (sqrt -4))
    #|      (complex? (sqrt -4/9))
    #|      (flonum? (sqrt (/ 100000000000000000001 3)))
    #|      (flonum? (sqrt (/ 100000000000000000000 3))))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t #t #t)")
}

///|
test "numeric rounding exact branches" {
  let program =
    #|(let* ((big 10000000000000000000001)
    #|       (bigrat (/ big 2))
    #|       (bigrat3 (/ big 3)))
    #|  (list (floor 7/3)
    #|        (ceiling 7/3)
    #|        (truncate 7/3)
    #|        (round 7/3)
    #|        (floor big)
    #|        (ceiling big)
    #|        (truncate big)
    #|        (round big)
    #|        (floor bigrat)
    #|        (ceiling bigrat)
    #|        (truncate bigrat)
    #|        (round bigrat3)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(2 3 2 2 10000000000000000000001 10000000000000000000001 10000000000000000000001 10000000000000000000001 5000000000000000000000 5000000000000000000001 5000000000000000000000 3333333333333333333334)",
  )
}

///|
test "numeric rounding edge cases" {
  let value = eval_program(
    "(list (floor 4/2) (floor -3/2) (ceiling 4/2) (ceiling -3/2) (round 4/2) (round 5/3) (round -5/3))",
  )
  inspect(@runtime.value_to_string(value), content="(2 -2 2 -1 2 2 -2)")
}

///|
test "numeric rounding tie cases" {
  let program =
    #|(let* ((big-even 10000000000000000000001)
    #|       (big-odd 10000000000000000000003)
    #|       (bigrat-even (/ big-even 2))
    #|       (bigrat-odd (/ big-odd 2)))
    #|  (list (round 1/2)
    #|        (round 3/2)
    #|        (round -1/2)
    #|        (round -3/2)
    #|        (round 1/3)
    #|        (round -7/3)
    #|        (round bigrat-even)
    #|        (round bigrat-odd)
    #|        (round (- bigrat-odd))))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(0 2 0 -2 0 -2 5000000000000000000000 5000000000000000000002 -5000000000000000000002)",
  )
}

///|
test "numeric rounding float branches" {
  let program =
    #|(list (flonum? (floor 1.9))
    #|      (flonum? (ceiling 1.1))
    #|      (flonum? (truncate 1.9))
    #|      (flonum? (round 1.9)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t)")
}

///|
test "numeric gcd lcm modulo big" {
  let program =
    #|(let* ((big 10000000000000000000001))
    #|  (list (gcd 12 18)
    #|        (gcd big 3)
    #|        (lcm 6 15)
    #|        (lcm big 3)
    #|        (modulo 5 3)
    #|        (modulo big 3)
    #|        (flonum? (max 1 2.5 2))
    #|        (flonum? (min 1 2.5 2))))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(6 1 30 30000000000000000000003 2 2 #t #t)",
  )
}

///|
test "index coercion and exact integer conversions" {
  let ok_vec = eval_program("(vector-ref #(10 20 30) (make-rectangular 1 0))")
  inspect(@runtime.value_to_string(ok_vec), content="20")
  let ok_bit = eval_program("(bitwise-and 7 (make-rectangular 1 0))")
  inspect(@runtime.value_to_string(ok_bit), content="1")
  expect_error(() => eval_program("(vector-ref #(1 2) (make-rectangular 1 2))"))
  expect_error(() => eval_program("(vector-ref #(1) +)"))
  expect_error(() => eval_program("(bitwise-and 7 (make-rectangular 1 2))"))
}

///|
test "record primitive arity errors" {
  expect_error(() => eval_program("(record?)"))
  expect_error(() => eval_program("(record-type-descriptor?)"))
  expect_error(() => eval_program("(record-constructor-descriptor?)"))
  expect_error(() => eval_program("(record-type-name)"))
  expect_error(() => eval_program("(record-type-parent)"))
  expect_error(() => eval_program("(record-type-uid)"))
  expect_error(() => eval_program("(record-type-generative?)"))
  expect_error(() => eval_program("(record-type-sealed?)"))
  expect_error(() => eval_program("(record-type-opaque?)"))
  expect_error(() => eval_program("(record-type-field-names)"))
  expect_error(() => eval_program("(record-constructor-descriptor)"))
  expect_error(() => eval_program("(record-predicate)"))
  expect_error(() => eval_program("(record-accessor)"))
  expect_error(() => eval_program("(record-mutator)"))
  expect_error(() => eval_program("(make-record-type-descriptor)"))
  expect_error(() => eval_program("(make-record-constructor-descriptor)"))
  expect_error(() => eval_program("(condition?)"))
  expect_error(() => eval_program("(simple-conditions)"))
  expect_error(() => eval_program("(condition-predicate)"))
  expect_error(() => eval_program("(condition-accessor)"))
}

///|
test "hashtable and enum-set arity errors" {
  expect_error(() => eval_program("(make-eq-hashtable 1 2)"))
  expect_error(() => eval_program("(make-eqv-hashtable 1 2)"))
  expect_error(() => eval_program("(make-hashtable)"))
  expect_error(() => eval_program("(hashtable?)"))
  expect_error(() => eval_program("(hashtable-size)"))
  expect_error(() => eval_program("(hashtable-copy)"))
  expect_error(() => eval_program("(hashtable-clear!)"))
  expect_error(() => eval_program("(hashtable-keys)"))
  expect_error(() => eval_program("(hashtable-entries)"))
  expect_error(() => eval_program("(hashtable-equivalence-function)"))
  expect_error(() => eval_program("(hashtable-hash-function)"))
  expect_error(() => eval_program("(hashtable-mutable?)"))
  expect_error(() => eval_program("(make-enumeration)"))
  expect_error(() => eval_program("(enum-set-universe)"))
  expect_error(() => eval_program("(enum-set-indexer)"))
  expect_error(() => eval_program("(enum-set-constructor)"))
  expect_error(() => eval_program("(enum-set?)"))
  expect_error(() => eval_program("(enum-set-member?)"))
  expect_error(() => eval_program("(enum-set-subset?)"))
  expect_error(() => eval_program("(enum-set=?)"))
  expect_error(() => eval_program("(enum-set-union)"))
  expect_error(() => eval_program("(enum-set-intersection)"))
  expect_error(() => eval_program("(enum-set-difference)"))
  expect_error(() => eval_program("(enum-set-complement)"))
  expect_error(() => eval_program("(enum-set-projection)"))
  expect_error(() => eval_program("(enum-set->list)"))
}

///|
test "eqv? on runtime values" {
  let program =
    #|(begin
    #|  (define f (lambda (x) x))
    #|  (define g (lambda (x) x))
    #|  (define c (case-lambda ((x) x)))
    #|  (define p (make-parameter 1))
    #|  (define pr (delay 1))
    #|  (define env (environment '(rnrs base)))
    #|  (define k (call/cc (lambda (k) k)))
    #|  (define ht (make-eq-hashtable))
    #|  (define enum (make-enumeration '(a b)))
    #|  (define set ((enum-set-constructor enum) '(a)))
    #|  (define rtd (make-record-type-descriptor 'pt #f #f #f #f '#((mutable x))))
    #|  (define rcd (make-record-constructor-descriptor rtd #f #f))
    #|  (define make-pt (record-constructor rcd))
    #|  (define rec (make-pt 1))
    #|  (define acc (record-accessor rtd 0))
    #|  (define pred (record-predicate rtd))
    #|  (define-condition-type &c &condition make-c c? (x c-x))
    #|  (define c1 (make-c 1))
    #|  (define cond-acc
    #|    (condition-accessor
    #|      (record-rtd c1)
    #|      (record-accessor (record-rtd c1) 0)))
    #|  (list (eqv? + +)
    #|        (eqv? f f)
    #|        (eqv? f g)
    #|        (eqv? c c)
    #|        (eqv? p p)
    #|        (eqv? pr pr)
    #|        (eqv? env env)
    #|        (eqv? k k)
    #|        (eqv? ht ht)
    #|        (eqv? set set)
    #|        (eqv? rec rec)
    #|        (eqv? acc acc)
    #|        (eqv? pred pred)
    #|        (eqv? cond-acc cond-acc)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(#t #t #f #t #t #t #t #t #t #t #t #t #t #t)",
  )
}

///|
test "eqv? descriptors and ports" {
  let program =
    #|(begin
    #|  (define out (open-output-string))
    #|  (define colors (make-enumeration '(red green)))
    #|  (define ctor (enum-set-constructor colors))
    #|  (define rtd (make-record-type-descriptor 'pt #f #f #f #f '#((mutable x))))
    #|  (define rcd (make-record-constructor-descriptor rtd #f #f))
    #|  (define mk (record-constructor rcd))
    #|  (define s (syntax foo))
    #|  (define x 0)
    #|  (list (eqv? out out)
    #|        (eqv? out 1)
    #|        (eqv? ctor ctor)
    #|        (eqv? rtd rtd)
    #|        (eqv? rcd rcd)
    #|        (eqv? mk mk)
    #|        (eqv? s s)
    #|        (eqv? (set! x 1) (set! x 1))))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #f #t #t #t #t #t #t)")
}

///|
test "identifier scope mismatch" {
  let value = eval_program(
    "(begin (define a (datum->syntax #f 'x)) (define b (syntax x)) (list (free-identifier=? a b) (bound-identifier=? a b)))",
  )
  inspect(@runtime.value_to_string(value), content="(#f #f)")
}

///|
test "datum->syntax gensym stripping" {
  let value = eval_program(
    "(begin (define s (datum->syntax (syntax foo__sg123__sg4) 'foo)) (symbol->string (syntax->datum s)))",
  )
  inspect(@runtime.value_to_string(value), content="\"foo\"")
  let plain = eval_program(
    "(begin (define s (datum->syntax (syntax foo) 'foo)) (symbol->string (syntax->datum s)))",
  )
  inspect(@runtime.value_to_string(plain), content="\"foo\"")
}

///|
test "datum->syntax gensym suffix handling" {
  let value = eval_program(
    "(begin (define tmpl (syntax foo__sg1__sg2)) (define res1 (datum->syntax tmpl 'foo)) (define res2 (datum->syntax tmpl 'bar)) (define digits (datum->syntax #f (string->symbol \"12345\"))) (define res3 (datum->syntax digits 'foo)) (define res4 (datum->syntax (syntax __sg123) '__sg123)) (list (symbol->string (syntax->datum res1)) (symbol->string (syntax->datum res2)) (symbol->string (syntax->datum res3)) (symbol->string (syntax->datum res4))))",
  )
  inspect(
    @runtime.value_to_string(value),
    content="(\"foo\" \"bar\" \"foo\" \"__sg123\")",
  )
}

///|
test "numeric complex operations" {
  let program =
    #|(begin
    #|  (abs (make-rectangular 3 4))
    #|  (angle (make-rectangular 0 1))
    #|  (sqrt (make-rectangular 0 1))
    #|  (exp (make-rectangular 0 1))
    #|  (log (make-rectangular 0 1))
    #|  (expt 2 -3)
    #|  (expt 12345678901234567890 -2)
    #|  (expt 1/2 -2)
    #|  (expt 12345678901234567890/12345678901234567891 -1)
    #|  (number->string 12345678901234567890)
    #|  (string? (number->string (make-rectangular 1 2)))
    #|  #t)
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="#t")
}

///|
test "flonum edge cases" {
  let program =
    #|(begin
    #|  (import (rnrs arithmetic flonums))
    #|  (list (fl=?)
    #|        (fl=? 1.0)
    #|        (fl<? 1.0)
    #|        (fl>? 1.0)
    #|        (fl<=? 1.0)
    #|        (fl>=? 1.0)
    #|        (fl=? 1.0 2.0)
    #|        (fl<? 2.0 1.0)
    #|        (fl>? 1.0 2.0)
    #|        (fl<=? 2.0 1.0)
    #|        (fl>=? 1.0 2.0)
    #|        (flnan? (flmax +nan.0 1.0))
    #|        (flnan? (flmin +nan.0 1.0))))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(#t #t #t #t #t #t #f #f #f #f #f #t #t)",
  )
  expect_error(() => {
    eval_program("(begin (import (rnrs arithmetic flonums)) (flmax))")
  })
  expect_error(() => {
    eval_program("(begin (import (rnrs arithmetic flonums)) (flmin))")
  })
  expect_error(() => {
    eval_program("(begin (import (rnrs arithmetic flonums)) (fl-))")
  })
  expect_error(() => {
    eval_program("(begin (import (rnrs arithmetic flonums)) (fl/))")
  })
}

///|
test "hashtable sizing and arity" {
  let program =
    #|(begin
    #|  (define ht1 (make-eq-hashtable 4))
    #|  (define ht2 (make-eqv-hashtable 2))
    #|  (define ht3 (make-hashtable (lambda (x) 0) eq? 3))
    #|  (hashtable-set! ht1 'a 1)
    #|  (define keys (hashtable-keys ht1))
    #|  (define ok-entries
    #|    (call-with-values
    #|      (lambda () (hashtable-entries ht1))
    #|      (lambda (ks vs)
    #|        (and (= (vector-length ks) 1)
    #|             (= (vector-length vs) 1)))))
    #|  (define copy (hashtable-copy ht1))
    #|  (define copy-imm (hashtable-copy ht1 #f))
    #|  (hashtable-clear! copy)
    #|  (hashtable-clear! ht1 5)
    #|  (and (hashtable? ht1)
    #|       (not (hashtable? 1))
    #|       (= (vector-length keys) 1)
    #|       ok-entries
    #|       (procedure? (hashtable-equivalence-function ht1))
    #|       (eq? (hashtable-hash-function ht1) #f)
    #|       (procedure? (hashtable-hash-function ht3))
    #|       (hashtable-mutable? copy)
    #|       (not (hashtable-mutable? copy-imm))
    #|       (= (hashtable-size ht2) 0)
    #|       (= (hashtable-size ht3) 0)
    #|       (= (hashtable-size ht1) 0)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="#t")
  expect_error(() => eval_program("(make-eq-hashtable -1)"))
  expect_error(() => eval_program("(make-hashtable 1 eq?)"))
  expect_error(() => eval_program("(make-hashtable 1 eq? 3)"))
  expect_error(() => eval_program("(hashtable?)"))
}

///|
test "hashtable immutable mutation" {
  expect_error(() => {
    eval_program(
      "(begin (define ht (hashtable-copy (make-eq-hashtable) #f)) (hashtable-set! ht 'a 1))",
    )
  })
}

///|
test "enum set error paths" {
  let program =
    #|(begin
    #|  (define colors (make-enumeration '(red green blue)))
    #|  (define ctor (enum-set-constructor colors))
    #|  (define s1 (ctor '(red blue)))
    #|  (define s2 (ctor '(green)))
    #|  (define uni (enum-set-universe s1))
    #|  (list (enum-set-member? 'green uni)
    #|        (enum-set-subset? s1 s2)
    #|        (enum-set=? s1 s2)
    #|        (enum-set=? (enum-set-projection s1 s2) s1)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #f #f #t)")
  expect_error(() => eval_program("(enum-set-member?)"))
  expect_error(() => eval_program("(enum-set-universe)"))
  expect_error(() => eval_program("(enum-set?)"))
}

///|
test "record and condition error paths" {
  let rtd_false = eval_program("(record-type-descriptor? 1)")
  inspect(@runtime.value_to_string(rtd_false), content="#f")
  let rcd_false = eval_program("(record-constructor-descriptor? 1)")
  inspect(@runtime.value_to_string(rcd_false), content="#f")
  expect_error(() => eval_program("(record-rtd 1)"))
  expect_error(() => eval_program("(record-rtd)"))
  expect_error(() => eval_program("(record-type-name)"))
  expect_error(() => eval_program("(record-type-field-mutable?)"))
  expect_error(() => {
    eval_program(
      "(begin (define rtd (make-record-type-descriptor 'pt #f #f #f #f '#((immutable x)))) (record-mutator rtd 0))",
    )
  })
  expect_error(() => {
    eval_program("(make-record-type-descriptor 'pt #f 1 #f #f '#())")
  })
  expect_error(() => {
    eval_program(
      "(begin (define rtd (make-record-type-descriptor 'pt #f #f #f #f '#())) (make-record-constructor-descriptor rtd #f 1))",
    )
  })
  expect_error(() => eval_program("(condition)"))
  expect_error(() => {
    eval_program(
      "(begin (define-condition-type &c1 &condition make-c1 c1? (x c1-x)) (define-condition-type &c2 &condition make-c2 c2? (y c2-y)) (define acc (record-accessor (record-rtd (make-c2 1)) 0)) (condition-accessor (record-rtd (make-c1 1)) acc))",
    )
  })
  expect_error(() => {
    eval_program(
      "(condition-predicate (make-record-type-descriptor 'pt #f #f #f #f '#((mutable x))))",
    )
  })
  expect_error(() => {
    eval_program(
      "(begin (define-condition-type &c &condition make-c c? (x c-x)) (condition-accessor (record-rtd (make-c 1)) (lambda (x) x)))",
    )
  })
}

///|
test "define-record-type option variants" {
  let program =
    #|(begin
    #|  (define-record-type base-r (make-base-r x) base-r? (x base-r-x))
    #|  (define-record-type sealed-r (make-sealed-r x) sealed-r? (sealed) (x sealed-r-x))
    #|  (define-record-type opaque-r (make-opaque-r x) opaque-r? (opaque) (x opaque-r-x))
    #|  (define-record-type child-r
    #|    (make-child-r x y)
    #|    child-r?
    #|    (parent base-r)
    #|    (protocol (lambda (p) (lambda (x y) ((p x) y))))
    #|    (sealed #f)
    #|    (opaque #f)
    #|    (y child-r-y))
    #|  (define-record-type gen0 (make-gen0 a) gen0? (nongenerative) (a gen0-a))
    #|  (define-record-type gen1 (make-gen1 a) gen1? (nongenerative uid-shared) (a gen1-a))
    #|  (define-record-type gen2 (make-gen2 a) gen2? (nongenerative uid-shared) (a gen2-a))
    #|  (list (sealed-r? (make-sealed-r 1))
    #|        (opaque-r? (make-opaque-r 2))
    #|        (child-r? (make-child-r 3 4))
    #|        (child-r-y (make-child-r 5 6))
    #|        (gen0? (make-gen0 7))
    #|        (gen2? (make-gen2 8))))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t 6 #t #t)")
}

///|
test "define-record-type error cases" {
  let expect_err = (expr : String) => expect_error(() => eval_program(expr))
  expect_err("(define-record-type r () r?)")
  expect_err(
    "(define-record-type r (make-r a) r? (parent p) (parent q) (a r-a))",
  )
  expect_err(
    "(define-record-type r (make-r a) r? (protocol (lambda (p) p)) (protocol (lambda (p) p)) (a r-a))",
  )
  expect_err(
    "(define-record-type r (make-r a) r? (nongenerative uid) (nongenerative uid) (a r-a))",
  )
  expect_err("(define-record-type r (make-r a) r? (parent unknown) (a r-a))")
  expect_err("(define-record-type r (make-r a) r? (a))")
  expect_err("(define-record-type r (make-r a b) r? (a r-a))")
  expect_err("(define-record-type r (make-r a a) r? (a r-a) (b r-b))")
  expect_err("(define-record-type r (make-r a c) r? (a r-a) (b r-b))")
  let parent_mismatch =
    #|(begin
    #|  (define-record-type p (make-p a) p? (a p-a))
    #|  (define-record-type c (make-c b c) c? (parent p) (b c-b) (c c-c)))
  expect_err(parent_mismatch)
  let sealed_parent =
    #|(begin
    #|  (define-record-type sp (make-sp a) sp? (sealed) (a sp-a))
    #|  (define-record-type sc (make-sc a b) sc? (parent sp) (a sc-a) (b sc-b)))
  expect_err(sealed_parent)
  let uid_mismatch =
    #|(begin
    #|  (define-record-type u1 (make-u1 a) u1? (nongenerative uid-mismatch) (a u1-a))
    #|  (define-record-type u2 (make-u2 a b) u2? (nongenerative uid-mismatch) (a u2-a) (b u2-b)))
  expect_err(uid_mismatch)
  let uid_protocol =
    #|(begin
    #|  (define-record-type u3 (make-u3 a) u3? (nongenerative uid-proto) (a u3-a))
    #|  (define-record-type u4 (make-u4 a) u4? (nongenerative uid-proto) (protocol (lambda (p) p)) (a u4-a)))
  expect_err(uid_protocol)
}

///|
test "define-condition-type error cases" {
  let expect_err = (expr : String) => expect_error(() => eval_program(expr))
  expect_err("(define-condition-type)")
  expect_err("(define-condition-type &c &missing make-c c? (x c-x))")
  expect_err("(define-condition-type &c &condition make-c c? (x))")
  expect_err("(define-condition-type &c &condition make-c c? (x c-x) (x c-y))")
  let not_condition_parent =
    #|(begin
    #|  (define-record-type r (make-r a) r? (a r-a))
    #|  (define-condition-type &c r make-c c? (x c-x)))
  expect_err(not_condition_parent)
  let sealed_parent =
    #|(begin
    #|  (define-record-type sealed (make-sealed a) sealed? (sealed) (a sealed-a))
    #|  (define-condition-type &c sealed make-c c? (x c-x)))
  expect_err(sealed_parent)
  let name_exists =
    #|(begin
    #|  (define-record-type r (make-r a) r? (a r-a))
    #|  (define-condition-type r &condition make-c c? (x c-x)))
  expect_err(name_exists)
}

///|
test "macro invalid syntax" {
  let ok_id = eval_program(
    "(begin (define-syntax const (identifier-syntax 42)) const)",
  )
  inspect(@runtime.value_to_string(ok_id), content="42")
  expect_error(() => eval_program("(define-syntax bad (syntax-rules ()))"))
  expect_error(() => eval_program("(define-syntax bad (syntax-rules () (x)))"))
  expect_error(() => eval_program("(define-syntax bad (identifier-syntax))"))
  expect_error(() => {
    eval_program("(define-syntax bad (identifier-syntax (x) (y)))")
  })
  expect_error(() => {
    eval_program(
      "(define-syntax bad (lambda (stx) (syntax-case stx () ((_) 1 2 3))))",
    )
  })
}

///|
test "eval environment and promise" {
  let basic =
    #|(let ((env (environment '(rnrs base))))
    #|  (eval '(+ 1 2) env))
  inspect(@runtime.value_to_string(eval_program(basic)), content="3")
  let define_prog =
    #|(let ((env (environment '(rnrs base))))
    #|  (eval '(define x 10) env)
    #|  (eval 'x env))
  inspect(@runtime.value_to_string(eval_program(define_prog)), content="10")
  expect_error(() => eval_program("(environment)"))
  let promise_val = eval_program("(force (delay (+ 1 2)))")
  inspect(@runtime.value_to_string(promise_val), content="3")
  expect_error(() => eval_program("(make-promise 1)"))
}

///|
test "numeric error branches" {
  expect_error(() => eval_program("(exact-integer-sqrt 1/2)"))
  expect_error(() => eval_program("(rationalize (make-rectangular 1 1) 0.1)"))
  expect_error(() => eval_program("(number->string 1/2 2)"))
  expect_error(() => eval_program("(string->number \"10\" 1)"))
  expect_error(() => eval_program("(expt 0 -1)"))
  expect_error(() => eval_program("(fxcopy-bit 0 0 2)"))
}

///|
test "numeric helper error cases" {
  expect_error(() => eval_program("(abs 'a)"))
  expect_error(() => eval_program("(numerator 1.5)"))
  expect_error(() => eval_program("(denominator 1.5)"))
}

///|
test "numeric complex branches" {
  let complex_flags = eval_program(
    "(list (complex? (sin (make-rectangular 1 1))) (complex? (cos (make-rectangular 1 1))) (complex? (tan (make-rectangular 1 1))) (complex? (exp (make-rectangular 1 1))) (complex? (log (make-rectangular 1 1))) (complex? (asin (make-rectangular 1 1))) (complex? (acos (make-rectangular 1 1))) (complex? (atan (make-rectangular 1 1))) (complex? (sqrt -1)))",
  )
  inspect(
    @runtime.value_to_string(complex_flags),
    content="(#t #t #t #t #t #t #t #t #t)",
  )
}

///|
test "make-rectangular inexact zero" {
  let program =
    #|(list (inexact? (make-rectangular 2 0.0))
    #|      (inexact? (make-rectangular 1/2 0.0))
    #|      (inexact? (make-rectangular 10000000000000000000001 0.0))
    #|      (inexact? (make-rectangular 10000000000000000000001/3 0.0))
    #|      (exact? (make-rectangular 2 0)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t #t)")
}

///|
test "macro pattern coverage" {
  let program =
    #|(begin
    #|  (define-syntax classify
    #|    (lambda (stx)
    #|      (syntax-case stx (foo)
    #|        ((_ ()) #'(quote nil))
    #|        ((_ #t) #'(quote bool))
    #|        ((_ 42) #'(quote int))
    #|        ((_ 1/2) #'(quote rat))
    #|        ((_ 1.0) #'(quote float))
    #|        ((_ 1+2i) #'(quote complex))
    #|        ((_ #\a) #'(quote char))
    #|        ((_ "hi") #'(quote string))
    #|        ((_ foo) #'(quote symbol))
    #|        ((_ (1 2)) #'(quote pair))
    #|        ((_ #(1 2)) #'(quote vector))
    #|        ((_ #vu8(1 2)) #'(quote bytevector))
    #|        ((_ 9999999999999999999999) #'(quote bigint))
    #|        ((_ 9999999999999999999999/2) #'(quote bigrat))
    #|        (_ #'(quote other)))))
    #|  (list (classify ())
    #|        (classify #t)
    #|        (classify 42)
    #|        (classify 1/2)
    #|        (classify 1.0)
    #|        (classify 1+2i)
    #|        (classify #\a)
    #|        (classify "hi")
    #|        (classify foo)
    #|        (classify (1 2))
    #|        (classify #(1 2))
    #|        (classify #vu8(1 2))
    #|        (classify 9999999999999999999999)
    #|        (classify 9999999999999999999999/2)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(nil bool int rat float complex char string symbol pair vector bytevector bigint bigrat)",
  )
}

///|
test "macro vector template coverage" {
  let program =
    #|(begin
    #|  (define-syntax list->vec
    #|    (syntax-rules ()
    #|      ((_ x ...) #(x ...))))
    #|  (list->vec 1 2 3))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="#(1 2 3)")
}

///|
test "macro dotted pair template" {
  let program =
    #|(begin
    #|  (define-syntax make-dotted
    #|    (syntax-rules ()
    #|      ((_ a b)
    #|        (syntax->datum (quasisyntax (a . b))))))
    #|  (make-dotted 1 2))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(1 . 2)")
}

///|
test "macro with-syntax coverage" {
  let program =
    #|(begin
    #|  (define-syntax dup
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_ x)
    #|          (with-syntax ((y x))
    #|            #'(list y y))))))
    #|  (dup 7))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(7 7)")
}

///|
test "macro with-syntax multi-body" {
  let program =
    #|(begin
    #|  (define-syntax dup+sum
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_ x)
    #|          (with-syntax ((y x))
    #|            #'(begin y (+ y y)))))))
    #|  (dup+sum 3))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="6")
}

///|
test "macro procedure transformer vector and complex" {
  let program =
    #|(begin
    #|  (define-syntax make-vec
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_ x) #'#(x x)))))
    #|  (define-syntax make-cpx
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_) #'1+2i))))
    #|  (list (make-vec 5) (make-cpx)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#(5 5) 1+2i)")
}

///|
test "datum->syntax vector and complex" {
  let program =
    #|(begin
    #|  (define v (datum->syntax #f '#(1 2 3)))
    #|  (define c (datum->syntax #f '1+2i))
    #|  (list (syntax->datum v) (syntax->datum c)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#(1 2 3) 1+2i)")
}

///|
test "macro invalid forms" {
  let expect_err = (expr : String) => expect_error(() => eval_program(expr))
  expect_err("(define-syntax bad ())")
  expect_err("(define-syntax bad (make-variable-transformer))")
  expect_err("(define-syntax bad 1)")
  expect_err("(define-syntax bad (syntax-rules))")
  expect_err("(define-syntax bad (syntax-rules (1) ((_ x) x)))")
  expect_err("(define-syntax bad (identifier-syntax))")
  expect_err("(define-syntax bad (lambda (x) (syntax-case x () 1)))")
}

///|
test "syntax-case fender" {
  let program =
    #|(begin
    #|  (define-syntax classify
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_ x) (eqv? (syntax->datum #'x) 1) #'(list 'num x))
    #|        ((_ x) #'(list 'other x)))))
    #|  (list (classify 1) (classify 'a)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="((other 1) (other a))")
}

///|
test "syntax-case invalid form" {
  expect_error(() => eval_program("(syntax-case (syntax x) () )"))
}

///|
test "procedure macro expansion" {
  let program =
    #|(begin
    #|  (define-syntax proc-vec (lambda (stx) '#(1 2)))
    #|  (define-syntax proc-cpx (lambda (stx) '1+2i))
    #|  (list (proc-vec) (proc-cpx)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#(1 2) 1+2i)")
}

///|
test "with-syntax invalid bindings" {
  expect_error(() => {
    eval_program(
      "(let-syntax ((m (syntax-rules () ((_ ) (with-syntax (x) #'x))))) (m))",
    )
  })
  expect_error(() => {
    eval_program(
      "(let-syntax ((m (syntax-rules () ((_ ) (with-syntax (((x y) '(1))) #'x))))) (m))",
    )
  })
}

///|
test "quasisyntax splicing with syntax object" {
  let value = eval_program(
    "(syntax->datum (quasisyntax ((unsyntax-splicing (syntax (1 2))) 3)))",
  )
  inspect(@runtime.value_to_string(value), content="(1 2 3)")
  expect_error(() => {
    eval_program("(quasisyntax (unsyntax-splicing (syntax (1 2))))")
  })
}

///|
test "quasisyntax ellipsis templates" {
  let program =
    #|(begin
    #|  (define-syntax qlist
    #|    (syntax-rules ()
    #|      ((_ (x ...) y)
    #|        (syntax->datum (quasisyntax (x ... y))))))
    #|  (define-syntax qvec
    #|    (syntax-rules ()
    #|      ((_ x ...)
    #|        (syntax->datum (quasisyntax #(x ...))))))
    #|  (list (qlist (1 2 3) 4) (qvec 1 2 3)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="((1 2 3 4) #(1 2 3))")
}

///|
test "quasisyntax ellipsis errors" {
  expect_error(() => {
    eval_program(
      "(begin (define-syntax bad (lambda (stx) (syntax-case stx () ((_ x) (quasisyntax (...)))))) (bad 1))",
    )
  })
  expect_error(() => {
    eval_program(
      "(begin (define-syntax bad (lambda (stx) (syntax-case stx () ((_ x) (quasisyntax (x ... ...)))))) (bad 1))",
    )
  })
  expect_error(() => {
    eval_program(
      "(begin (define-syntax bad (lambda (stx) (syntax-case stx () ((_ x) (quasisyntax #(x ... ...)))))) (bad 1))",
    )
  })
}

///|
test "macro ellipsis expansion" {
  let program =
    #|(begin
    #|  (define-syntax swap-pairs
    #|    (syntax-rules ()
    #|      ((_ (a b) ...) (list (list b a) ...))))
    #|  (swap-pairs (1 2) (3 4)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="((2 1) (4 3))")
}

///|
test "utf8 multibyte roundtrip" {
  let roundtrip = eval_program(
    "(equal? (string->utf8 (utf8->string #vu8(65 194 162 226 130 172 240 144 141 136))) #vu8(65 194 162 226 130 172 240 144 141 136))",
  )
  inspect(@runtime.value_to_string(roundtrip), content="#t")
}

///|
test "bytevector int refs" {
  let uint_big = eval_program("(bytevector-uint-ref #vu8(1 2 3 4) 0 'big 2)")
  inspect(@runtime.value_to_string(uint_big), content="258")
  let uint_little = eval_program(
    "(bytevector-uint-ref #vu8(1 2 3 4) 0 'little 2)",
  )
  inspect(@runtime.value_to_string(uint_little), content="513")
  let sint_neg = eval_program("(bytevector-sint-ref #vu8(255) 0 'big 1)")
  inspect(@runtime.value_to_string(sint_neg), content="-1")
  let uint_set = eval_program(
    "(let ((bv (make-bytevector 4 0))) (bytevector-uint-set! bv 0 'big 2 258) bv)",
  )
  inspect(@runtime.value_to_string(uint_set), content="#vu8(1 2 0 0)")
  let sint_set = eval_program(
    "(let ((bv (make-bytevector 1 0))) (bytevector-sint-set! bv 0 'big 1 -1) bv)",
  )
  inspect(@runtime.value_to_string(sint_set), content="#vu8(255)")
}

///|
test "complex exactness predicates" {
  let exact_int = eval_program("(exact-integer? (make-rectangular 2 0))")
  inspect(@runtime.value_to_string(exact_int), content="#t")
  let integer_val = eval_program("(integer? (make-rectangular 3 0))")
  inspect(@runtime.value_to_string(integer_val), content="#t")
  let rational_val = eval_program("(rational? (make-rectangular 3/2 0))")
  inspect(@runtime.value_to_string(rational_val), content="#t")
}

///|
test "odd and even complex cases" {
  let program =
    #|(let ((small (string->number "1+0i"))
    #|      (evenv (string->number "2+0i"))
    #|      (big (string->number "10000000000000000000001+0i")))
    #|  (list (odd? small) (even? evenv) (odd? big) (even? big)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #f)")
  expect_error(() => eval_program("(odd? 1+1i)"))
}

///|
test "exact? complex combinations" {
  let program =
    #|(let* ((big 10000000000000000000001)
    #|       (rat 1/2)
    #|       (bigrat (/ big 3)))
    #|  (list (exact? (make-rectangular 1 1))
    #|        (exact? (make-rectangular 1 big))
    #|        (exact? (make-rectangular 1 rat))
    #|        (exact? (make-rectangular 1 bigrat))
    #|        (exact? (make-rectangular big 1))
    #|        (exact? (make-rectangular big big))
    #|        (exact? (make-rectangular big rat))
    #|        (exact? (make-rectangular big bigrat))
    #|        (exact? (make-rectangular rat 1))
    #|        (exact? (make-rectangular rat big))
    #|        (exact? (make-rectangular rat rat))
    #|        (exact? (make-rectangular rat bigrat))
    #|        (exact? (make-rectangular bigrat 1))
    #|        (exact? (make-rectangular bigrat big))
    #|        (exact? (make-rectangular bigrat rat))
    #|        (exact? (make-rectangular bigrat bigrat))))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)",
  )
}

///|
test "inexact/exact conversions" {
  let exact_zero = eval_program("(inexact->exact 0.0)")
  inspect(@runtime.value_to_string(exact_zero), content="0")
  let exact_half = eval_program("(inexact->exact 0.5)")
  inspect(@runtime.value_to_string(exact_half), content="1/2")
  expect_error(() => eval_program("(inexact->exact +inf.0)"))
  let inexact_big = eval_program(
    "(flonum? (exact->inexact 9999999999999999999999))",
  )
  inspect(@runtime.value_to_string(inexact_big), content="#t")
  let inexact_bigrat = eval_program(
    "(flonum? (exact->inexact 9999999999999999999999/2))",
  )
  inspect(@runtime.value_to_string(inexact_bigrat), content="#t")
}

///|
test "expt branch matrix" {
  let program =
    #|(let* ((big 9999999999999999999999)
    #|       (zero (+ big (- big)))
    #|       (one (+ 1 zero))
    #|       (neg-one (- zero 1)))
    #|  (list (expt 2/3 -1)
    #|        (expt -2.0 -2)
    #|        (complex? (expt (make-rectangular 1 1) 2))
    #|        (integer? (expt big one))
    #|        (rational? (expt big neg-one))
    #|        (expt big zero)
    #|        (complex? (expt (make-rectangular 1 1) one))
    #|        (flonum? (expt -2.0 one))
    #|        (complex? (expt (make-rectangular 1 1) 0.5))
    #|        (complex? (expt (make-rectangular 1 1) 1/2))
    #|        (complex? (expt 2 (make-rectangular 1 1)))))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(3/2 0.25 #t #t #t 1 #t #t #t #t #t)",
  )
}

///|
test "integer division and gcd/lcm bigints" {
  let program =
    #|(let ((big 9999999999999999999999))
    #|  (list (integer? (quotient big 3))
    #|        (integer? (remainder big 3))
    #|        (= (modulo (- 0 big 1) 3) 2)
    #|        (= (gcd big 3) 3)
    #|        (= (lcm big 3) big)
    #|        (flonum? (abs 1+2i))))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t #t #t)")
  expect_error(() => eval_program("(quotient 1 0)"))
}

///|
test "expt bigint paths" {
  let values = eval_program(
    "(let ((big 9999999999999999999999)) (list (expt 2 (+ 2 (- big big))) (expt 2 (- (+ big (- big)) 1))))",
  )
  inspect(@runtime.value_to_string(values), content="(4 1/2)")
}

///|
test "numeric real/magnitude branches" {
  let magnitude_rat = eval_program("(magnitude -2/3)")
  inspect(@runtime.value_to_string(magnitude_rat), content="2/3")
  let imag_rat = eval_program("(imag-part 2/3)")
  inspect(@runtime.value_to_string(imag_rat), content="0")
  let sqrt_real = eval_program("(sqrt (make-rectangular 9 0))")
  inspect(@runtime.value_to_string(sqrt_real), content="3")
  let complex_check = eval_program("(complex? (expt -2 0.5))")
  inspect(@runtime.value_to_string(complex_check), content="#t")
  let first_zero = eval_program("(fxfirst-bit-set 0)")
  inspect(@runtime.value_to_string(first_zero), content="-1")
}

///|
test "magnitude big branches" {
  let program =
    #|(list (magnitude -4)
    #|      (magnitude -1.5)
    #|      (magnitude 1.5)
    #|      (magnitude -9999999999999999999999)
    #|      (magnitude (- (/ 10000000000000000000001 3))))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(4 1.5 1.5 9999999999999999999999 10000000000000000000001/3)",
  )
}

///|
test "number->string radix branches" {
  let ok = eval_program(
    "(list (number->string 1/2 10) (number->string 1.5 10))",
  )
  inspect(@runtime.value_to_string(ok), content="(\"1/2\" \"1.5\")")
  expect_error(() => eval_program("(number->string 1/2 16)"))
  expect_error(() => eval_program("(number->string 1 1)"))
  expect_error(() => eval_program("(number->string 1+2i 16)"))
}

///|
test "expt rational int exponent branches" {
  let program =
    #|(let ((big 10000000000000000000001))
    #|  (list (expt 2/3 1)
    #|        (expt 2/3 -1)
    #|        (expt (/ big 3) 1)
    #|        (expt (/ big 3) -1)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(2/3 3/2 10000000000000000000001/3 3/10000000000000000000001)",
  )
  expect_error(() => {
    eval_program("(let ((big 10000000000000000000001)) (expt (- big big) -1))")
  })
}

///|
test "expt bigint exponent branches" {
  let program =
    #|(let* ((big 10000000000000000000001)
    #|       (exp big)
    #|       (exp-neg (- big)))
    #|  (list (integer? (expt 1 exp))
    #|        (rational? (expt 1 exp-neg))
    #|        (flonum? (expt 1.0 exp))
    #|        (flonum? (expt 1.0 exp-neg))))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t #t)")
  expect_error(() => {
    eval_program(
      "(let ((big 10000000000000000000001)) (expt (- big big) (- (abs big))))",
    )
  })
}

///|
test "fixnum empty and copy bit branches" {
  let program =
    #|(list (fxand)
    #|      (fxior)
    #|      (fxxor)
    #|      (fxnot 0)
    #|      (fxcopy-bit 10 1 0))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(-1 0 0 -1 8)")
}

///|
test "bitwise copy bit branches" {
  let value = eval_program(
    "(list (bitwise-copy-bit 10 1 0) (bitwise-copy-bit 10 1 1))",
  )
  inspect(@runtime.value_to_string(value), content="(8 10)")
  expect_error(() => eval_program("(bitwise-copy-bit 10 1 2)"))
}

///|
test "fixnum division sign branches" {
  let program =
    #|(list (fxdiv 7 -2)
    #|      (fxdiv -7 2)
    #|      (fxmod 7 -2)
    #|      (fxmod -7 2)
    #|      (fxdiv0 -7 2)
    #|      (fxmod0 -7 2))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(-4 -4 -1 1 -3 -1)")
  expect_error(() => eval_program("(fxdiv 1 0)"))
  expect_error(() => eval_program("(fxmod 1 0)"))
  expect_error(() => eval_program("(fxdiv0 1 0)"))
  expect_error(() => eval_program("(fxmod0 1 0)"))
}

///|
test "numeric arity mismatch coverage" {
  let expect_err = (expr : String) => expect_error(() => eval_program(expr))
  expect_err("(exp)")
  expect_err("(log)")
  expect_err("(expt 1)")
  expect_err("(bitwise-not)")
  expect_err("(bitwise-if 1 2)")
  expect_err("(bitwise-bit-count)")
  expect_err("(bitwise-length)")
  expect_err("(bitwise-first-bit-set)")
  expect_err("(bitwise-bit-set? 1)")
  expect_err("(bitwise-copy-bit 1 2)")
  expect_err("(bitwise-bit-field 1 2)")
  expect_err("(bitwise-copy-bit-field 1 2 3)")
  expect_err("(bitwise-rotate-bit-field 1 2 3)")
  expect_err("(bitwise-reverse-bit-field 1 2)")
  expect_err("(arithmetic-shift 1)")
  expect_err("(fxmin)")
  expect_err("(fxmax)")
  expect_err("(fx-)")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fx+/carry 1 2))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fx-/carry 1 2))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fx*/carry 1 2))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fxnot))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fxbit-count))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fxlength))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fxfirst-bit-set))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fxbit-set?))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fxcopy-bit 1 2))")
  expect_err("(begin (import (rnrs arithmetic fixnums)) (fxbit-field 1 2))")
  expect_err(
    "(begin (import (rnrs arithmetic fixnums)) (fxcopy-bit-field 1 2 3))",
  )
  expect_err(
    "(begin (import (rnrs arithmetic fixnums)) (fxrotate-bit-field 1 2 3))",
  )
  expect_err(
    "(begin (import (rnrs arithmetic fixnums)) (fxreverse-bit-field 1 2))",
  )
  expect_err(
    "(begin (import (rnrs arithmetic fixnums)) (fxarithmetic-shift 1))",
  )
  expect_err(
    "(begin (import (rnrs arithmetic fixnums)) (fxarithmetic-shift-left 1))",
  )
  expect_err(
    "(begin (import (rnrs arithmetic fixnums)) (fxarithmetic-shift-right 1))",
  )
  expect_err("(begin (import (rnrs arithmetic flonums)) (flonum? 1.0 2.0))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (real->flonum))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fixnum->flonum))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flinteger?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flzero?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flpositive?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flnegative?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flodd?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fleven?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flfinite?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flinfinite?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flnan?))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flabs))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fldiv-and-mod 1.0))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fldiv 1.0))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flmod 1.0))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fldiv0-and-mod0 1.0))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fldiv0 1.0))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flmod0 1.0))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flnumerator))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fldenominator))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flfloor))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flceiling))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fltruncate))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flround))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flexp))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fllog))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flsin))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flcos))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (fltan))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flasin))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flacos))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flatan))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flsqrt))")
  expect_err("(begin (import (rnrs arithmetic flonums)) (flexpt 1.0))")
}

///|
test "flmax and flmin nan branches" {
  let program =
    #|(list (flnan? (flmax 1.0 +nan.0 2.0))
    #|      (flnan? (flmin 1.0 +nan.0 2.0)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t)")
}

///|
test "flonum non-finite rounding" {
  let values = eval_program(
    "(list (flonum? (lambda (x) x)) (flfloor +inf.0) (flceiling +inf.0) (fltruncate +inf.0) (flround +inf.0))",
  )
  inspect(
    @runtime.value_to_string(values),
    content="(#f Infinity Infinity Infinity Infinity)",
  )
}

///|
test "bytevector utf8 and endian errors" {
  let native = eval_program("(native-endianness)")
  inspect(@runtime.value_to_string(native), content="little")
  expect_error(() => eval_program("(bytevector-uint-ref #vu8(1 2) 0 'bogus 1)"))
  expect_error(() => eval_program("(bytevector-uint-ref #vu8(1 2) 0 'big 0)"))
  expect_error(() => eval_program("(bytevector-uint-ref #vu8(1 2) 2 'big 1)"))
  expect_error(() => eval_program("(utf8->string #vu8(255))"))
  expect_error(() => eval_program("(utf8->string #vu8(194))"))
  expect_error(() => eval_program("(utf8->string #vu8(194 0))"))
  expect_error(() => eval_program("(utf8->string #vu8(1 2) 2 1)"))
  expect_error(() => eval_program("(utf8->string #vu8(226 130))"))
  expect_error(() => eval_program("(utf8->string #vu8(226 0 130))"))
  expect_error(() => eval_program("(utf8->string #vu8(224 159 128))"))
  expect_error(() => eval_program("(utf8->string #vu8(237 160 128))"))
  expect_error(() => eval_program("(utf8->string #vu8(240 159 128))"))
  expect_error(() => eval_program("(utf8->string #vu8(240 159 0 128))"))
  expect_error(() => eval_program("(utf8->string #vu8(240 143 128 128))"))
  expect_error(() => eval_program("(utf8->string #vu8(244 144 128 128))"))
  expect_error(() => eval_program("(string->utf8 \"abc\" 3 1)"))
}

///|
test "utf8 decode multibyte success" {
  let program =
    #|(let* ((s2 (utf8->string #vu8(194 162)))
    #|       (s3 (utf8->string #vu8(226 130 172)))
    #|       (s4 (utf8->string #vu8(240 159 152 128)))
    #|       (text "\xA2;\x20AC;\x1F600;"))
    #|  (list (string->utf8 s2)
    #|        (string->utf8 s3)
    #|        (string->utf8 s4)
    #|        (string->utf8 text)))
  let value = eval_program(program)
  inspect(
    @runtime.value_to_string(value),
    content="(#vu8(194 162) #vu8(226 130 172) #vu8(240 159 152 128) #vu8(194 162 226 130 172 240 159 152 128))",
  )
}

///|
test "complex literal index coercion" {
  let ok_vec = eval_program("(vector-ref #(10 20 30) 1+0i)")
  inspect(@runtime.value_to_string(ok_vec), content="20")
  let ok_bit = eval_program("(bitwise-and 7 1+0i)")
  inspect(@runtime.value_to_string(ok_bit), content="1")
  expect_error(() => {
    eval_program("(vector-ref #(1 2) 10000000000000000000001+0i)")
  })
  expect_error(() => eval_program("(vector-ref #(1 2) 10000000000000000000001)"))
}

///|
test "flonum numerator denominator integer" {
  let program =
    #|(list (fl=? (flnumerator 4.0) 4.0)
    #|      (fl=? (fldenominator 4.0) 1.0)
    #|      (fl=? (flnumerator -2.0) -2.0))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(#t #t #t)")
}

///|
test "number->string radix small ints" {
  let program =
    #|(list (number->string 255 16)
    #|      (number->string -255 16)
    #|      (number->string 5 2))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="(\"ff\" \"-ff\" \"101\")")
}

///|
test "number->string negative bigint" {
  let value = eval_program("(number->string -12345678901234567890)")
  inspect(@runtime.value_to_string(value), content="\"-12345678901234567890\"")
}

///|
test "eqv identity for runtime values" {
  let program =
    #|(begin
    #|  (define p (make-parameter 1))
    #|  (define prom (delay 1))
    #|  (define port (open-output-string))
    #|  (define env (environment '(rnrs base)))
    #|  (define cont (call/cc (lambda (k) k)))
    #|  (define h (make-eq-hashtable))
    #|  (define colors (make-enumeration '(red)))
    #|  (define enum-ctor (enum-set-constructor colors))
    #|  (define enum-set (enum-ctor '(red)))
    #|  (define rtd (make-record-type-descriptor 'pt #f #f #f #f '#((mutable x))))
    #|  (define rcd (make-record-constructor-descriptor rtd #f #f))
    #|  (define make-pt (record-constructor rcd))
    #|  (define pred (record-predicate rtd))
    #|  (define acc (record-accessor rtd 0))
    #|  (define mut (record-mutator rtd 0))
    #|  (define pt (make-pt 1))
    #|  (define-condition-type &c &condition make-c c? (msg c-msg))
    #|  (define c (make-c "hi"))
    #|  (define cond-pred (condition-predicate (record-rtd c)))
    #|  (define cond-acc
    #|    (condition-accessor
    #|      (record-rtd c)
    #|      (record-accessor (record-rtd c) 0)))
    #|  (define stx #'x)
    #|  (define results
    #|    (list (eqv? p p)
    #|          (eqv? prom prom)
    #|          (eqv? port port)
    #|          (eqv? env env)
    #|          (eqv? cont cont)
    #|          (eqv? h h)
    #|          (eqv? enum-ctor enum-ctor)
    #|          (eqv? enum-set enum-set)
    #|          (eqv? rtd rtd)
    #|          (eqv? rcd rcd)
    #|          (eqv? make-pt make-pt)
    #|          (eqv? pred pred)
    #|          (eqv? acc acc)
    #|          (eqv? mut mut)
    #|          (eqv? pt pt)
    #|          (eqv? cond-pred cond-pred)
    #|          (eqv? cond-acc cond-acc)
    #|          (eqv? stx stx)))
    #|  (not (memv #f results)))
  let value = eval_program(program)
  inspect(@runtime.value_to_string(value), content="#t")
}
```
