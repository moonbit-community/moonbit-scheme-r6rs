# eval

Evaluator and macro expander for Scheme R6RS.

## Entry points

- eval_program and eval_program_all evaluate full source strings
- register_include_source supplies in-memory (include ...) content

## Example

```mbt
///|
let program = "(define x 1) (+ x 2)"

///|
let result = @eval.eval_program(program)
```

```mbt check
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
  let result = try? eval_program("(car 1)")
  inspect(result is Err(_), content="true")
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
  let set_value =
    eval_program(
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
  let copy_bang_full =
    eval_program(
      "(let ((dst (make-bytevector 4 0)) (src #vu8(1 2 3))) (bytevector-copy! dst 0 src) dst)",
    )
  inspect(@runtime.value_to_string(copy_bang_full), content="#vu8(1 2 3 0)")
  let copy_bang_start =
    eval_program(
      "(let ((dst (make-bytevector 4 0)) (src #vu8(1 2 3))) (bytevector-copy! dst 0 src 1) dst)",
    )
  inspect(@runtime.value_to_string(copy_bang_start), content="#vu8(2 3 0 0)")
  let copy_bang_slice =
    eval_program(
      "(let ((dst (make-bytevector 4 0)) (src #vu8(1 2 3))) (bytevector-copy! dst 1 src 0 2) dst)",
    )
  inspect(@runtime.value_to_string(copy_bang_slice), content="#vu8(0 1 2 0)")
  let fill =
    eval_program("(let ((bv #vu8(1 2 3))) (bytevector-fill! bv 7 1 3) bv)")
  inspect(@runtime.value_to_string(fill), content="#vu8(1 7 7)")
  let fill_start =
    eval_program("(let ((bv #vu8(1 2 3))) (bytevector-fill! bv 9 1) bv)")
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
  let pred_err = try? eval_program("(bytevector?)")
  inspect(pred_err is Err(_), content="true")
  let len_err = try? eval_program("(bytevector-length)")
  inspect(len_err is Err(_), content="true")
  let ref_err = try? eval_program("(bytevector-u8-ref #vu8(1))")
  inspect(ref_err is Err(_), content="true")
  let set_err = try? eval_program("(bytevector-u8-set! #vu8(1) 0)")
  inspect(set_err is Err(_), content="true")
  let copy_err = try? eval_program("(bytevector-copy)")
  inspect(copy_err is Err(_), content="true")
  let copy_bang_err = try? eval_program("(bytevector-copy! #vu8(1) 0)")
  inspect(copy_bang_err is Err(_), content="true")
  let fill_err = try? eval_program("(bytevector-fill! #vu8(1))")
  inspect(fill_err is Err(_), content="true")
  let list_err = try? eval_program("(bytevector->u8-list)")
  inspect(list_err is Err(_), content="true")
  let from_list_err = try? eval_program("(u8-list->bytevector)")
  inspect(from_list_err is Err(_), content="true")
  let from_list_type_err = try? eval_program("(u8-list->bytevector '(1 a))")
  inspect(from_list_type_err is Err(_), content="true")
  let utf8_err = try? eval_program("(string->utf8)")
  inspect(utf8_err is Err(_), content="true")
  let utf8_to_string_err = try? eval_program("(utf8->string)")
  inspect(utf8_to_string_err is Err(_), content="true")
  let endian_err = try? eval_program("(native-endianness 1)")
  inspect(endian_err is Err(_), content="true")
  let uint_ref_err = try? eval_program("(bytevector-uint-ref #vu8(1) 0)")
  inspect(uint_ref_err is Err(_), content="true")
  let sint_ref_err = try? eval_program("(bytevector-sint-ref #vu8(1) 0)")
  inspect(sint_ref_err is Err(_), content="true")
  let uint_set_err = try? eval_program("(bytevector-uint-set! #vu8(1) 0 0 1)")
  inspect(uint_set_err is Err(_), content="true")
  let sint_set_err = try? eval_program("(bytevector-sint-set! #vu8(1) 0 0 1)")
  inspect(sint_set_err is Err(_), content="true")
}

///|
test "char and string primitives" {
  let char_ci =
    eval_program(
      "(list (char-ci>? #\\b #\\A) (char-ci<=? #\\a #\\A) (char-ci>=? #\\A #\\b))",
    )
  inspect(@runtime.value_to_string(char_ci), content="(#t #t #f)")
  let char_preds =
    eval_program(
      "(list (char? #\\a) (char? 1) (char-alphabetic? #\\a) (char-numeric? #\\9) (char-whitespace? #\\space) (char-upper-case? #\\A) (char-lower-case? #\\a))",
    )
  inspect(@runtime.value_to_string(char_preds), content="(#t #f #t #t #t #t #t)")
  let char_cases =
    eval_program(
      "(list (char->integer (char-upcase #\\a)) (char->integer (char-downcase #\\A)) (char->integer (char-foldcase #\\A)) (char->integer (integer->char 66)))",
    )
  inspect(@runtime.value_to_string(char_cases), content="(65 97 97 66)")
  let string_ci =
    eval_program(
      "(list (string-ci>? \"b\" \"A\") (string-ci<=? \"a\" \"A\") (string-ci>=? \"A\" \"b\"))",
    )
  inspect(@runtime.value_to_string(string_ci), content="(#t #t #f)")
  let string_basic =
    eval_program(
      "(let ((s (string-copy \"hi\"))) (string-set! s 0 #\\H) (list (string? s) (string? 1) (string-length s) (string-append \"a\" \"b\") (char->integer (string-ref s 1)) s))",
    )
  inspect(@runtime.value_to_string(string_basic), content="(#t #f 2 \"ab\" 105 \"Hi\")")
}

///|
test "char and string arity errors" {
  let char_p_err = try? eval_program("(char?)")
  inspect(char_p_err is Err(_), content="true")
  let char_to_int_err = try? eval_program("(char->integer)")
  inspect(char_to_int_err is Err(_), content="true")
  let int_to_char_err = try? eval_program("(integer->char)")
  inspect(int_to_char_err is Err(_), content="true")
  let alphabetic_err = try? eval_program("(char-alphabetic?)")
  inspect(alphabetic_err is Err(_), content="true")
  let numeric_err = try? eval_program("(char-numeric?)")
  inspect(numeric_err is Err(_), content="true")
  let whitespace_err = try? eval_program("(char-whitespace?)")
  inspect(whitespace_err is Err(_), content="true")
  let upper_err = try? eval_program("(char-upper-case?)")
  inspect(upper_err is Err(_), content="true")
  let lower_err = try? eval_program("(char-lower-case?)")
  inspect(lower_err is Err(_), content="true")
  let upcase_err = try? eval_program("(char-upcase)")
  inspect(upcase_err is Err(_), content="true")
  let downcase_err = try? eval_program("(char-downcase)")
  inspect(downcase_err is Err(_), content="true")
  let foldcase_err = try? eval_program("(char-foldcase)")
  inspect(foldcase_err is Err(_), content="true")
  let category_err = try? eval_program("(char-general-category)")
  inspect(category_err is Err(_), content="true")
  let make_string_err = try? eval_program("(make-string)")
  inspect(make_string_err is Err(_), content="true")
  let string_length_err = try? eval_program("(string-length)")
  inspect(string_length_err is Err(_), content="true")
  let string_ref_err = try? eval_program("(string-ref \"a\")")
  inspect(string_ref_err is Err(_), content="true")
  let string_set_err = try? eval_program("(string-set! \"a\" 0)")
  inspect(string_set_err is Err(_), content="true")
  let string_copy_err = try? eval_program("(string-copy \"a\" 0 1 2)")
  inspect(string_copy_err is Err(_), content="true")
  let substring_err = try? eval_program("(substring \"a\" 0)")
  inspect(substring_err is Err(_), content="true")
}

///|
test "pair and list primitives" {
  let pair =
    eval_program("(let ((p (cons 1 2))) (set-car! p 3) (set-cdr! p 4) p)")
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
  let cons_err = try? eval_program("(cons 1)")
  inspect(cons_err is Err(_), content="true")
  let set_car_err = try? eval_program("(set-car! (cons 1 2))")
  inspect(set_car_err is Err(_), content="true")
  let set_cdr_err = try? eval_program("(set-cdr! (cons 1 2))")
  inspect(set_cdr_err is Err(_), content="true")
  let car_err = try? eval_program("(car)")
  inspect(car_err is Err(_), content="true")
  let cdr_err = try? eval_program("(cdr)")
  inspect(cdr_err is Err(_), content="true")
  let cxr_err = try? eval_program("(cadr)")
  inspect(cxr_err is Err(_), content="true")
  let cadr_err = try? eval_program("(cadr 1)")
  inspect(cadr_err is Err(_), content="true")
  let make_list_err = try? eval_program("(make-list)")
  inspect(make_list_err is Err(_), content="true")
  let make_list_err2 = try? eval_program("(make-list 1 2 3)")
  inspect(make_list_err2 is Err(_), content="true")
  let null_err = try? eval_program("(null?)")
  inspect(null_err is Err(_), content="true")
  let pair_err = try? eval_program("(pair?)")
  inspect(pair_err is Err(_), content="true")
  let list_false = eval_program("(list? (lambda (x) x))")
  inspect(@runtime.value_to_string(list_false), content="#f")
  let list_err = try? eval_program("(list?)")
  inspect(list_err is Err(_), content="true")
  let length_err = try? eval_program("(length (lambda (x) x))")
  inspect(length_err is Err(_), content="true")
  let length_arity = try? eval_program("(length)")
  inspect(length_arity is Err(_), content="true")
  let append_err = try? eval_program("(append (lambda (x) x) '(1))")
  inspect(append_err is Err(_), content="true")
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
  let vec_set =
    eval_program("(let ((v (vector 1 2 3))) (vector-set! v 1 9) v)")
  inspect(@runtime.value_to_string(vec_set), content="#(1 9 3)")
  let vec_fill =
    eval_program("(let ((v (vector 1 2 3))) (vector-fill! v 9 1) v)")
  inspect(@runtime.value_to_string(vec_fill), content="#(1 9 9)")
  let vec_copy = eval_program("(vector-copy #(1 2 3) 1)")
  inspect(@runtime.value_to_string(vec_copy), content="#(2 3)")
  let vec_copy_bang =
    eval_program(
      "(let ((dst (make-vector 4 0)) (src #(1 2 3))) (vector-copy! dst 0 src) dst)",
    )
  inspect(@runtime.value_to_string(vec_copy_bang), content="#(1 2 3 0)")
  let vec_copy_bang_start =
    eval_program(
      "(let ((dst (make-vector 4 0)) (src #(1 2 3))) (vector-copy! dst 0 src 1) dst)",
    )
  inspect(@runtime.value_to_string(vec_copy_bang_start), content="#(2 3 0 0)")
  let vec_copy_bang_slice =
    eval_program(
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
  let vec_p_err = try? eval_program("(vector?)")
  inspect(vec_p_err is Err(_), content="true")
  let vec_len_err = try? eval_program("(vector-length)")
  inspect(vec_len_err is Err(_), content="true")
  let vec_ref_err = try? eval_program("(vector-ref #(1))")
  inspect(vec_ref_err is Err(_), content="true")
  let vec_set_err = try? eval_program("(vector-set! #(1) 0)")
  inspect(vec_set_err is Err(_), content="true")
  let vec_fill_err = try? eval_program("(vector-fill! #(1) 0 1 2 3)")
  inspect(vec_fill_err is Err(_), content="true")
  let vec_copy_err = try? eval_program("(vector-copy)")
  inspect(vec_copy_err is Err(_), content="true")
  let vec_copy_bang_err = try? eval_program("(vector-copy! #(1) 0)")
  inspect(vec_copy_bang_err is Err(_), content="true")
  let vec_to_list_err = try? eval_program("(vector->list)")
  inspect(vec_to_list_err is Err(_), content="true")
  let list_to_vec_err = try? eval_program("(list->vector)")
  inspect(list_to_vec_err is Err(_), content="true")
  let make_vec_err = try? eval_program("(make-vector)")
  inspect(make_vec_err is Err(_), content="true")
}

///|
test "symbol and predicate basics" {
  let preds =
    eval_program(
      "(list (symbol? 'a) (symbol? 1) (identifier? #'a) (identifier? 1) (syntax? #'a) (syntax? 'a) (boolean? #t) (boolean? 1) (number? 1) (number? 'a) (integer? 1) (integer? 1.2) (exact-integer? 1) (exact-integer? 1.2) (rational? 1/2) (rational? 1+2i) (real? 1.0) (real? 1+2i) (complex? 1.0) (complex? 1+2i))",
    )
  inspect(
    @runtime.value_to_string(preds),
    content="(#t #f #t #f #t #f #t #f #t #f #t #f #t #f #t #f #t #f #t #t)",
  )
  let non_datum =
    eval_program(
      "(let ((f (lambda (x) x))) (list (boolean? f) (number? f) (integer? f) (exact-integer? f) (rational? f) (real? f) (complex? f)))",
    )
  inspect(@runtime.value_to_string(non_datum), content="(#f #f #f #f #f #f #f)")
  let hashes =
    eval_program(
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
  let eq_err = try? eval_program("(eq? 1)")
  inspect(eq_err is Err(_), content="true")
  let eqv_err = try? eval_program("(eqv? 1)")
  inspect(eqv_err is Err(_), content="true")
  let equal_err = try? eval_program("(equal? 1)")
  inspect(equal_err is Err(_), content="true")
  let symbol_p_err = try? eval_program("(symbol?)")
  inspect(symbol_p_err is Err(_), content="true")
  let identifier_err = try? eval_program("(identifier?)")
  inspect(identifier_err is Err(_), content="true")
  let syntax_err = try? eval_program("(syntax?)")
  inspect(syntax_err is Err(_), content="true")
  let free_id_err = try? eval_program("(free-identifier=? #'a)")
  inspect(free_id_err is Err(_), content="true")
  let bound_id_err = try? eval_program("(bound-identifier=? #'a)")
  inspect(bound_id_err is Err(_), content="true")
  let sym_string_err = try? eval_program("(symbol->string 1)")
  inspect(sym_string_err is Err(_), content="true")
  let sym_string_arity = try? eval_program("(symbol->string)")
  inspect(sym_string_arity is Err(_), content="true")
  let sym_string_arity2 = try? eval_program("(symbol->string 'a 'b)")
  inspect(sym_string_arity2 is Err(_), content="true")
  let string_sym_err = try? eval_program("(string->symbol 1)")
  inspect(string_sym_err is Err(_), content="true")
  let string_sym_arity = try? eval_program("(string->symbol)")
  inspect(string_sym_arity is Err(_), content="true")
  let string_sym_arity2 = try? eval_program("(string->symbol \"a\" \"b\")")
  inspect(string_sym_arity2 is Err(_), content="true")
  let string_hash_err = try? eval_program("(string-hash)")
  inspect(string_hash_err is Err(_), content="true")
  let string_ci_hash_err = try? eval_program("(string-ci-hash)")
  inspect(string_ci_hash_err is Err(_), content="true")
  let symbol_hash_err = try? eval_program("(symbol-hash)")
  inspect(symbol_hash_err is Err(_), content="true")
  let equal_hash_err = try? eval_program("(equal-hash)")
  inspect(equal_hash_err is Err(_), content="true")
  let syntax_to_datum_err = try? eval_program("(syntax->datum (lambda (x) x))")
  inspect(syntax_to_datum_err is Err(_), content="true")
  let syntax_to_datum_arity = try? eval_program("(syntax->datum)")
  inspect(syntax_to_datum_arity is Err(_), content="true")
  let syntax_to_datum_arity2 = try? eval_program("(syntax->datum #'x #'y)")
  inspect(syntax_to_datum_arity2 is Err(_), content="true")
  let datum_to_syntax_err = try? eval_program("(datum->syntax #'x (lambda (x) x))")
  inspect(datum_to_syntax_err is Err(_), content="true")
  let datum_to_syntax_arity = try? eval_program("(datum->syntax #'x)")
  inspect(datum_to_syntax_arity is Err(_), content="true")
  let boolean_err = try? eval_program("(boolean?)")
  inspect(boolean_err is Err(_), content="true")
  let number_err = try? eval_program("(number?)")
  inspect(number_err is Err(_), content="true")
}

///|
test "numeric predicate edges" {
  let exact_complex = eval_program("(exact? (make-rectangular 1 1000000000000))")
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
test "numeric predicate errors" {
  let integer_err = try? eval_program("(integer?)")
  inspect(integer_err is Err(_), content="true")
  let exact_integer_err = try? eval_program("(exact-integer?)")
  inspect(exact_integer_err is Err(_), content="true")
  let rational_err = try? eval_program("(rational?)")
  inspect(rational_err is Err(_), content="true")
  let real_err = try? eval_program("(real?)")
  inspect(real_err is Err(_), content="true")
  let complex_err = try? eval_program("(complex?)")
  inspect(complex_err is Err(_), content="true")
  let exact_err = try? eval_program("(exact?)")
  inspect(exact_err is Err(_), content="true")
  let inexact_err = try? eval_program("(inexact?)")
  inspect(inexact_err is Err(_), content="true")
  let zero_err = try? eval_program("(zero?)")
  inspect(zero_err is Err(_), content="true")
  let positive_err = try? eval_program("(positive?)")
  inspect(positive_err is Err(_), content="true")
  let negative_err = try? eval_program("(negative?)")
  inspect(negative_err is Err(_), content="true")
  let odd_err = try? eval_program("(odd?)")
  inspect(odd_err is Err(_), content="true")
  let odd_float_err = try? eval_program("(odd? 1.2)")
  inspect(odd_float_err is Err(_), content="true")
  let odd_complex_err = try? eval_program("(odd? (make-rectangular 1 1))")
  inspect(odd_complex_err is Err(_), content="true")
  let odd_real_err = try? eval_program("(odd? (make-rectangular 1.2 0))")
  inspect(odd_real_err is Err(_), content="true")
  let even_err = try? eval_program("(even?)")
  inspect(even_err is Err(_), content="true")
  let even_float_err = try? eval_program("(even? 1.2)")
  inspect(even_float_err is Err(_), content="true")
  let even_complex_err = try? eval_program("(even? (make-rectangular 1 1))")
  inspect(even_complex_err is Err(_), content="true")
  let even_real_err = try? eval_program("(even? (make-rectangular 1.2 0))")
  inspect(even_real_err is Err(_), content="true")
  let finite_err = try? eval_program("(finite?)")
  inspect(finite_err is Err(_), content="true")
  let finite_complex_err = try? eval_program("(finite? (make-rectangular 1 1))")
  inspect(finite_complex_err is Err(_), content="true")
  let infinite_err = try? eval_program("(infinite?)")
  inspect(infinite_err is Err(_), content="true")
  let infinite_complex_err = try? eval_program("(infinite? (make-rectangular 1 1))")
  inspect(infinite_complex_err is Err(_), content="true")
  let infinite_type_err = try? eval_program("(infinite? 'a)")
  inspect(infinite_type_err is Err(_), content="true")
  let nan_err = try? eval_program("(nan?)")
  inspect(nan_err is Err(_), content="true")
  let nan_complex_err = try? eval_program("(nan? (make-rectangular 1 1))")
  inspect(nan_complex_err is Err(_), content="true")
  let nan_type_err = try? eval_program("(nan? 'a)")
  inspect(nan_type_err is Err(_), content="true")
  let procedure_err = try? eval_program("(procedure?)")
  inspect(procedure_err is Err(_), content="true")
}

///|
test "syntax-rules macro" {
  let program =
    "(begin (define-syntax add2 (syntax-rules () ((add2 x) (+ x 2)))) (add2 3))"
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
test "primitive output helpers" {
  let value = eval_program(
    "(begin (write 1) (newline) (get-output-string (current-output-port)))",
  )
  inspect(@runtime.value_to_string(value), content="\"1\\n\"")
}

///|
test "primitive arity errors" {
  let display_err = try? eval_program("(display 1 2 3)")
  inspect(display_err is Err(_), content="true")
  let write_err = try? eval_program("(write)")
  inspect(write_err is Err(_), content="true")
  let newline_err = try? eval_program("(newline 1 2)")
  inspect(newline_err is Err(_), content="true")
  let open_out_err = try? eval_program("(open-output-string 1)")
  inspect(open_out_err is Err(_), content="true")
  let get_out_err = try? eval_program("(get-output-string)")
  inspect(get_out_err is Err(_), content="true")
  let current_err = try? eval_program("(current-output-port 1)")
  inspect(current_err is Err(_), content="true")
  let not_err = try? eval_program("(not)")
  inspect(not_err is Err(_), content="true")
}

///|
test "syntax helper errors" {
  let var_err = try? eval_program("(make-variable-transformer 1)")
  inspect(var_err is Err(_), content="true")
  let temp_err = try? eval_program("(generate-temporaries '(1))")
  inspect(temp_err is Err(_), content="true")
  let syntax_err = try? eval_program("(generate-temporaries (list #'(1)))")
  inspect(syntax_err is Err(_), content="true")
}
```
