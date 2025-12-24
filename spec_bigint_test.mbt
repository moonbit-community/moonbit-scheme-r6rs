///|
test "r6rs bigints: arithmetic and rationals" {
  let program : String =
    #|(begin
    #|  (define big 4294967296)
    #|  (define bigger 4294967297)
    #|  (list
    #|    (+ big 5)
    #|    (- bigger 6)
    #|    (* big 3)
    #|    (/ big 2)
    #|    (/ 1 big)))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(4294967301 4294967291 12884901888 2147483648 1/4294967296)",
  )
}

///|
test "r6rs bigints: number<->string" {
  let program : String =
    #|(begin
    #|  (define big 4294967296)
    #|  (list
    #|    (number->string big 16)
    #|    (string->number "100000000" 16)
    #|    (number->string (/ 1 big))
    #|    (string->number "1/4294967296")))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(\"100000000\" 4294967296 \"1/4294967296\" 1/4294967296)",
  )
}

///|
test "r6rs bigints: rounding and predicates" {
  let program : String =
    #|(begin
    #|  (define big 4294967297)
    #|  (define half (/ big 2))
    #|  (define neg-half (/ (- big) 2))
    #|  (list
    #|    (floor half)
    #|    (ceiling neg-half)
    #|    (truncate neg-half)
    #|    (round half)
    #|    (odd? big)
    #|    (even? (* big 2))
    #|    (positive? big)
    #|    (negative? (- big))
    #|    (zero? (- big big))
    #|    (finite? big)
    #|    (infinite? big)
    #|    (nan? big)))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(2147483648 -2147483648 -2147483648 2147483648 #t #t #t #t #t #t #f #f)",
  )
}

///|
test "r6rs bigints: expt" {
  let program : String =
    #|(begin
    #|  (list
    #|    (expt 1 4294967296)
    #|    (expt -1 4294967297)
    #|    (expt 4294967296 2)))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(1 -1 18446744073709551616)",
  )
}

///|
test "r6rs bigints: exact-integer-sqrt" {
  let program : String =
    #|(begin
    #|  (call-with-values
    #|    (lambda () (exact-integer-sqrt 18446744073709551616))
    #|    (lambda (s r) (list s r))))
    #|
  inspect(
    value_to_string(eval_program(program)),
    content="(4294967296 0)",
  )
}
