///|
test "r6rs libraries: fixnums predicates and limits" {
  let program =
    #|(import (rnrs arithmetic fixnums))
    #|(let ((w (fixnum-width))
    #|      (lo (least-fixnum))
    #|      (hi (greatest-fixnum)))
    #|  (list (fixnum? 0)
    #|        (fixnum? lo)
    #|        (fixnum? hi)
    #|        (> w 0)
    #|        (< lo 0)
    #|        (> hi 0)
    #|        (not (fixnum? (string->number "1000000000000000000000000000000000000000")))))
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t #t #t #t #t #t)",
  )
}

///|
test "r6rs libraries: fixnum comparisons and predicates" {
  let program =
    #|(import (rnrs arithmetic fixnums))
    #|(list (fx=? 1 1 1)
    #|      (fx<? -1 0 2)
    #|      (fx>? 3 2 1)
    #|      (fx<=? 1 1 2)
    #|      (fx>=? 2 2 1)
    #|      (fxzero? 0)
    #|      (fxpositive? 1)
    #|      (fxnegative? -1)
    #|      (fxodd? 3)
    #|      (fxeven? 4))
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t #t #t #t #t #t #t #t #t)",
  )
}

///|
test "r6rs libraries: fixnum arithmetic" {
  let program =
    #|(import (rnrs arithmetic fixnums))
    #|(list (fx+ 1 2 3)
    #|      (fx- 10 3 2)
    #|      (fx- 5)
    #|      (fx* 2 3 4)
    #|      (fxmin 5 2 9)
    #|      (fxmax 5 2 9))
  inspect(value_to_string(eval_program(program)), content="(6 5 -5 24 2 9)")
}

///|
test "r6rs libraries: fixnum division" {
  let program =
    #|(import (rnrs arithmetic fixnums))
    #|(list (fxdiv 7 3)
    #|      (fxmod 7 3)
    #|      (fxdiv -7 3)
    #|      (fxmod -7 3)
    #|      (fxdiv0 -7 3)
    #|      (fxmod0 -7 3))
  inspect(value_to_string(eval_program(program)), content="(2 1 -3 2 -2 -1)")
}

///|
test "r6rs libraries: fixnum carry ops" {
  let program =
    #|(import (rnrs arithmetic fixnums))
    #|(list (call-with-values (lambda () (fx+/carry 1 2 0)) list)
    #|      (call-with-values (lambda () (fx-/carry 5 3 0)) list)
    #|      (call-with-values (lambda () (fx*/carry 2 3 0)) list)
    #|      (call-with-values
    #|        (lambda () (fx+/carry (greatest-fixnum) 1 0))
    #|        (lambda (s c) (list (= s (least-fixnum)) (= c 1))))
    #|      (call-with-values
    #|        (lambda () (fx-/carry (least-fixnum) 1 0))
    #|        (lambda (s c) (list (= s (greatest-fixnum)) (= c -1)))))
  inspect(
    value_to_string(eval_program(program)),
    content="((3 0) (2 0) (6 0) (#t #t) (#t #t))",
  )
}

///|
test "r6rs libraries: fixnum bitwise ops" {
  let program =
    #|(import (rnrs arithmetic fixnums))
    #|(list (fxand 12 10)
    #|      (fxior 12 10)
    #|      (fxxor 12 10)
    #|      (fxnot 0)
    #|      (fxif 12 10 5))
  inspect(value_to_string(eval_program(program)), content="(8 14 6 -1 9)")
}

///|
test "r6rs libraries: fixnum bit fields and shifts" {
  let program =
    #|(import (rnrs arithmetic fixnums))
    #|(list (fxbit-count 11)
    #|      (fxlength 11)
    #|      (fxfirst-bit-set 8)
    #|      (fxbit-set? 10 1)
    #|      (fxcopy-bit 8 0 1)
    #|      (fxbit-field 240 4 8)
    #|      (fxcopy-bit-field 0 0 4 10)
    #|      (fxrotate-bit-field 9 0 4 1)
    #|      (fxreverse-bit-field 12 0 4)
    #|      (fxarithmetic-shift 1 3)
    #|      (fxarithmetic-shift-left 1 2)
    #|      (fxarithmetic-shift-right -8 1))
  inspect(
    value_to_string(eval_program(program)),
    content="(3 4 3 #t 9 15 10 3 3 8 4 -4)",
  )
}

///|
test "r6rs libraries: fixnum errors" {
  let overflow = try? eval_program(
    (
      #|(import (rnrs arithmetic fixnums))
      #|(fx+ (greatest-fixnum) 1)
    ),
  )
  match overflow {
    Err(err) => inspect(err, content="EvalError(\"fixnum overflow\")")
    _ => fail("expected fixnum overflow")
  }
  let bad_type = try? eval_program(
    (
      #|(import (rnrs arithmetic fixnums))
      #|(fx+ 1/2 1)
    ),
  )
  match bad_type {
    Err(err) =>
      inspect(err, content="EvalError(\"type error: fixnum expected\")")
    _ => fail("expected fixnum type error")
  }
}
