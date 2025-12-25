///|
test "r6rs libraries: flonums predicates and conversion" {
  let program = (
    (
      #|(import (rnrs arithmetic flonums))
      #|(list (flonum? 1.0)
      #|      (flonum? 1)
      #|      (= (real->flonum 1/2) 0.5)
      #|      (flonum? (fixnum->flonum 3))
      #|      (flinteger? 2.0)
      #|      (flinteger? 2.5))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #f #t #t #t #f)",
  )
}

///|
test "r6rs libraries: flonums comparisons and predicates" {
  let program = (
    (
      #|(import (rnrs arithmetic flonums))
      #|(list (fl=? 1.0 1.0 1.0)
      #|      (fl<? -1.0 0.0 2.0)
      #|      (fl>? 3.0 2.0 1.0)
      #|      (fl<=? 1.0 1.0 2.0)
      #|      (fl>=? 2.0 2.0 1.0)
      #|      (flzero? 0.0)
      #|      (flpositive? 1.0)
      #|      (flnegative? -1.0)
      #|      (flodd? 3.0)
      #|      (fleven? 4.0)
      #|      (flfinite? 1.0)
      #|      (flinfinite? +inf.0)
      #|      (flnan? +nan.0))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t #t #t #t #t #t #t #t #t #t #t #t)",
  )
}

///|
test "r6rs libraries: flonums arithmetic" {
  let program = (
    (
      #|(import (rnrs arithmetic flonums))
      #|(list (= (fl+ 1.0 2.0 3.0) 6.0)
      #|      (= (fl- 10.0 3.0 2.0) 5.0)
      #|      (= (fl- 5.0) -5.0)
      #|      (= (fl* 2.0 3.0 4.0) 24.0)
      #|      (= (fl/ 8.0 2.0 2.0) 2.0)
      #|      (= (fl/ 2.0) 0.5)
      #|      (= (flmax 1.0 3.0 2.0) 3.0)
      #|      (= (flmin 1.0 3.0 2.0) 1.0)
      #|      (= (flabs -3.0) 3.0))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t #t #t #t #t #t #t #t)",
  )
}

///|
test "r6rs libraries: flonums division and remainder" {
  let program = (
    (
      #|(import (rnrs arithmetic flonums))
      #|(list (call-with-values
      #|        (lambda () (fldiv-and-mod 7.0 3.0))
      #|        (lambda (d m) (list (= d 2.0) (= m 1.0))))
      #|      (call-with-values
      #|        (lambda () (fldiv0-and-mod0 -7.0 3.0))
      #|        (lambda (d m) (list (= d -2.0) (= m -1.0))))
      #|      (= (fldiv -7.0 3.0) -3.0)
      #|      (= (flmod -7.0 3.0) 2.0)
      #|      (= (fldiv0 -7.0 3.0) -2.0)
      #|      (= (flmod0 -7.0 3.0) -1.0))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="((#t #t) (#t #t) #t #t #t #t)",
  )
}

///|
test "r6rs libraries: flonums numerator/denominator and rounding" {
  let program = (
    (
      #|(import (rnrs arithmetic flonums))
      #|(list (fl=? (flnumerator 0.75) 3.0)
      #|      (fl=? (fldenominator 0.75) 4.0)
      #|      (fl=? (flfloor -1.2) -2.0)
      #|      (fl=? (flceiling -1.2) -1.0)
      #|      (fl=? (fltruncate -1.2) -1.0)
      #|      (fl=? (flround 2.5) 2.0)
      #|      (fl=? (flround 3.5) 4.0))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t #t #t #t #t #t)",
  )
}

///|
test "r6rs libraries: flonums elementary functions" {
  let program = (
    (
      #|(import (rnrs arithmetic flonums))
      #|(list (fl=? (flsqrt 4.0) 2.0)
      #|      (fl=? (flexpt 2.0 3.0) 8.0)
      #|      (fl=? (flatan 0.0) 0.0)
      #|      (fl=? (flatan 0.0 1.0) 0.0))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t #t #t)",
  )
}

///|
test "r6rs libraries: flonums condition types" {
  let program = (
    (
      #|(import (rnrs arithmetic flonums))
      #|(list (no-infinities-violation? (make-no-infinities-violation))
      #|      (no-nans-violation? (make-no-nans-violation)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t #t)",
  )
}
