///|
test "rnrs lists: find and find-tail" {
  let program =
    #|(import (rnrs lists))
    #|(list
    #|  (find odd? '(2 4 5 6))
    #|  (find-tail odd? '(2 4 5 6)))
  inspect(value_to_string(eval_program(program)), content="(5 (5 6))")
}

///|
test "rnrs lists: for-all and exists" {
  let program =
    #|(import (rnrs lists))
    #|(list
    #|  (for-all < '(1 2 3) '(2 3 4))
    #|  (exists (lambda (x) (and (> x 2) x)) '(1 2 3 4)))
  inspect(value_to_string(eval_program(program)), content="(#t 3)")
}

///|
test "rnrs lists: remq remv memp assp" {
  let program =
    #|(import (rnrs lists))
    #|(list
    #|  (remq 'a '(a b a c))
    #|  (remv 2 '(1 2 3 2))
    #|  (memp odd? '(2 4 5 6))
    #|  (assp (lambda (x) (= x 2)) '((1 . a) (2 . b) (3 . c))))
  inspect(
    value_to_string(eval_program(program)),
    content="((b c) (1 3) (5 6) (2 . b))",
  )
}

///|
test "rnrs lists: cons*" {
  let program =
    #|(import (rnrs lists))
    #|(list
    #|  (cons* 1 2 '(3 4))
    #|  (cons* 1 2 3))
  inspect(
    value_to_string(eval_program(program)),
    content="((1 2 3 4) (1 2 . 3))",
  )
}
