///|
test "r6rs libraries: hashtables" {
  let program =
    #|(import (rnrs hashtables))
    #|(let ((h (make-eqv-hashtable)))
    #|  (hashtable-set! h 'a 1)
    #|  (hashtable-ref h 'a #f))
  inspect(value_to_string(eval_program(program)), content="1")
}

///|
test "r6rs libraries: hash helpers" {
  let program =
    #|(import (rnrs hashtables))
    #|(let* ((a (string-hash "ab"))
    #|       (b (string-hash "ab"))
    #|       (ci1 (string-ci-hash "Ab"))
    #|       (ci2 (string-ci-hash "aB"))
    #|       (sym (symbol-hash 'ab))
    #|       (eq1 (equal-hash '(1 2)))
    #|       (eq2 (equal-hash (list 1 2))))
    #|  (list (= a b)
    #|        (= ci1 ci2)
    #|        (= sym (string-hash "ab"))
    #|        (= eq1 eq2)
    #|        (exact-integer? a)
    #|        (>= a 0)))
  inspect(value_to_string(eval_program(program)), content="(#t #t #t #t #t #t)")
}
