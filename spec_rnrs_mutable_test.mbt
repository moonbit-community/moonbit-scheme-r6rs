///|
test "r6rs libraries: mutable-pairs" {
  let program =
    #|(import (rnrs mutable-pairs))
    #|(let ((p (cons 1 2)))
    #|  (set-car! p 3)
    #|  (set-cdr! p 4)
    #|  p)
  inspect(value_to_string(eval_program(program)), content="(3 . 4)")
}

///|
test "r6rs libraries: mutable-strings" {
  let program =
    #|(import (rnrs mutable-strings))
    #|(let ((s (string-copy "hello")))
    #|  (string-set! s 0 #\H)
    #|  (string-copy! s 1 "xy" 0 2)
    #|  (string-fill! s #\! 4 5)
    #|  s)
  inspect(value_to_string(eval_program(program)), content="\"Hxyl!\"")
}
