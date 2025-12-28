///|
test "r6rs conditions: simple condition" {
  let program =
    #|(define-condition-type &c &condition make-c c? (x c-x))
    #|(define c1 (make-c 1))
    #|(list (condition? c1) (c? c1) (c-x c1)
    #|      (record? (condition c1)) (condition? (condition c1)))
  inspect(value_to_string(eval_program(program)), content="(#t #t 1 #t #t)")
}

///|
test "r6rs conditions: compound conditions" {
  let program =
    #|(define-condition-type &c &condition make-c c? (x c-x))
    #|(define c1 (make-c 1))
    #|(define c2 (make-c 2))
    #|(define cc (condition c1 c2))
    #|(list (condition? cc) (record? cc) (c? cc) (c-x cc)
    #|      (map c-x (simple-conditions cc)))
  inspect(value_to_string(eval_program(program)), content="(#t #f #t 1 (1 2))")
}

///|
test "r6rs conditions: subtype predicates and accessors" {
  let program =
    #|(define-condition-type &c &condition make-c c? (x c-x))
    #|(define-condition-type &c1 &c make-c1 c1? (y c1-y))
    #|(define c3 (make-c1 10 20))
    #|(define cc (condition c3 (make-c1 30 40)))
    #|(define rtd (record-rtd c3))
    #|(define pred (condition-predicate rtd))
    #|(define acc (condition-accessor rtd (record-accessor rtd 1)))
    #|(list (c? c3) (c1? c3) (c-x c3) (c1-y c3) (pred cc) (acc cc))
  inspect(value_to_string(eval_program(program)), content="(#t #t 10 20 #t 20)")
}
