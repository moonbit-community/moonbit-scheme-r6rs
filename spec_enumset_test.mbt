///|
test "r6rs enum-sets: define-enumeration and membership" {
  let program =
    #|(define-enumeration color (red green blue) color-set)
    #|(define c1 (color-set '(red blue)))
    #|(list (enum-set-member? 'red c1)
    #|      (enum-set-member? 'green c1)
    #|      (enum-set-subset? c1 color))
  inspect(value_to_string(eval_program(program)), content="(#t #f #t)")
}

///|
test "r6rs enum-sets: union/intersection/difference/complement" {
  let program =
    #|(define-enumeration fruit (apple banana cherry) fruit-set)
    #|(define f1 (fruit-set '(apple)))
    #|(define f2 (fruit-set '(banana cherry)))
    #|(define f3 (enum-set-union f1 f2))
    #|(list (enum-set->list f3)
    #|      (enum-set->list (enum-set-intersection f3 f1))
    #|      (enum-set->list (enum-set-difference f3 f1))
    #|      (enum-set->list (enum-set-complement f1)))
  inspect(
    value_to_string(eval_program(program)),
    content="((apple banana cherry) (apple) (banana cherry) (banana cherry))",
  )
}

///|
test "r6rs enum-sets: make-enumeration and indexer" {
  let program =
    #|(define colors (make-enumeration '(red green blue)))
    #|(define idx (enum-set-indexer colors))
    #|(list (enum-set->list colors) (idx 'red) (idx 'cyan))
  inspect(
    value_to_string(eval_program(program)),
    content="((red green blue) 0 #f)",
  )
}

///|
test "r6rs enum-sets: projection" {
  let program =
    #|(define-enumeration big (a b c d) big-set)
    #|(define-enumeration small (b d) small-set)
    #|(define bs (big-set '(a d)))
    #|(define proj (enum-set-projection bs small))
    #|(enum-set->list proj)
  inspect(value_to_string(eval_program(program)), content="(d)")
}

///|
test "r6rs enum-sets: universe mismatch error" {
  let program =
    #|(define a (make-enumeration '(a b)))
    #|(define b (make-enumeration '(c d)))
    #|(define sa ((enum-set-constructor a) '(a)))
    #|(define sb ((enum-set-constructor b) '(c)))
    #|(enum-set-union sa sb)
  let result = try? eval_program(program)
  inspect(result is Err(_), content="true")
}

///|
test "r6rs enum-sets: duplicate symbol error" {
  let result = try? eval_program("(make-enumeration '(a a))")
  inspect(result is Err(_), content="true")
}
