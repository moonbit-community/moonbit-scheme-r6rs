///|
test "r6rs libraries: conditions basics" {
  let program = (
    (
      #|(import (rnrs conditions))
      #|(define c (make-message-condition "hi"))
      #|(list (message-condition? c) (condition-message c))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t \"hi\")")
}

///|
test "r6rs libraries: conditions compound" {
  let program = (
    (
      #|(import (rnrs conditions))
      #|(define c1 (make-message-condition "a"))
      #|(define c2 (make-warning))
      #|(define cc (condition c1 c2))
      #|(define parts (simple-conditions cc))
      #|(list (condition? cc)
      #|      (message-condition? (car parts))
      #|      (condition-message (car parts))
      #|      (warning? (car (cdr parts))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t #t \"a\" #t)")
}

///|
test "r6rs libraries: define-condition-type" {
  let program = (
    (
      #|(import (rnrs conditions))
      #|(define-condition-type &c &condition make-c c? (x c-x))
      #|(define c (make-c 42))
      #|(list (c? c) (c-x c))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t 42)")
}

///|
test "r6rs libraries: syntax violations" {
  let program = (
    (
      #|(import (rnrs conditions))
      #|(define s (make-syntax-violation 'form 'subform))
      #|(list (syntax-violation? s)
      #|      (syntax-violation-form s)
      #|      (syntax-violation-subform s))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(#t form subform)")
}
