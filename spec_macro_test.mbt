///|
test "r6rs macro: define-syntax basics" {
  let program =
    "(define-syntax inc (syntax-rules () ((inc x) (set! x (+ x 1))))) (let ((x 1)) (inc x) x)"
  inspect(value_to_string(eval_program(program)), content="2")

  let program2 =
    "(define-syntax swap! (syntax-rules () ((swap! a b) (let ((tmp a)) (set! a b) (set! b tmp))))) (let ((x 1) (y 2)) (swap! x y) (list x y))"
  inspect(value_to_string(eval_program(program2)), content="(2 1)")
}

///|
test "r6rs macro: syntax-rules literals" {
  let program =
    "(define-syntax my-if (syntax-rules (then else) ((my-if test then a else b) (if test a b)))) (my-if #t then 1 else 2)"
  inspect(value_to_string(eval_program(program)), content="1")

  let program2 =
    "(define-syntax my-if (syntax-rules (then else) ((my-if test then a else b) (if test a b)))) (my-if #f then 1 else 2)"
  inspect(value_to_string(eval_program(program2)), content="2")
}
