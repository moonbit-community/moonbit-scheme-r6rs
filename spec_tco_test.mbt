///|
test "r6rs tco: tail recursion" {
  let program =
    #|(letrec ((loop (lambda (n acc)
    #|                  (if (= n 0)
    #|                    acc
    #|                    (loop (- n 1) (+ acc n))))))
    #|  (loop 20000 0))
  inspect(value_to_string(eval_program(program)), content="200010000")
}
