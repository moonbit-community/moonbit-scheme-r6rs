///|
test "r6rs syntax-case: basic pattern" {
  let program = (
    (
      #|(begin
      #|  (define-syntax when
      #|    (lambda (stx)
      #|      (syntax-case stx ()
      #|        ((when test body ...)
      #|         (if test (begin body ...) (begin))))))
      #|  (when #t 1 2 3))
    )
  )
  inspect(value_to_string(eval_program(program)), content="3")
}
