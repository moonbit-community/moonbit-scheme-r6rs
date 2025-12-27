///|
test "r6rs macro: ellipsis in templates" {
  let program = (
    (
      #|(define-syntax when
      #|  (syntax-rules ()
      #|    ((when test body ...)
      #|     (if test (begin body ...) #f))))
      #|(let ((x 0))
      #|  (when #t (set! x 1) (set! x (+ x 1)))
      #|  x)
    )
  )
  inspect(value_to_string(eval_program(program)), content="2")

  let program2 = (
    (
      #|(define-syntax when
      #|  (syntax-rules ()
      #|    ((when test body ...)
      #|     (if test (begin body ...) #f))))
      #|(let ((x 0))
      #|  (when #f (set! x 1))
      #|  x)
    )
  )
  inspect(value_to_string(eval_program(program2)), content="0")
}

///|
test "r6rs macro: ellipsis with list patterns" {
  let program = (
    (
      #|(define-syntax pairs
      #|  (syntax-rules ()
      #|    ((pairs (a b) ...) (list (list a b) ...))))
      #|(pairs (1 2) (3 4))
    )
  )
  inspect(value_to_string(eval_program(program)), content="((1 2) (3 4))")
}

///|
test "r6rs macro: custom ellipsis identifier" {
  let program = (
    (
      #|(define-syntax collect
      #|  (syntax-rules ::: ()
      #|    ((collect x :::) (list x :::))))
      #|(collect 1 2 3)
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 3)")
}

///|
test "r6rs macro: custom ellipsis leaves ... as literal" {
  let program = (
    (
      #|(define-syntax dot
      #|  (syntax-rules ::: (... )
      #|    ((dot ...) 'ok)
      #|    ((dot x) x)))
      #|(dot ...)
    )
  )
  inspect(value_to_string(eval_program(program)), content="ok")
}

///|
test "r6rs macro: ellipsis in middle of pattern" {
  let program = (
    (
      #|(define-syntax tail
      #|  (syntax-rules ()
      #|    ((tail a ... b) (list a ... b))))
      #|(list (tail 1 2 3) (tail 1))
    )
  )
  inspect(value_to_string(eval_program(program)), content="((1 2 3) (1))")
}

///|
test "r6rs macro: ellipsis enforces repeated bindings" {
  let program = (
    (
      #|(define-syntax same-seq
      #|  (syntax-rules ()
      #|    ((_ (x ...) (x ...)) 'same)
      #|    ((_ (x ...) (y ...)) 'diff)))
      #|(list (same-seq (1 2) (1 2))
      #|      (same-seq (1 2) (1 3)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(same diff)")
}
