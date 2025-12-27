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
test "r6rs macro: literal ellipsis pattern (syntax-rules)" {
  let program = (
    (
      #|(define-syntax ellipsis-lit
      #|  (syntax-rules (...)
      #|    ((_ x ...) 'literal)
      #|    ((_ x y) 'pair)
      #|    ((_ x) 'single)))
      #|(list (ellipsis-lit 1 ...) (ellipsis-lit 1 2) (ellipsis-lit 1))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(literal pair single)")
}

///|
test "r6rs macro: literal ellipsis template (syntax-rules)" {
  let program = (
    (
      #|(define-syntax show
      #|  (syntax-rules (...)
      #|    ((_ x) (syntax->datum (syntax (x ...))))))
      #|(show 1)
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 ...)")
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

///|
test "r6rs macro: invalid ellipsis in pattern" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ x ... ...) 'ok)))
      #|(bad 1 2)
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for invalid ellipsis")
  }
}

///|
test "r6rs macro: invalid ellipsis in template" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ x) (list ...))))
      #|(bad 1)
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for invalid template ellipsis")
  }
}

///|
test "r6rs macro: invalid ellipsis depth" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ (x ...)) x)))
      #|(bad (1 2))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for mismatched ellipsis depth")
  }
}

///|
test "r6rs macro: invalid nested ellipsis depth (fewer in template)" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ ((x ...) ...)) (list x ...))))
      #|(bad ((1 2) (3 4)))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for nested ellipsis mismatch")
  }
}

///|
test "r6rs macro: invalid nested ellipsis depth (more in template)" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ (x ...)) (list (list x ...) ...))))
      #|(bad (1 2))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for nested ellipsis mismatch")
  }
}

///|
test "r6rs macro: invalid nested ellipsis depth (vector fewer in template)" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ #((x ...) ...)) #(x ...))))
      #|(bad #((1 2) (3 4)))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for vector ellipsis mismatch")
  }
}

///|
test "r6rs macro: invalid nested ellipsis depth (vector more in template)" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ #(x ...)) #((x ...) ...))))
      #|(bad #(1 2))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for vector ellipsis mismatch")
  }
}

///|
test "r6rs macro: invalid nested ellipsis depth (vector fewer in quasisyntax)" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ #((x ...) ...)) (quasisyntax #(x ...)))))
      #|(bad #((1 2) (3 4)))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for vector ellipsis mismatch")
  }
}

///|
test "r6rs macro: invalid nested ellipsis depth (vector more in quasisyntax)" {
  let bad = try? eval_program(
    (
      #|(define-syntax bad
      #|  (syntax-rules ()
      #|    ((_ #(x ...)) (quasisyntax #((x ...) ...)))))
      #|(bad #(1 2))
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"invalid syntax-rules\")")
    _ => fail("expected syntax-rules error for vector ellipsis mismatch")
  }
}
