///|
test "r6rs libraries: define and import" {
  let program = (
    (
      #|(library (math)
      #|  (export add1)
      #|  (import)
      #|  (define (add1 x) (+ x 1)))
      #|(import (math))
      #|(add1 4)
    )
  )
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs libraries: exported bindings" {
  let program = (
    (
      #|(library (data)
      #|  (export answer)
      #|  (import)
      #|  (define answer 42))
      #|(import (data))
      #|answer
    )
  )
  inspect(value_to_string(eval_program(program)), content="42")
}

///|
test "r6rs libraries: library imports" {
  let program = (
    (
      #|(library (base)
      #|  (export inc)
      #|  (import)
      #|  (define (inc x) (+ x 1)))
      #|(library (user)
      #|  (export add2)
      #|  (import (base))
      #|  (define (add2 x) (inc (inc x))))
      #|(import (user))
      #|(add2 3)
    )
  )
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs libraries: import sets" {
  let only_prog = (
    (
      #|(library (base-only)
      #|  (export a b)
      #|  (import)
      #|  (define a 1)
      #|  (define b 2))
      #|(import (only (base-only) a))
      #|a
    )
  )
  inspect(value_to_string(eval_program(only_prog)), content="1")
  let except_prog = (
    (
      #|(library (base-except)
      #|  (export a b)
      #|  (import)
      #|  (define a 3)
      #|  (define b 4))
      #|(import (except (base-except) a))
      #|b
    )
  )
  inspect(value_to_string(eval_program(except_prog)), content="4")
  let rename_prog = (
    (
      #|(library (base-rename)
      #|  (export a b)
      #|  (import)
      #|  (define a 5)
      #|  (define b 6))
      #|(import (rename (base-rename) (a x) (b y)))
      #|(+ x y)
    )
  )
  inspect(value_to_string(eval_program(rename_prog)), content="11")
  let prefix_prog = (
    (
      #|(library (base-prefix)
      #|  (export a b)
      #|  (import)
      #|  (define a 7)
      #|  (define b 8))
      #|(import (prefix (base-prefix) pre-))
      #|(+ pre-a pre-b)
    )
  )
  inspect(value_to_string(eval_program(prefix_prog)), content="15")
  let nested_prog = (
    (
      #|(library (base-nested)
      #|  (export a b)
      #|  (import)
      #|  (define a 9)
      #|  (define b 10))
      #|(import (only (prefix (base-nested) pre-) pre-b))
      #|pre-b
    )
  )
  inspect(value_to_string(eval_program(nested_prog)), content="10")
  let for_prog = (
    (
      #|(library (base-for)
      #|  (export a)
      #|  (import)
      #|  (define a 12))
      #|(import (for (base-for) run))
      #|a
    )
  )
  inspect(value_to_string(eval_program(for_prog)), content="12")
}

///|
test "r6rs libraries: export rename" {
  let program = (
    (
      #|(library (export-rename)
      #|  (export (rename (add1 inc)))
      #|  (import)
      #|  (define (add1 x) (+ x 1)))
      #|(import (export-rename))
      #|(inc 4)
    )
  )
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs libraries: versioned names" {
  let program = (
    (
      #|(library (ver-lib (1 0))
      #|  (export val)
      #|  (import)
      #|  (define val 99))
      #|(import (ver-lib (1 0)))
      #|val
    )
  )
  inspect(value_to_string(eval_program(program)), content="99")
}

///|
test "r6rs libraries: version references" {
  let program = (
    (
      #|(library (ver-ref (1 0))
      #|  (export val)
      #|  (import)
      #|  (define val 10))
      #|(library (ver-ref (2 0))
      #|  (export val)
      #|  (import)
      #|  (define val 20))
      #|(import (ver-ref (>= 1 0)))
      #|val
    )
  )
  inspect(value_to_string(eval_program(program)), content="20")
  let program2 = (
    (
      #|(library (ver-and (1 0))
      #|  (export val)
      #|  (import)
      #|  (define val 100))
      #|(library (ver-and (2 0))
      #|  (export val)
      #|  (import)
      #|  (define val 200))
      #|(import (ver-and (and (>= (1 0)) (< (2 0)))))
      #|val
    )
  )
  inspect(value_to_string(eval_program(program2)), content="100")
}

///|
test "r6rs libraries: base syntax keyword import" {
  let program = (
    (
      #|(import (only (rnrs base)
      #|              define lambda if set! begin syntax-rules identifier-syntax))
      #|(define x 0)
      #|((lambda (v) (if v (begin (set! x 2) x) x)) #t)
    )
  )
  inspect(value_to_string(eval_program(program)), content="2")
}

///|
test "r6rs libraries: base excludes syntax-case" {
  let bad = try? eval_program(
    (
      #|(import (only (rnrs base) syntax-case))
      #|#t
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"unknown import: syntax-case\")")
    _ => fail("expected import error for syntax-case")
  }
}
