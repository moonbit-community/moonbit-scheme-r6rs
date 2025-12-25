///|
test "begin-for-syntax defines macros" {
  let program = (
    (
      #|(begin-for-syntax
      #|  (define-syntax mk (syntax-rules () ((_ x) x))))
      #|(mk 7)
    )
  )
  inspect(value_to_string(eval_program(program)), content="7")
}

///|
test "begin-for-syntax does not export values" {
  let bad = try? eval_program(
    (
      #|(begin-for-syntax (define x 1))
      #|x
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"unbound variable: x\")")
    _ => fail("expected EvalError for begin-for-syntax value")
  }
}

///|
test "import for run filters macros" {
  let program = (
    (
      #|(library (phase-lib)
      #|  (export val m)
      #|  (import)
      #|  (define val 5)
      #|  (define-syntax m (syntax-rules () ((_ ) 42))))
      #|(import (for (phase-lib) run))
      #|val
    )
  )
  inspect(value_to_string(eval_program(program)), content="5")
  let bad = try? eval_program(
    (
      #|(library (phase-lib)
      #|  (export val m)
      #|  (import)
      #|  (define val 5)
      #|  (define-syntax m (syntax-rules () ((_ ) 42))))
      #|(import (for (phase-lib) run))
      #|(m)
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"unbound variable: m\")")
    _ => fail("expected EvalError for run-only macro")
  }
}

///|
test "import for expand filters values" {
  let program = (
    (
      #|(library (phase-lib)
      #|  (export val m)
      #|  (import)
      #|  (define val 5)
      #|  (define-syntax m (syntax-rules () ((_ ) 42))))
      #|(import (for (phase-lib) expand))
      #|(m)
    )
  )
  inspect(value_to_string(eval_program(program)), content="42")
  let bad = try? eval_program(
    (
      #|(library (phase-lib)
      #|  (export val m)
      #|  (import)
      #|  (define val 5)
      #|  (define-syntax m (syntax-rules () ((_ ) 42))))
      #|(import (for (phase-lib) expand))
      #|val
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"unbound variable: val\")")
    _ => fail("expected EvalError for expand-only value")
  }
}

///|
test "import for meta 2 behaves like expand" {
  let program = (
    (
      #|(library (phase-lib2)
      #|  (export val m)
      #|  (import)
      #|  (define val 5)
      #|  (define-syntax m (syntax-rules () ((_ ) 42))))
      #|(import (for (phase-lib2) (meta 2)))
      #|(m)
    )
  )
  inspect(value_to_string(eval_program(program)), content="42")
  let bad = try? eval_program(
    (
      #|(library (phase-lib2)
      #|  (export val m)
      #|  (import)
      #|  (define val 5)
      #|  (define-syntax m (syntax-rules () ((_ ) 42))))
      #|(import (for (phase-lib2) (meta 2)))
      #|val
    )
  )
  match bad {
    Err(err) => inspect(err, content="EvalError(\"unbound variable: val\")")
    _ => fail("expected EvalError for meta-only value")
  }
}
