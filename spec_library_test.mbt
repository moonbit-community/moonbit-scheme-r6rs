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
}
