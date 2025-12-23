///|
test "r6rs libraries: define and import" {
  let program = (
    #|(library (math)
    #|  (export add1)
    #|  (import)
    #|  (define (add1 x) (+ x 1)))
    #|(import (math))
    #|(add1 4)
  )
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs libraries: exported bindings" {
  let program = (
    #|(library (data)
    #|  (export answer)
    #|  (import)
    #|  (define answer 42))
    #|(import (data))
    #|answer
  )
  inspect(value_to_string(eval_program(program)), content="42")
}

///|
test "r6rs libraries: library imports" {
  let program = (
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
  inspect(value_to_string(eval_program(program)), content="5")
}
