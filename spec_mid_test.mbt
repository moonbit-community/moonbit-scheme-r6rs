///|
test "r6rs mid: control and bindings" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(if #t 1 2)
        ),
      ),
    ),
    content="1",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(if #f 1 2)
        ),
      ),
    ),
    content="2",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(begin (define x 1) (set! x 5) x)
        ),
      ),
    ),
    content="5",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(begin (define x 3) (define y 4) (+ x y))
        ),
      ),
    ),
    content="7",
  )
}

///|
test "r6rs mid: lambda and lexical scope" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|((lambda (x) (+ x 1)) 4)
        ),
      ),
    ),
    content="5",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(begin
          #|  (define x 1)
          #|  ((lambda (x) (begin (set! x 2) x)) 10)
          #|  x)
        ),
      ),
    ),
    content="1",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(procedure? +)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(procedure? (lambda (x) x))
        ),
      ),
    ),
    content="#t",
  )
}

///|
test "r6rs mid: let forms" {
  inspect(
    value_to_string(
      eval_program(
        (
          #|(let ((x 1) (y 2)) (+ x y))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    value_to_string(
      eval_program(
        (
          #|(let* ((x 1) (y (+ x 2))) y)
        ),
      ),
    ),
    content="3",
  )
}
