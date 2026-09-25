///|
test "r6rs mid: control and bindings" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(if #t 1 2)
        ),
      ),
    ),
    content="1",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(if #f 1 2)
        ),
      ),
    ),
    content="2",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(begin (define x 1) (set! x 5) x)
        ),
      ),
    ),
    content="5",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
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
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|((lambda (x) (+ x 1)) 4)
        ),
      ),
    ),
    content="5",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
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
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(procedure? +)
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
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
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((x 1) (y 2)) (+ x y))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let* ((x 1) (y (+ x 2))) y)
        ),
      ),
    ),
    content="3",
  )
}
