///|
test "r6rs case: datum matching" {
  let program = (
    (
      #|(case 2 ((1) 10) ((2 3) 20) (else 30))
    )
  )
  inspect(value_to_string(eval_program(program)), content="20")

  let program2 = (
    (
      #|(case 'b ((a) 1) ((b c) 2) (else 3))
    )
  )
  inspect(value_to_string(eval_program(program2)), content="2")
}

///|
test "r6rs case: => clause" {
  let program = (
    (
      #|(case 4
      #|  ((1 2 3) 10)
      #|  ((4) => (lambda (x) (+ x 1)))
      #|  (else 0))
    )
  )
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs cond: => clause" {
  let program = (
    (
      #|(cond
      #|  ((+ 1 2) => (lambda (x) (+ x 1)))
      #|  (else 0))
    )
  )
  inspect(value_to_string(eval_program(program)), content="4")
}

///|
test "r6rs let: named let" {
  let program = (
    (
      #|(let loop ((i 0) (acc 0))
      #|  (if (< i 5)
      #|    (loop (+ i 1) (+ acc i))
      #|    acc))
    )
  )
  inspect(value_to_string(eval_program(program)), content="10")
}

///|
test "r6rs letrec*: sequential init" {
  let program = (
    (
      #|(letrec* ((x 1) (y (+ x 2))) y)
    )
  )
  inspect(value_to_string(eval_program(program)), content="3")
}
