///|
test "r6rs promises" {
  inspect(
    value_to_string(eval_program(
      (
        #|(force (delay (+ 1 2)))
      )
    )),
    content="3",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(let ((p (delay 1)))
        #|  (promise? p))
      )
    )),
    content="#t",
  )
  inspect(
    value_to_string(eval_program(
      (
        #|(promise? 1)
      )
    )),
    content="#f",
  )
  let memo_prog = (
    (
      #|(let ((count 0))
      #|  (let ((p (delay (begin (set! count (+ count 1)) count))))
      #|    (force p)
      #|    (force p)
      #|    count))
    )
  )
  inspect(value_to_string(eval_program(memo_prog)), content="1")
  inspect(
    value_to_string(eval_program(
      (
        #|(force (make-promise (lambda () 42)))
      )
    )),
    content="42",
  )
}
