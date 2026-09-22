///|
test "r6rs promises" {
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(force (delay (+ 1 2)))
        ),
      ),
    ),
    content="3",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(let ((p (delay 1)))
          #|  (promise? p))
        ),
      ),
    ),
    content="#t",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(promise? 1)
        ),
      ),
    ),
    content="#f",
  )
  let memo_prog =
    #|(let ((count 0))
    #|  (let ((p (delay (begin (set! count (+ count 1)) count))))
    #|    (force p)
    #|    (force p)
    #|    count))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(memo_prog)),
    content="1",
  )
  inspect(
    @scheme-r6rs.value_to_string(
      @scheme-r6rs.eval_program(
        (
          #|(force (make-promise (lambda () 42)))
        ),
      ),
    ),
    content="42",
  )
}
