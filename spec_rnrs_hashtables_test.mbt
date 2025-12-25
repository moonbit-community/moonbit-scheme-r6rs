///|
test "r6rs libraries: hashtables" {
  let program = (
    (
      #|(import (rnrs hashtables))
      #|(let ((h (make-eqv-hashtable)))
      #|  (hashtable-set! h 'a 1)
      #|  (hashtable-ref h 'a #f))
    )
  )
  inspect(value_to_string(eval_program(program)), content="1")
}
