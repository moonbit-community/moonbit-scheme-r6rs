///|
test "r6rs condition stdlib: &message and accessors" {
  let program =
    #|(define c (make-message-condition "hi"))
    #|(list (record-type-descriptor? &message)
    #|      (message-condition? c)
    #|      (condition-message c))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="(#t #t \"hi\")",
  )
}

///|
test "r6rs condition stdlib: error constructs compound condition" {
  let program =
    #|(guard (e ((error? e)
    #|           (list (condition-message e)
    #|                 (condition-who e)
    #|                 (condition-irritants e)))
    #|          (else 'bad))
    #|  (error 'who "boom" 1 2))
  inspect(
    @scheme-r6rs.value_to_string(@scheme-r6rs.eval_program(program)),
    content="(\"boom\" who (1 2))",
  )
}
