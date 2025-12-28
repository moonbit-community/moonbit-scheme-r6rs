///|
test "quasisyntax: unsyntax and splicing" {
  let program =
    #|(let ((x 3) (xs '(4 5)))
    #|  (list
    #|    (syntax->datum (quasisyntax (+ (unsyntax x) 1)))
    #|    (syntax->datum (quasisyntax (a (unsyntax-splicing xs) b)))))
  inspect(value_to_string(eval_program(program)), content="((+ 3 1) (a 4 5 b))")
}

///|
test "quasisyntax: reader abbreviations" {
  let program =
    #|(let ((x 7) (xs '(8 9)))
    #|  (list
    #|    (syntax->datum #'(a b))
    #|    (syntax->datum #`(a #,x b))
    #|    (syntax->datum #`(a #,@xs b))))
  inspect(
    value_to_string(eval_program(program)),
    content="((a b) (a 7 b) (a 8 9 b))",
  )
}

///|
test "quasisyntax: vector unsyntax and splicing" {
  let program =
    #|(let ((x 3) (xs '(4 5)))
    #|  (list
    #|    (syntax->datum #`#(a #,x b))
    #|    (syntax->datum #`#(a #,@xs b))))
  inspect(
    value_to_string(eval_program(program)),
    content="(#(a 3 b) #(a 4 5 b))",
  )
}
