///|
test "r6rs equality predicates" {
  inspect(value_to_string(eval_program("(eq? 1 1)")), content="#t")
  inspect(value_to_string(eval_program("(eqv? #t #t)")), content="#t")
  inspect(value_to_string(eval_program("(equal? '(1 2) '(1 2))")), content="#t")
  inspect(value_to_string(eval_program("(eq? \"hi\" \"hi\")")), content="#t")
  inspect(value_to_string(eval_program("(eq? 'a 'b)")), content="#f")
  inspect(value_to_string(eval_program("(eq? '(1 2) '(1 3))")), content="#f")
}

///|
test "r6rs list utilities" {
  inspect(value_to_string(eval_program("(list? '())")), content="#t")
  inspect(value_to_string(eval_program("(list? '(1 2 3))")), content="#t")
  inspect(value_to_string(eval_program("(list? '(1 . 2))")), content="#f")
  inspect(value_to_string(eval_program("(length '())")), content="0")
  inspect(value_to_string(eval_program("(length '(1 2 3))")), content="3")
  inspect(value_to_string(eval_program("(make-list 3 'a)")), content="(a a a)")
  inspect(value_to_string(eval_program("(length (make-list 4))")), content="4")
  inspect(value_to_string(eval_program("(list-ref '(a b c) 1)")), content="b")
  inspect(value_to_string(eval_program("(list-tail '(a b c d) 2)")), content="(c d)")
  inspect(value_to_string(eval_program("(cadr '(1 2 3))")), content="2")
  inspect(value_to_string(eval_program("(caddr '(1 2 3 4))")), content="3")
  inspect(value_to_string(eval_program("(cddr '(1 2 3))")), content="(3)")
  inspect(value_to_string(eval_program("(caar '((1 2) (3 4)))")), content="1")
  inspect(value_to_string(eval_program("(member 2 '(1 2 3))")), content="(2 3)")
  inspect(value_to_string(eval_program("(memq 2 '(1 2 3))")), content="(2 3)")
  inspect(value_to_string(eval_program("(memv 2 '(1 2 3))")), content="(2 3)")
  inspect(value_to_string(eval_program("(assoc 'b '((a . 1) (b . 2)))")), content="(b . 2)")
  inspect(value_to_string(eval_program("(assq 'b '((a . 1) (b . 2)))")), content="(b . 2)")
  inspect(value_to_string(eval_program("(assv 'b '((a . 1) (b . 2)))")), content="(b . 2)")
  inspect(
    value_to_string(eval_program("(map (lambda (x) (+ x 1)) '(1 2 3))")),
    content="(2 3 4)",
  )
  inspect(value_to_string(eval_program("(map + '(1 2 3) '(4 5 6))")), content="(5 7 9)")
  let foreach_prog =
    "(let ((acc 0)) (for-each (lambda (x) (set! acc (+ acc x))) '(1 2 3)) acc)"
  inspect(value_to_string(eval_program(foreach_prog)), content="6")
  inspect(value_to_string(eval_program("(append)")), content="()")
  inspect(value_to_string(eval_program("(append '(1 2) '(3 4))")), content="(1 2 3 4)")
  inspect(value_to_string(eval_program("(append '(1 2) '(3 . 4))")), content="(1 2 3 . 4)")
  inspect(value_to_string(eval_program("(reverse '(1 2 3))")), content="(3 2 1)")
}

///|
test "r6rs list utilities: errors" {
  let improper = try? eval_program("(length '(1 . 2))")
  match improper {
    Err(err) => inspect(err, content="EvalError(\"type error: proper list expected\")")
    _ => fail("expected EvalError for improper list")
  }

  let bad_ref = try? eval_program("(list-ref '(1 2) 2)")
  match bad_ref {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for list-ref out of range")
  }

  let bad_tail = try? eval_program("(list-tail '(1 2) 3)")
  match bad_tail {
    Err(err) => inspect(err, content="EvalError(\"index out of range\")")
    _ => fail("expected EvalError for list-tail out of range")
  }

  let negative_len = try? eval_program("(make-list -1)")
  match negative_len {
    Err(err) => inspect(err, content="EvalError(\"type error: exact nonnegative integer expected\")")
    _ => fail("expected EvalError for negative make-list length")
  }

  let bad_member = try? eval_program("(member 3 '(1 . 2))")
  match bad_member {
    Err(err) => inspect(err, content="EvalError(\"type error: proper list expected\")")
    _ => fail("expected EvalError for improper list")
  }

  let bad_assoc = try? eval_program("(assoc 'a '(1 2))")
  match bad_assoc {
    Err(err) => inspect(err, content="EvalError(\"type error: pair expected\")")
    _ => fail("expected EvalError for non-pair alist entry")
  }

  let bad_map = try? eval_program("(map (lambda (x) x) '(1 . 2))")
  match bad_map {
    Err(err) => inspect(err, content="EvalError(\"type error: proper list expected\")")
    _ => fail("expected EvalError for improper list in map")
  }
}
