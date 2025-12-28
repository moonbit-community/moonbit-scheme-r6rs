///|
test "r6rs hashtable: basic ops" {
  let program =
    #|(define ht (make-eq-hashtable))
    #|(hashtable-set! ht 'a 1)
    #|(hashtable-set! ht 'b 2)
    #|(define size1 (hashtable-size ht))
    #|(define a1 (hashtable-ref ht 'a 0))
    #|(define c1 (hashtable-contains? ht 'c))
    #|(hashtable-delete! ht 'b)
    #|(define size2 (hashtable-size ht))
    #|(hashtable-clear! ht)
    #|(list size1 a1 c1 size2 (hashtable-size ht))
  inspect(value_to_string(eval_program(program)), content="(2 1 #f 1 0)")
}

///|
test "r6rs hashtable: update and default" {
  let program =
    #|(define ht (make-eq-hashtable))
    #|(hashtable-update! ht 'a (lambda (x) (+ x 1)) 0)
    #|(hashtable-update! ht 'a (lambda (x) (+ x 1)) 0)
    #|(hashtable-ref ht 'a 0)
  inspect(value_to_string(eval_program(program)), content="2")
}

///|
test "r6rs hashtable: custom equivalence" {
  let program =
    #|(define ht (make-hashtable (lambda (x) 0) (lambda (a b) #t)))
    #|(hashtable-set! ht 'a 10)
    #|(hashtable-ref ht 'b 0)
  inspect(value_to_string(eval_program(program)), content="10")
}

///|
test "r6rs hashtable: copy and entries" {
  let program =
    #|(define ht (make-eq-hashtable))
    #|(hashtable-set! ht 'k 42)
    #|(define ht2 (hashtable-copy ht #f))
    #|(define eq-proc (hashtable-equivalence-function ht))
    #|(define hash-proc (hashtable-hash-function ht))
    #|(define pair
    #|  (call-with-values (lambda () (hashtable-entries ht))
    #|    (lambda (ks vs) (list (vector-ref ks 0) (vector-ref vs 0)))))
    #|(list (hashtable-mutable? ht2)
    #|      (procedure? eq-proc)
    #|      (boolean? hash-proc)
    #|      pair)
  inspect(value_to_string(eval_program(program)), content="(#f #t #t (k 42))")
}

///|
test "r6rs hashtable: immutable set fails" {
  let program =
    #|(define ht (make-eq-hashtable))
    #|(hashtable-set! ht 'a 1)
    #|(define ht2 (hashtable-copy ht #f))
    #|(hashtable-set! ht2 'b 2)
  let result = try? eval_program(program)
  match result {
    Err(err) => inspect(err, content="EvalError(\"hashtable is immutable\")")
    _ => fail("expected immutable hashtable error")
  }
}
