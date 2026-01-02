///|
test "r6rs macro: define-syntax basics" {
  let program =
    #|(define-syntax inc
    #|  (syntax-rules ()
    #|    ((inc x) (set! x (+ x 1)))))
    #|(let ((x 1))
    #|  (inc x)
    #|  x)
  inspect(value_to_string(eval_program(program)), content="2")
  let program2 =
    #|(define-syntax swap!
    #|  (syntax-rules ()
    #|    ((swap! a b) (let ((tmp a)) (set! a b) (set! b tmp)))))
    #|(let ((x 1) (y 2))
    #|  (swap! x y)
    #|  (list x y))
  inspect(value_to_string(eval_program(program2)), content="(2 1)")
}

///|
test "r6rs macro: syntax-rules literals" {
  let program =
    #|(define-syntax my-if
    #|  (syntax-rules (then else)
    #|    ((my-if test then a else b) (if test a b))))
    #|(my-if #t then 1 else 2)
  inspect(value_to_string(eval_program(program)), content="1")
  let program2 =
    #|(define-syntax my-if
    #|  (syntax-rules (then else)
    #|    ((my-if test then a else b) (if test a b))))
    #|(my-if #f then 1 else 2)
  inspect(value_to_string(eval_program(program2)), content="2")
}

///|
test "r6rs macro: literals respect bindings" {
  let program =
    #|(define x 'top)
    #|(define-syntax pick
    #|  (syntax-rules (x)
    #|    ((_ x) 'literal)
    #|    ((_ y) 'other)))
    #|(list (pick x) (let ((x 'local)) (pick x)))
  inspect(value_to_string(eval_program(program)), content="(literal other)")
}

///|
test "r6rs macro: quote strips syntax objects" {
  let program =
    #|(define-syntax literal-pair
    #|  (syntax-rules ()
    #|    ((literal-pair) '(a b))))
    #|(literal-pair)
  inspect(value_to_string(eval_program(program)), content="(a b)")
}

///|
test "r6rs macro: hygiene with internal bindings" {
  let program =
    #|(define-syntax or2
    #|  (syntax-rules ()
    #|    ((or2 a b)
    #|      (let ((t a))
    #|        (if t t b)))))
    #|(let ((t 1))
    #|  (or2 #f t))
  inspect(value_to_string(eval_program(program)), content="1")
}

///|
test "r6rs macro: dotted list pattern" {
  let program =
    #|(define-syntax split-dot
    #|  (syntax-rules ()
    #|    ((split-dot (a . b)) (list a b))))
    #|(split-dot (1 . 2))
  inspect(value_to_string(eval_program(program)), content="(1 2)")
}

///|
test "r6rs macro: chained expansions" {
  let program =
    #|(define-syntax add1
    #|  (syntax-rules ()
    #|    ((add1 x) (+ x 1))))
    #|(define-syntax add2
    #|  (syntax-rules ()
    #|    ((add2 x) (add1 (add1 x)))))
    #|(add2 3)
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs macro: definition environment bindings" {
  let program =
    #|(library (def-env)
    #|  (export m)
    #|  (import)
    #|  (define (helper x) (+ x 10))
    #|  (define-syntax m
    #|    (syntax-rules ()
    #|      ((_ x) (helper x)))))
    #|(import (def-env))
    #|(m 2)
  inspect(value_to_string(eval_program(program)), content="12")
}

///|
test "r6rs macro: procedural definition environment" {
  let program =
    #|(library (proc-env)
    #|  (export m)
    #|  (import)
    #|  (define (helper x) (+ x 10))
    #|  (define-syntax m
    #|    (lambda (stx)
    #|      (datum->syntax #f '(helper 3)))))
    #|(import (proc-env))
    #|(let ((helper (lambda (x) (+ x 100))))
    #|  (m))
  inspect(value_to_string(eval_program(program)), content="13")
}

///|
test "r6rs macro: procedural use-site identifiers" {
  let program =
    #|(define-syntax get-x
    #|  (lambda (stx)
    #|    (datum->syntax stx 'x)))
    #|(let ((x 7))
    #|  (get-x))
  inspect(value_to_string(eval_program(program)), content="7")
}

///|
test "r6rs macro: procedural transformer expression" {
  let program =
    #|(define-syntax const-ok
    #|  (let ((d '(quote ok)))
    #|    (lambda (stx) (datum->syntax stx d))))
    #|(const-ok)
  inspect(value_to_string(eval_program(program)), content="ok")
}

///|
test "r6rs macro: procedural identifier equality" {
  let program =
    #|(let ((x 'outer))
    #|  (define-syntax check
    #|    (let ((a (syntax x))
    #|          (b (let ((x 'inner)) (syntax x))))
    #|      (lambda (stx)
    #|        (if (and (free-identifier=? a a)
    #|                 (bound-identifier=? a a)
    #|                 (not (free-identifier=? a b))
    #|                 (not (bound-identifier=? a b)))
    #|            #'#t
    #|            #'#f))))
    #|  (check))
  inspect(value_to_string(eval_program(program)), content="#t")
}

///|
test "r6rs macro: datum->syntax def and use-site contexts" {
  let program =
    #|(let ((x 'def))
    #|  (define-syntax pick-x
    #|    (let ((def-x (syntax x)))
    #|      (lambda (stx)
    #|        (let ((use (datum->syntax stx 'x))
    #|              (def (datum->syntax def-x 'x)))
    #|          (list 'list use def)))))
    #|  (let ((x 'use))
    #|    (pick-x x)))
  inspect(value_to_string(eval_program(program)), content="(use def)")
}

///|
test "r6rs macro: procedural transformer with syntax-case" {
  let program =
    #|(define-syntax addk
    #|  (let ((k 2))
    #|    (lambda (stx)
    #|      (syntax-case stx ()
    #|        ((_ x) #`(+ x #,k))))))
    #|(addk 3)
  inspect(value_to_string(eval_program(program)), content="5")
}

///|
test "r6rs macro: syntax-rules preserves syntax forms" {
  let program =
    #|(define-syntax make-stx
    #|  (syntax-rules ()
    #|    ((_ x) (syntax x))
    #|    ((_ x y) (quasisyntax (list #,x #,y)))))
    #|(define a (syntax? (make-stx 1)))
    #|(define b (syntax->datum (make-stx 1)))
    #|(define c (syntax? (make-stx 1 2)))
    #|(define d (syntax->datum (make-stx 1 2)))
    #|(list a b c d)
  inspect(
    value_to_string(eval_program(program)),
    content="(#t 1 #t (list 1 2))",
  )
}

///|
test "r6rs macro: syntax-case ellipsis template" {
  let program =
    #|(define-syntax sc-pairs
    #|  (lambda (stx)
    #|    (syntax-case stx ()
    #|      ((_ (a b) ...)
    #|       #'(list (list a b) ...)))))
    #|(sc-pairs (1 2) (3 4))
  inspect(value_to_string(eval_program(program)), content="((1 2) (3 4))")
}

///|
test "r6rs macro: syntax-case nested ellipsis" {
  let program =
    #|(define-syntax sc-nested
    #|  (lambda (stx)
    #|    (syntax-case stx ()
    #|      ((_ ((x ...) ...))
    #|       #'(list (list x ...) ...)))))
    #|(sc-nested ((1 2) (3 4 5)))
  inspect(value_to_string(eval_program(program)), content="((1 2) (3 4 5))")
}

///|
test "r6rs macro: syntax-case fender" {
  let program =
    #|(define-syntax sc-true
    #|  (lambda (stx)
    #|    (syntax-case stx ()
    #|      ((_ x) #t #'(+ x 1))
    #|      ((_ x) #'x))))
    #|(define-syntax sc-false
    #|  (lambda (stx)
    #|    (syntax-case stx ()
    #|      ((_ x) #f #'(+ x 1))
    #|      ((_ x) #'x))))
    #|(list (sc-true 2) (sc-false 2))
  inspect(value_to_string(eval_program(program)), content="(3 2)")
}

///|
test "r6rs macro: syntax-case with-syntax" {
  let program =
    #|(define-syntax sc-dup
    #|  (lambda (stx)
    #|    (syntax-case stx ()
    #|      ((_ x)
    #|       (with-syntax ((y x))
    #|         #'(list y y))))))
    #|(sc-dup 5)
  inspect(value_to_string(eval_program(program)), content="(5 5)")
}

///|
test "r6rs macro: syntax-case transformer errors" {
  let bad_params = try? eval_program(
    (
      #|(define-syntax bad
      #|  (lambda (a b)
      #|    (syntax-case a ()
      #|      ((_ x) #'x))))
      #|(bad 1)
    ),
  )
  inspect(bad_params is Err(_), content="true")
  let bad_clauses = try? eval_program(
    (
      #|(define-syntax bad
      #|  (lambda (stx)
      #|    (syntax-case stx ())))
      #|(bad 1)
    ),
  )
  inspect(bad_clauses is Err(_), content="true")
}

///|
test "r6rs macro: identifier-syntax templates" {
  let program =
    #|(define-syntax dotted
    #|  (identifier-syntax
    #|    ((dotted x) (x . y))))
    #|(define-syntax vect
    #|  (identifier-syntax
    #|    ((vect x) #(x y))))
    #|#t
  inspect(value_to_string(eval_program(program)), content="#t")
}

///|
test "r6rs macro: identifier-syntax patterns" {
  let program =
    #|(define-syntax dotpat
    #|  (identifier-syntax
    #|    ((dotpat (a . b)) (list a b))))
    #|(define-syntax vecpat
    #|  (identifier-syntax
    #|    ((vecpat #(a b)) (list a b))))
    #|#t
  inspect(value_to_string(eval_program(program)), content="#t")
}

///|
test "r6rs macro: syntax-case with-syntax bodies" {
  let program =
    #|(define-syntax sc-begin
    #|  (lambda (stx)
    #|    (syntax-case stx ()
    #|      ((_ x)
    #|       (with-syntax ((y x))
    #|         #'(list y y)
    #|         #'(list y))))))
    #|(sc-begin 5)
  inspect(value_to_string(eval_program(program)), content="(5)")
}

///|
test "r6rs macro: with-syntax errors" {
  let bad_arity = try? eval_program(
    (
      #|(define-syntax bad
      #|  (lambda (stx)
      #|    (syntax-case stx ()
      #|      ((_ x) (with-syntax)))))
      #|(bad 1)
    ),
  )
  inspect(bad_arity is Err(_), content="true")
  let bad_spec = try? eval_program(
    (
      #|(define-syntax bad
      #|  (lambda (stx)
      #|    (syntax-case stx ()
      #|      ((_ x) (with-syntax ((y)) #'x)))))
      #|(bad 1)
    ),
  )
  inspect(bad_spec is Err(_), content="true")
  let bad_match = try? eval_program(
    (
      #|(define-syntax bad
      #|  (lambda (stx)
      #|    (syntax-case stx ()
      #|      ((_ x) (with-syntax (((y) 1)) #'x)))))
      #|(bad 1)
    ),
  )
  inspect(bad_match is Err(_), content="true")
  let bad_merge = try? eval_program(
    (
      #|(define-syntax bad
      #|  (lambda (stx)
      #|    (syntax-case stx ()
      #|      ((_ x) (with-syntax ((y 1) (y 2)) #'y)))))
      #|(bad 1)
    ),
  )
  inspect(bad_merge is Err(_), content="true")
}

///|
test "r6rs macro: syntax-case literal matching" {
  let program =
    #|(define-syntax sc-lit
    #|  (lambda (stx)
    #|    (syntax-case stx (kw)
    #|      ((_ kw x) #'(list 'ok x))
    #|      ((_ other x) #'(list 'no x)))))
    #|(list (sc-lit kw 1)
    #|      (let ((kw 'shadow)) (sc-lit kw 2)))
  inspect(value_to_string(eval_program(program)), content="((ok 1) (no 2))")
}

///|
test "r6rs macro: vector template ellipsis" {
  let program =
    #|(define-syntax vlist
    #|  (syntax-rules ()
    #|    ((_ x ...) #(x ...))))
    #|(vlist 1 2 3)
  inspect(value_to_string(eval_program(program)), content="#(1 2 3)")
}

///|
test "r6rs macro: syntax-case literal pattern matrix" {
  let program =
    #|(define-syntax lit-case
    #|  (lambda (stx)
    #|    (syntax-case stx (sym)
    #|      ((_ 1) #'"one")
    #|      ((_ 10000000000000000000001) #'"big")
    #|      ((_ 1/3) #'"rat")
    #|      ((_ 10000000000000000000001/3) #'"bigrat")
    #|      ((_ 1+2i) #'"complex")
    #|      ((_ 1.5) #'"float")
    #|      ((_ #\a) #'"char")
    #|      ((_ "hi") #'"string")
    #|      ((_ #(1 2)) #'"vector")
    #|      ((_ #vu8(1 2)) #'"bytevector")
    #|      ((_ (1 . 2)) #'"pair")
    #|      ((_ sym) #'"sym")
    #|      ((_ x) #'"other"))))
    #|(list (lit-case 1)
    #|      (lit-case 10000000000000000000001)
    #|      (lit-case 1/3)
    #|      (lit-case 10000000000000000000001/3)
    #|      (lit-case 1+2i)
    #|      (lit-case 1.5)
    #|      (lit-case #\a)
    #|      (lit-case "hi")
    #|      (lit-case #(1 2))
    #|      (lit-case #vu8(1 2))
    #|      (lit-case (1 . 2))
    #|      (lit-case sym))
  inspect(
    value_to_string(eval_program(program)),
    content="(\"one\" \"big\" \"rat\" \"bigrat\" \"complex\" \"float\" \"char\" \"string\" \"vector\" \"bytevector\" \"pair\" \"sym\")",
  )
}
