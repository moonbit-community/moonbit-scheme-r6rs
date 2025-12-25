///|
test "large program: quicksort with macro" {
  let program = (
    (
      #|(let-syntax ((when
      #|               (syntax-rules ()
      #|                 ((_ test expr ...)
      #|                  (if test (begin expr ...) #f)))))
      #|  (define (quicksort lst)
      #|    (if (null? lst)
      #|        '()
      #|        (let* ((pivot (car lst))
      #|               (rest (cdr lst))
      #|               (smaller (filter (lambda (x) (< x pivot)) rest))
      #|               (greater (filter (lambda (x) (>= x pivot)) rest)))
      #|          (append (quicksort smaller) (list pivot) (quicksort greater)))))
      #|  (define (sorted? lst)
      #|    (cond ((null? lst) #t)
      #|          ((null? (cdr lst)) #t)
      #|          ((<= (car lst) (cadr lst)) (sorted? (cdr lst)))
      #|          (else #f)))
      #|  (let ((result (quicksort '(3 1 4 1 5 9 2 6 5 3 5)))
      #|        (ok #t))
      #|    (when (not (sorted? result)) (set! ok #f))
      #|    (list ok result)))
    )
  )
  inspect(
    value_to_string(eval_program(program)),
    content="(#t (1 1 2 3 3 4 5 5 5 6 9))",
  )
}

///|
test "large program: n-queens count" {
  let program = (
    (
      #|(let ()
      #|  (define (enumerate-interval low high)
      #|    (if (> low high)
      #|        '()
      #|        (cons low (enumerate-interval (+ low 1) high))))
      #|  (define (adjoin-position new-row rest)
      #|    (cons new-row rest))
      #|  (define (safe? positions)
      #|    (let ((row (car positions)))
      #|      (define (iter rest dist)
      #|        (cond ((null? rest) #t)
      #|              ((or (= row (car rest))
      #|                   (= (- row dist) (car rest))
      #|                   (= (+ row dist) (car rest)))
      #|               #f)
      #|              (else (iter (cdr rest) (+ dist 1)))))
      #|      (iter (cdr positions) 1)))
      #|  (define (queen-cols k n)
      #|    (if (= k 0)
      #|        (list '())
      #|        (filter safe?
      #|                (apply append
      #|                       (map (lambda (rest-of-queens)
      #|                              (map (lambda (new-row)
      #|                                     (adjoin-position new-row rest-of-queens))
      #|                                   (enumerate-interval 1 n)))
      #|                            (queen-cols (- k 1) n))))))
      #|  (length (queen-cols 7 7)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="40")
}

///|
test "large program: prime sum" {
  let program = (
    (
      #|(let ()
      #|  (define (range low high)
      #|    (if (> low high)
      #|        '()
      #|        (cons low (range (+ low 1) high))))
      #|  (define (sieve lst)
      #|    (if (null? lst)
      #|        '()
      #|        (let ((p (car lst)))
      #|          (cons p
      #|                (sieve
      #|                  (filter (lambda (x) (not (= 0 (modulo x p))))
      #|                          (cdr lst)))))))
      #|  (define primes (sieve (range 2 100)))
      #|  (apply + primes))
    )
  )
  inspect(value_to_string(eval_program(program)), content="1060")
}

///|
test "large program: takeuchi recursion" {
  let program = (
    (
      #|(let ()
      #|  (define (tak x y z)
      #|    (if (not (< y x))
      #|        z
      #|        (tak (tak (- x 1) y z)
      #|             (tak (- y 1) z x)
      #|             (tak (- z 1) x y))))
      #|  (tak 18 12 6))
    )
  )
  inspect(value_to_string(eval_program(program)), content="7")
}

///|
test "large program: tree sum" {
  let program = (
    (
      #|(let ()
      #|  (define (tree-sum t)
      #|    (cond ((null? t) 0)
      #|          ((pair? t) (+ (tree-sum (car t)) (tree-sum (cdr t))))
      #|          (else t)))
      #|  (tree-sum '(1 (2 (3 4) 5) (6 7) 8)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="36")
}

///|
test "large program: matrix multiply" {
  let program = (
    (
      #|(let ()
      #|  (define (transpose m)
      #|    (if (null? (car m))
      #|        '()
      #|        (cons (map car m) (transpose (map cdr m)))))
      #|  (define (dot v w)
      #|    (apply + (map * v w)))
      #|  (define (matmul a b)
      #|    (let ((cols (transpose b)))
      #|      (map (lambda (row)
      #|             (map (lambda (col) (dot row col)) cols))
      #|           a)))
      #|  (matmul '((1 2 3) (4 5 6)) '((7 8) (9 10) (11 12))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="((58 64) (139 154))")
}

///|
test "large program: call/cc early exit" {
  let program = (
    (
      #|(let ()
      #|  (define (find-negative tree)
      #|    (call/cc
      #|      (lambda (exit)
      #|        (define (walk t)
      #|          (cond ((null? t) #f)
      #|                ((pair? t) (begin (walk (car t)) (walk (cdr t))))
      #|                ((< t 0) (exit t))
      #|                (else #f)))
      #|        (walk tree)
      #|        #f)))
      #|  (find-negative '(1 (2 3) (4 (-5 6)) 7)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="-5")
}

///|
test "large program: mergesort" {
  let program = (
    (
      #|(let ()
      #|  (define (merge xs ys)
      #|    (cond ((null? xs) ys)
      #|          ((null? ys) xs)
      #|          ((<= (car xs) (car ys))
      #|           (cons (car xs) (merge (cdr xs) ys)))
      #|          (else
      #|           (cons (car ys) (merge xs (cdr ys))))))
      #|  (define (split lst)
      #|    (let loop ((slow lst) (fast lst) (acc '()))
      #|      (if (or (null? fast) (null? (cdr fast)))
      #|          (list (reverse acc) slow)
      #|          (loop (cdr slow) (cddr fast) (cons (car slow) acc)))))
      #|  (define (mergesort lst)
      #|    (if (or (null? lst) (null? (cdr lst)))
      #|        lst
      #|        (let* ((parts (split lst))
      #|               (left (car parts))
      #|               (right (cadr parts)))
      #|          (merge (mergesort left) (mergesort right)))))
      #|  (mergesort '(8 3 7 4 9 2 6 5 1)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(1 2 3 4 5 6 7 8 9)")
}

///|
test "large program: symbolic differentiation" {
  let program = (
    (
      #|(let ()
      #|  (define (make-sum a b)
      #|    (cond ((and (number? a) (= a 0)) b)
      #|          ((and (number? b) (= b 0)) a)
      #|          ((and (number? a) (number? b)) (+ a b))
      #|          (else (list '+ a b))))
      #|  (define (make-product a b)
      #|    (cond ((or (and (number? a) (= a 0))
      #|               (and (number? b) (= b 0))) 0)
      #|          ((and (number? a) (= a 1)) b)
      #|          ((and (number? b) (= b 1)) a)
      #|          ((and (number? a) (number? b)) (* a b))
      #|          (else (list '* a b))))
      #|  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
      #|  (define (product? x) (and (pair? x) (eq? (car x) '*)))
      #|  (define (deriv expr var)
      #|    (cond ((number? expr) 0)
      #|          ((symbol? expr) (if (eq? expr var) 1 0))
      #|          ((sum? expr)
      #|           (make-sum (deriv (cadr expr) var)
      #|                     (deriv (caddr expr) var)))
      #|          ((product? expr)
      #|           (make-sum
      #|             (make-product (cadr expr) (deriv (caddr expr) var))
      #|             (make-product (deriv (cadr expr) var) (caddr expr))))
      #|          (else 'unknown)))
      #|  (deriv '(* x (+ x 3)) 'x))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(+ x (+ x 3))")
}

///|
test "large program: permutations count" {
  let program = (
    (
      #|(let ()
      #|  (define (remove-one x lst)
      #|    (cond ((null? lst) '())
      #|          ((equal? x (car lst)) (cdr lst))
      #|          (else (cons (car lst) (remove-one x (cdr lst))))))
      #|  (define (perms lst)
      #|    (if (null? lst)
      #|        (list '())
      #|        (apply append
      #|               (map (lambda (x)
      #|                      (map (lambda (p) (cons x p))
      #|                           (perms (remove-one x lst))))
      #|                    lst))))
      #|  (length (perms '(1 2 3 4))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="24")
}

///|
test "large program: memoized fibonacci" {
  let program = (
    (
      #|(let ()
      #|  (define table (make-eqv-hashtable))
      #|  (define (fib n)
      #|    (let ((hit (hashtable-ref table n #f)))
      #|      (if hit
      #|          hit
      #|          (let ((value (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
      #|            (hashtable-set! table n value)
      #|            value))))
      #|  (fib 20))
    )
  )
  inspect(value_to_string(eval_program(program)), content="6765")
}

///|
test "large program: towers of hanoi" {
  let program = (
    (
      #|(let ()
      #|  (define (hanoi n from to spare)
      #|    (if (= n 0)
      #|        '()
      #|        (append (hanoi (- n 1) from spare to)
      #|                (list (list from to))
      #|                (hanoi (- n 1) spare to from))))
      #|  (define moves (hanoi 5 'a 'b 'c))
      #|  (list (length moves) (car moves) (car (reverse moves))))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(31 (a b) (a b))")
}

///|
test "large program: meta-circular evaluator" {
  let program = (
    (
      #|(let ()
      #|  (define (lookup var env)
      #|    (let ((pair (assq var env)))
      #|      (if pair (cdr pair) (error "unbound"))))
      #|  (define (extend vars vals env)
      #|    (if (null? vars)
      #|        env
      #|        (cons (cons (car vars) (car vals))
      #|              (extend (cdr vars) (cdr vals) env))))
      #|  (define (eval-seq body env)
      #|    (if (null? (cdr body))
      #|        (m-eval (car body) env)
      #|        (begin (m-eval (car body) env)
      #|               (eval-seq (cdr body) env))))
      #|  (define (apply-proc proc args)
      #|    (if (and (pair? proc) (eq? (car proc) 'closure))
      #|        (let ((params (cadr proc))
      #|              (body (caddr proc))
      #|              (env (cadddr proc)))
      #|          (eval-seq body (extend params args env)))
      #|        (apply proc args)))
      #|  (define (m-eval exp env)
      #|    (cond ((number? exp) exp)
      #|          ((symbol? exp) (lookup exp env))
      #|          ((pair? exp)
      #|           (case (car exp)
      #|             ((quote) (cadr exp))
      #|             ((if) (if (m-eval (cadr exp) env)
      #|                      (m-eval (caddr exp) env)
      #|                      (m-eval (cadddr exp) env)))
      #|             ((lambda) (list 'closure (cadr exp) (cddr exp) env))
      #|             (else
      #|              (apply-proc (m-eval (car exp) env)
      #|                          (map (lambda (arg) (m-eval arg env)) (cdr exp))))))
      #|          (else exp)))
      #|  (define base-env
      #|    (list (cons '+ +) (cons '- -) (cons '* *) (cons '< <) (cons '= =)
      #|          (cons 'cons cons) (cons 'car car) (cons 'cdr cdr) (cons 'list list)))
      #|  (list
      #|    (m-eval '((lambda (x) (if (< x 10) (+ x 1) (* x 2))) 7) base-env)
      #|    (m-eval '((lambda (x) ((lambda (y) (+ x y)) 5)) 3) base-env)))
    )
  )
  inspect(value_to_string(eval_program(program)), content="(8 8)")
}
