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
