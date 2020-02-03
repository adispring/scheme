(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? (car l) a) (cdr l))
     (else (cons (car l) (rember-f test? a (cdr l)))))))

(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(eq?-salad 'tuna)
(eq?-salad 'salad)

((eq?-c 'salad) 'tuna)

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

((rember-f eq?) 'jelly '(jelly beans are good))

(define rember-eq? (rember-f eq?))

(rember-eq? 'tuna '(tuna salad is good))

((rember-f eq?) 'tuna '(shrimp salad and tuna salad))

((rember-f eq?) 'eq? '(equal? eq? eqan? eqlist? eqpair?))
