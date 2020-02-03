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

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((eq? (car lat) old)
             (cons new lat))
            (else (cons (car lat)
                        (insertL new old (cdr lat)))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old)
        (cons new lat))
       (else (cons (car lat)
                   ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) old)
        (cons old (cons new (cdr lat))))
       (else (cons (car lat)
                   ((insertR-f test?) new old (cdr lat))))))))

;; insert-g implemented by myself
(define insert-g
  (lambda (position)
    (lambda (test?)
      (lambda (new old lat)
        (cond
         ((null? lat) '())
         ((test? (car lat) old)
          (cond
           ((eq? position 'left)
            (cons new lat))
           ((eq? position 'right)
            (cons old (cons new (cdr lat))))))
         (else (cons (car lat)
                     (((insert-g position) test?) new old (cdr lat)))))))))

(define insert-L (insert-g 'left))

((insert-L eq?) '4 '3 '(1 2 3 5 6))

(define seqL
  (lambda (new old lat)
    (cons new (cons old lat))))

(define seqR
  (lambda (new old lat)
    (cons old (cons new lat))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((eq? (car lat) old)
        (seq new old (cdr lat)))
       (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))

(define insert-R (insert-g seqR))

(insert-R '4 '3 '(1 2 3 5 6))

(define seqS
  (lambda (new old lat)
    (cons new lat)))

(define subst (insert-g seqS))

(subst '4 '3 '(1 2 3 4 5))

(define seqrem
  (lambda (new old lat)
    lat))
(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(rember 'sausage '(pizza with sausage and bacon))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define ox
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (ox n (sub1 m)))))))

(define o^
  (lambda (m n)
    (cond
     ((zero? n) 1)
     (else (ox m (o^ m (sub1 n)))))))



(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2st-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) o+)
     ((eq? x 'x) ox)
     (else o^))))

(define value-prefix
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (o+
       (value-prefix (1st-sub-exp nexp))
       (value-prefix (2st-sub-exp nexp))))
     ((eq? (operator nexp) 'x)
      (ox (value-prefix (1st-sub-exp nexp)) (value-prefix (2st-sub-exp nexp))))
     ((eq? (operator nexp) '^)
      (o^ (value-prefix (1st-sub-exp nexp)) (value-prefix (2st-sub-exp nexp))))
     )))

(value-prefix '13)
(value-prefix '(+ 1 3))
(value-prefix '(+ 1 (^ 3 4)))
