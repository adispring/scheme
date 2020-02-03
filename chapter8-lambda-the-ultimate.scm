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

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value-prefix
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) '+)
      (o+ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
     ((eq? (operator nexp) 'x)
      (ox (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
     ((eq? (operator nexp) '^)
      (o^ (value-prefix (1st-sub-exp nexp))
          (value-prefix (2nd-sub-exp nexp))))
     (else #f))))

(value-prefix '13)
(value-prefix '(+ 1 3))
(value-prefix '(+ 1 (^ 3 4)))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) o+)
     ((eq? x 'x) ox)
     (else o^))))


(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
            (value (1st-sub-exp nexp))
            (value (2nd-sub-exp nexp)))))))

(value '13)
(value '(+ 1 3))
(value '(+ 1 (^ 3 4)))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a)
        ((multirember-f test?) a (cdr l)))
       (else (cons (car l)
                   ((multirember-f test?) a (cdr l))))))))

(define multirember-eq? (multirember-f eq?))

(define eq-3? (eq?-c '3))

(multirember-eq? '3 '(1 2 3 4 3 5 6))

(define multirember-T
  (lambda (eq?-a l)
    (cond
     ((null? l) '())
     ((eq?-a (car l))
      (multirember-T eq?-a (cdr l)))
     (else (cons (car l)
                 (multirember-T eq?-a (cdr l)))))))

(multirember-T eq-3? '(1 2 3 4 3 5))

;; 理解了这个，从能初步理解递归

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? (car lat) a)
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

(multirember&co 'tuna '() a-friend)

(multirember&co 'tuna '(tuna) a-friend)

(multirember&co 'tuna '(and tuna) a-friend)

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
     (else
      (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))
     )))

(multiinsertLR 'h '1 '2 '(1 3 5 7 2 4 6))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multirember&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat numberL numberR)
         (col
          (cons new (cons oldL newlat))
          (+ numberL 1)
          numberR))))
     ((eq? (car lat) oldR)
      (multirember&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat numberL numberR)
         (col
          (cons oldR (cons new newlat))
          numberL
          (+ numberR 1)))))
     (else
      (multirember&co
       new
       oldL
       oldR
       (cdr lat)
       (lambda (newlat numberL numberR)
         (col
          (cons (car lat) newlat)
          numberL
          numberR)))))))
