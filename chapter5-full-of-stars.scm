(define atom?
  (lambda (a)
    (cond
     ((or (pair? a) (null? a)) #f)
     (else #t))))

(define atom?                                                                ;
  (lambda (x)                                                                 ;
    (and (not (pair? x)) (not (null? x)))))                                  ;

(atom? 'hello)
(atom? '())
(atom? '(hello))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else (cons (car l)
                   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else
        (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old (cons new (insertR* new old (cdr l)))))
       (else
        (cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (if (a) ((wood chuck)))
                          could chuck wood))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else
      (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet))
                  (banana)
                  (bread)
                  (banana brandy)))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'orange 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet))
                  (banana)
                  (bread)
                  (banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (cons old (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l))
                 (insertL* new old (cdr l)))))))

(insertL* 'pecker 'chuck '((how much (wood))
                           could
                           ((a (wood) chuck))
                           (((chuck)))
                           (if (a) ((wood chuck)))
                           (could chuck wood)))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) #t)
       (else (member* a (cdr l)))))
     (else
      (or (member* a (car l)) (member* a (cdr l)))))))

(member* 'chips '((potato) (chips ((with) fish)) (chips)))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish) (chips))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or  (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1)) (atom? (car l2)))
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
     ((or (atom? (car l1)) (atom? (car l2))) #f)
     (else (and
            (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '(hello) '(hello))
(eqlist? '(strawberry ice cream) '(strawberry ice cream))
(eqlist? '(strawberry ice cream) '(strawberry cream ice))

(eqlist? '(banana ((split))) '((banana) (split)))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))

(define equal?
  (lambda (a b)
    (cond
     ((and (atom? a) (atom? b)) (eqan? a b))
     ((or (atom? a) (atom? b)) #f)
     (else (eqlist1? a b)))))

(define eqlist1?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and
            (equal? (car l1) (car l2))
            (eqlist1? (cdr l1) (cdr l2)))))))

(eqlist1? '(banana ((split))) '((banana) (split)))
(eqlist1? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))))
(eqlist1? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
