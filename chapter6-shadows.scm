(quote a)
(quote +)
(quote x)

(eq? (quote a) 'a)
(eq? 'a 'a)

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) 'o+)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'ox)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'o^)
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
     (else #f))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
           (numbered? (car (cdr (cdr aexp)))))))))

(numbered? '42)
(numbered? '(42 o+ 42))
(numbered? '(5 o+ a))
(numbered? '(5 ox (3 o^ 2)))
(numbered? '(5 ox (3 foo 2)))
(numbered? '(5 ox (3 'foo 2)))               ; #f
(numbered? '((5 o+ 2) ox (3 o^ 2)))

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

(define value
  (lambda (aexp)
    (cond
     ((atom? aexp) aexp)
     ((eq? (car (cdr aexp)) '+)
      (o+ (value (car aexp))
         (value (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) 'x)
      (o* (value (car aexp))
         (value (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) '^)
      (o^ (value (car aexp))
            (value (car (cdr (cdr aexp)))))))))

(value '13)
(value '(1 + 3))
(value '(1 + (3 ^ 4)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) '+)
      (o+ (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) 'x)
      (ox (value (car (cdr nexp))) (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) '^)
      (o^ (value (car (cdr nexp))) (value (car (cdr (cdr nexp)))))))))

(value '13)
(value '(+ 1 3))
(value '(+ 1 (^ 3 4)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2st-sub-exp
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
      (o+
       (value-prefix (1st-sub-exp nexp))
       (value-prefix (2st-sub-exp nexp))))
     ((eq? (operator nexp) 'x)
      (ox (value-prefix (1st-sub-exp nexp)) (value-prefix (2st-sub-exp nexp))))
     ((eq? (operator nexp) '^)
      (o^ (value-prefix (1st-sub-exp nexp)) (value-prefix (2st-sub-exp nexp))))
     )))

(value-prefix1 '13)
(value-prefix1 '(+ 1 3))
(value-prefix1 '(+ 1 (^ 3 4)))

