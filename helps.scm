;; numberal operators

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

(define o/
  (lambda (m n)
    (cond
     ((o< m n) 0)
     (else (add1 (div (o- m n) n))))))

(define o^
  (lambda (m n)
    (cond
     ((zero? n) 1)
     (else (ox m (o^ m (sub1 n)))))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))
