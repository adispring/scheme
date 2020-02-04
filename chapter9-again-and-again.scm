
(define pick
  (lambda (n l)
    (cond
     ((zero? (- n 1)) (car l))
     (else (pick (- n 1) (cdr l))))))

(pick 6 '(6 2 4 caviar 5 7 3))

(define keep-looking
  (lambda (a n lat)
    (cond
     ((number? n)
      (keep-looking a (pick n lat) lat))
     (else (eq? n a)))))

(keep-looking 'caviar 6 '(6 2 4 caviar 5 7 3))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))

(define eternity
  (lambda (x)
    (eternity x)))

(eternity 4)

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

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(shift '((a b) c))
(shift '((a b) (c d)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

;; 部分函数：可能陷入死循环的函数

(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (length* (first pora))
         (length* (second pora)))))))

(length* '((a b) c))
(length* '(a (b c)))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (* (weight* (first pora)) 2)
         (weight* (second pora)))))))

(weight* '((a b) c))
(weight* '(a (b c)))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l))))
     )))

(length '(1 2 3 4))

;; length0

(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1 (eternity (cdr l))))))

;; length<=1

(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1
     ((lambda(l)
        (cond
         ((null? l) 0)
         (else
          (add1 (eternity (cdr l))))))
      (cdr l))))))

;; All these programs contain a function that looks like length.
;; Perhaps we should abstract out this function.

;; length0

((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 eternity)

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  eternity) '())

;; length<=1
                                        ;
((lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (g (cdr l)))))))
  eternity))

(((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
   eternity)) '())

((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

(((lambda (mk-length)
    (mk-length eternity))
  (lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))) '())

((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1
              ((mk-length eternity)
               (cdr l))))))))
 '(apples))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1
             ((mk-length mk-length)
              (cdr l))))))))

(((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1
              ((mk-length mk-length)
               (cdr l)))))))) '(1 2 3 4 5))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (mk-length mk-length))
   ))



