(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 14)
(atom? -3)
(atom? 3.14159)

(define add1
  (lambda (n)
    (+ n 1)))

(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 0)

(zero? 0)
(zero? 1492)

(+ 46 12)

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(o+ 41 1)

(- 14 3)
(- 17 9)
(- 18 25)

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(o- 14 3)
(o- 17 9)
(o- 18 25)

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3 4 5))

(define ox
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (ox n (sub1 m)))))))

(ox 6 7)
(ox 6 0)
(ox 0 6)

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) (quote ()))
     (else (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2 3) '(4 5 6))

(define tup2+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (o+ (car tup1) (car tup2))
                 (tup2+ (cdr tup1) (cdr tup2)))))))

(tup2+ '(3 7) '(4 6 8 1))


(> 12 133)
(> 120 11)

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))

(o> 12 133)
(o> 120 11)
(o> 3 3)

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (o< (sub1 n) (sub1 m))))))

(o< 12 133)
(o< 120 11)
(o< 3 3)

(define o=
  (lambda (n m)
    (cond
     ((and (zero? m) (zero? n)) #t)
     ((or (zero? m) (zero? n)) #f)
     (else (o= (sub1 n) (sub1 m))))))

(o= 1 2)
(o= 11 11)

(define expt
  (lambda (m n)
    (cond
     ((zero? n) 1)
     (else (ox m (expt m (sub1 n)))))))

(expt 1 1)
(expt 2 3)
(expt 5 3)

(define div
  (lambda (m n)
    (cond
     ((o< m n) 0)
     (else (add1 (div (o- m n) n))))))

(div 15 4)
(div 16 4)

(length '(1 2))

(define olength
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (olength (cdr lat)))))))

(olength '(hotdogs with mustard sauerkraut and pickles))

(pick 4 '(1 2 3 4 5 6))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 4 '(1 2 3 4 5 6))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 4 '(1 2 3 4 5 6))

(number? 'tomato)
(number? 76)

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((number? (car lat)) (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat)))))))))

(no-nums '(5 pears 6 prunes 9 dates))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
            (else (all-nums (cdr lat))))))))

(all-nums '(5 pears 6 prunes 9 dates))

(eq? 1 1)

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? a (car lat)) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

(occur 'hello '(hello world hello))

(define one?
  (lambda (n)
    (cond
     ((zero? (sub1 n)) #t)
     (else #f))))

(one? 1)
(one? 2)

(define one?
  (lambda (n)
    (o= n 1)))

(one? 1)
(one? 2)

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 4 '(1 2 3 4 5 6))
