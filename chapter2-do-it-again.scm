;; lat?

(lat? '(Jack Sprat could eat no chicken fat))
(lat? '((Jack) Sprat could eat one chicken fat))
(lat? '(Jack (Sprat could) eat one chicken fat))
(lat? '())

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '(bacon and eggs))
(lat? '(bacon (and eggs)))

;; or

(or (null? '()) (atom? '(d e f g)))
(or (null? '(a b c)) (null? '(atom)))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(member? 'meat '(mashed potatoes and meat gravy))
(member? 'liver '(bagels and lox))
