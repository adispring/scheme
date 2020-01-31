(quote atom)
'atom

'turkey
1492
'u
'*abc$
'(atom)

'(atom turkey or)

'((atom turkey) or)

'(x y z)

'((x y) z)

'(how are you doing so far)

'()

'(() ())

;; car
(car '(a b c))
(car '((a b c) x y z))
(car '(((hotdogs)) (and) (pickle) relish))
(car '(((hotdogs)) (and)))
(cdr '(a b c))

;; cdr
(cdr '((abc) x y z))
(cdr '(hamburger))
(cdr '((x) t r))
(cdr 'a)
(cdr '())
(car (cdr '((b) (x y) ((c)))))
(cdr (cdr '((b) (x y) ((c)))))

;; cons
(cons 'peanut '(butter and jelly))
(cons '(banana and) '(peanut butter and jelly))
(cons '((help) this) '(is very ((hard) to learn)))
(cons '(a b (c)) '())
(cons 'a '())

(car (cons '((a b c)) 'b))
(cons '((a b c)) 'b)
(cons 'a 'b)

(cons 'a (car '((b) c d)))

;; null?
(null? '())
(null? (quote ()))
(null? '(a b c))
(null? 'a)

;; atom?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? 'Harry)
(atom? '(Harry had a heap of apples))
