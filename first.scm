(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? (quote ()))
(atom? 'atom)
(atom? 'turkey)
(atom? 1492)
(atom? 'u)
(atom? '*abc$)
(atom? '(atom))
(atom? '(atom turkey or))

(pair? '(atom turkey or))
(pair? '('(atom turkey) or))
(pair? '())
(atom? '())

(car (car '(((hotdogs)) (and) (pickle) (relish))))

