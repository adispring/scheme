(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else
      (lookup-in-entry-help name
                            (cdr names)
                            (cdr values)
                            entry-f)))))

(define new-entry build)

(lookup-in-entry 'entree
                 (new-entry '(appetizer entree beverage)
                            '(food tastes good))
                 (lambda (n) '()))

(lookup-in-entry 'dessert
                 (new-entry '(appetizer entree beverage)
                            '(food tastes good))
                 (lambda (n) '()))


(((appetizer entree beverage)
  (pate boeuf vin))
 ((beverage dessert)
  ((food is) (number one with us))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else
      (lookup-in-entry name
                       (car table)
                       (lambda (name)
                         (lookup-in-entry name (cdr table) table-f)))))))

(lookup-in-table 'entree
                 '(((entree dessert)
                            (spaghetti spumoni))
                           (appetizer entree beverage)
                           (food tastes good))
                 (lambda (n) '()))

(cons 'a
      (cons 'b
            (cons 'c
                  '())))

(cons 'car
      (cons (cons 'quote
                  (cons
                   (cons 'a
                         (cons 'b
                               (cons 'c
                                     (quote ()))))
                   (quote ())))
            (quote ())))

(car (quote (a b c)))

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

(value (quote (car (quote (a b c)))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e 'cons) *const)
     ((eq? e 'car) *const)
     ((eq? e 'cdr) *const)
     ((eq? e 'null?) *const)
     ((eq? e 'eq?) *const)
     ((eq? e 'atom?) *const)
     ((eq? e 'zero?) *const)
     ((eq? e 'add1) *const)
     ((eq? e 'sub1) *const)
     ((eq? e 'number?) *const)
     (else *identifier))))

                                        ; List to action
                                        ;
(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
     (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

