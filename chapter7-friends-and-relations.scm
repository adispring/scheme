(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((eq? (car l) a) #t)
     (else (member? a (cdr l))))))

(define set?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((member? (car l) (cdr l)) #f)
     (else (set? (cdr l))))))

(set? '(apple peaches apple plum))
(set? '(apple peaches plum))
(set? '(apply 3 pear 4 9 apply 3 4))
(set? '(apply 3 pear 9 apple 4))

(define makeset
  (lambda (l)
    (cond
     ((null? l) '())
     ((member? (car l) (cdr l)) (makeset (cdr l)))
     (else (cons (car l) (makeset (cdr l)))))))

(makeset '(apple peach pear peach
                 plum apple lemon peach))


(makeset '(apple 3 pear 4 9 apple 3 4))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2)
                (subset? (cdr set1) set2))))))

(subset? '(5 chicken wings)
         '(5 hamburgers
             2 pieces fried chicken and
             light duckling wings))

(subset? '(4 pounds of horseradish)
         '(four pounds chicken and
                5 ounces horseradish))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(eqset? '(6 large chickens with wings) '(6 chickens with large wings))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     ((or (member? (car set1) set2)
          (intersect? (cdr set1) set2))))))

(intersect? '(1 2 3) '(3 4 5))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(intersect '(1 2 3) '(3 4 5 2))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(union '(1 2 3) '(2 3 4 5))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '((1 2 3) (2 3) (3)))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(a-pair? '(2 7))
(a-pair? '(() (pair)))
(a-pair? '(2))

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

(define firsts
  (lambda (l)
    (cond
     ((null? l) (quote ()))
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (build (second (car rel))
                        (first (car rel)))
                 (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(revpair '(8 a))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(revrel '((8 a) (pumpkin pie) (got sick)))

(define fullfun?
  (lambda (rel)
    (set? (firsts (revrel rel)))))

(fullfun? '((8 a) (pumpkin pie) (got sick) (9 a)))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(one-to-one? '((8 a) (pumpkin pie) (got sick) (9 a)))
(one-to-one? '((8 a) (pumpkin pie) (got sick)))
