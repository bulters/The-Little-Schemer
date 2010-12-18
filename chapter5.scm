;;; rember*
(define rember*
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((atom? (car lat)) (cond
                         ((eq? (car lat) a) (rember* a (cdr lat)))
                         (else (cons (car lat) (rember* a (cdr lat))))))
     (else
      (cons (rember* a (car lat)) (rember* a (cdr lat)))))))

;;; insertR*
(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (cond
                   ((eq? (car l) old) (cons (old (cons new (insertR* new old (cdr l))))))
                   (else (cons (car l) (insertR* new old (cdr l))))))
     (else
       (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;;; occur*
(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
                       ((eq? (car l) a) (add1 (occur* a (cdr l))))
                       (else (occur* a (cdr l)))))
     (else
      (+ (occur* a (car l)) (occur* a (cdr l)))))))

;;; subst*
(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old) (cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

;;; insertL*
(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l)) (or
                       ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
                       (cons (car l) (insertL* new old (cdr l)))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

;;; member*
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (cond
                       ((eq? (car l) a) #t)
                       (else (member* a (cdr l)))))
     (else (or (member* a (car l)) (member* a (cdr l)))))))

;;; leftmost
(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

;;; eqan?
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

;;; eqlist?
;; (define eqlist?
;;   (lambda (l1 l2)
;;     (cond
;;      ((and (null? l1) (null? l2)) #t)
;;      ((or (null? l1) (null? l2)) #f)
;;      ((and (atom? (car l1)) (atom? (car l2)) (eqan? (car l1) (car l2))) (eqlist? (cdr l1) (cdr l2)))
;;      ((or (atom? (car l1)) (atom? (car l2))) #f)
;;      (else
;;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

;;; equal?
(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

;;; eqlist? (refactor using equal)
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))

;;; rember
(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal? (car l) s) ((cdr l)))
     (else
      (cons (car l) (rember s (cdr l)))))))
