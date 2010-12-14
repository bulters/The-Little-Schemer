;;; plus
(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (plus n (sub1 m)))))))

;;; minus
(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (minus n (sub1 m)))))))

;;; addtup
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (plus (car tup) (addtup (cdr tup)))))))

;;; multiply
(define multiply
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (plus n (multiply n (sub1 m)))))))

;;; tup+
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) (quote ()))
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;;; > (gt)
(define gt
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (gt (sub1 n) (sub1 m))))))

;;; < (lt)
(define lt
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lt (sub1 n) (sub1 m))))))

;;; equals-raw
(define equals-raw
  (lambda (n m)
    (cond
     ((and (zero? n) (zero? m)) #t)
     ((zero? m) #f)
     (else (equals-raw (sub1 n) (sub1 m))))))

;;; equals-comp
(define equals-comp
  (lambda (n m)
    (cond
     ((or (gt n m) (lt n m)) #f)
     (else #t))))

;;; pow
(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (multiply n (pow n (sub1 m)))))))

;;; quotient
(define quotient
  (lambda (n m)
    (cond
     ((lt n m) 0)
     (else (add1 (quotient (- n m) m))))))

;;; length
(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

;;; pick
(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else
      (pick (sub1 n) (cdr lat))))))

;;; rempick
;(define rempick
;  (lambda (n lat)
;    (cond
;     ((zero? (sub1 n)) (cdr lat))
;     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;; rempick rewritten below using one? instead of (sub1 n)

;;; no-nums
(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (no-nums (cdr lat)))
     (else (cons (car lat) (no-nums (cdr lat)))))))

;;; all-nums
(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
     (else (all-nums (cdr lat))))))

;;; eqan? (equal atom or number)
(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2)) (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))

;;; occur
(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     ((eq? (car lat) a) (add1 (occur a (cdr lat))))
     (else (occur a (cdr lat))))))

;;; one?
(define one?
  (lambda (n)
    (= n 1)))

;;; rempick (take two)
(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))