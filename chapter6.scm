;;; numbered?
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

;;; value
;; (define value
;;   (lambda (nexp)
;;     (cond
;;      ((atom? nexp) nexp)
;;      ((eq? (car (cdr nexp)) (quote +)) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
;;      ((eq? (car (cdr nexp)) (quote *)) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
;;      (else (expt (value (car nexp)) (value (car (cdr (cdr nexp)))))))))

;;; 1st-sub-exp
;; (define 1st-sub-exp
;;   (lambda (aexp)
;;     (car (cdr aexp))))

;;; 2nd-sub-exp
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

;;; operator
;; (define operator
;;   (lambda (aexp)
;;     (car aexp)))

;;; value
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))) (+ ( value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))
     ((eq? (operator nexp) (quote *))) (* ( value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))
     (else (expt (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp)))))))

;;; 1st-sub-exp
(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

;;; operator
(define operator
  (lambda (aexp)
    (car (cdr aexp))))

;;; sero?
(define sero?
  (lambda (n)
    (null? n)))

;;; edd1
(define edd1
  (lambda (n)
    (cons (quote ()) n)))

;;; zub1
(define zub1
  (lambda (n)
    (cdr n)))

;;; pluz
(define pluz
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (pluz n (zub1 m)))))))
