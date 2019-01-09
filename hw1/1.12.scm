
(define (p r e)
  (cond ((or (= e 0) (= e r)) 1)
      ;  ((= e r) 1)
        ((> e r) "Need: e<=r")
        (else (+ (p (- r 1) (- e 1)) (p (- r 1) e)))))



;(define pa (lambda (r e)
 ;           (cond ((= e 0) 1)
  ;                ((= e r) 1)
   ;               ((> e r) "Need: e<=r")
    ;                      (else (+ (p (- r 1) (- e 1)) (p (- r 1) e))))))
            

(p 0 0)
(p 0 2)
(p 2 1)
(p 4 2)
