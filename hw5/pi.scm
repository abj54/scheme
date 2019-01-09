;;; file: pi_header.scm
;;;
;;; This should be included (textually) at the top of pi.scm.  All
;;; these definitions are from the textbook.

;;; cons-stream is already defined (by a macro, as a special form) in
;;; UMB Scheme

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.
;2.

(define (mult-stream m strm)

  (define (pow lst)
    (expt 10 (- (length lst) 1)))

  (define (rem num numlist)
    (remainder num (if (> (pow numlist) 1)
                       (pow numlist)
                       1)))
  
  (define (action a a-list i-strm)
    (cond ((stream-null? i-strm) (list->stream a-list))
      ;;produce
          ((and (not (null? a-list)) (< (+ m (rem a a-list)) 
                                                 (pow a-list)))
           (cons-stream (makenum (car a-list))
                                     (action (remainder a (pow
a-list)) (cdr a-list) i-strm)))
          (else
           ; consume
           (let ((n-a (+ (* 10 a) (* m (stream-car i-strm)))))
             (action n-a (add0 (string->list (number->string n-a))
a-list)
                     (stream-cdr i-strm))))))

  (define z (string->list (number->string 0)))
  (define zero (car z))

  (define (makenum cnum)
    (- (char->integer cnum) (char->integer zero)))
  
  (define (add0 l1 l2)
    (cond ((> (length l1) (length l2)) l1)
          (else 
           (add0 (append (list zero) l1) l2))))


  (define (list->stream lst)
    (if (null? lst)
        the-empty-stream
        (cons-stream (makenum (car lst)) (list->stream (cdr lst)))))

  (action 0 '() strm)
  )
                                        
    
    




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;4.
(define (pi)

  (define (makemat a b c d)
    (list a b c d))

  (define firstmat (makemat 1 6 0 3)) ;; first item
  
  (define strm  ;;final strm of composite matrices
    (cons-stream firstmat (add-streams addtomat strm)))


  (define (stream-map proc . argstreams) ;; stream map from earlier part
    (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


  (define addtomat  ;;stream of addition matrices
    (cons-stream (makemat 1 4 0 2)  addtomat))

;;selectors
(define (fel mat1) (car mat1))
(define (sel mat1) (cadr mat1))
(define (tel mat1) (caddr mat1))
(define (lel mat1) (cadddr mat1))
  
(define (addproc mat1 mat2) ;;additional procedure for 2 matrices
  (makemat (+ (fel mat1) (fel mat2))
           (+ (sel mat1) (sel mat2))
           (+ (tel mat1) (tel mat2))
           (+ (lel mat1) (lel mat2))
           ))

;;multiplication for 2 matrices
(define (compose mat1 mat2)
  (let ((a1 (fel mat1))        
        (a2 (sel mat1))
        (a3 (tel mat1))
        (a4 (lel mat1))
        (b1 (fel mat2))
        (b2 (sel mat2))
        (b3 (tel mat2))
        (b4 (lel mat2)))
    (makemat (+ (* a1 b1) (* a2 b3))
             (+ (* a1 b2) (* a2 b4))
             (+ (* a3 b1) (* a4 b3))
             (+ (* a3 b2) (* a4 b4)))))
             
(define (add-streams s1 s2)  ;; add 2 streams of matrices
  (stream-map addproc s1 s2))

(define (checkx mat1 x)  ; solve for x in matrix mat1
  (quotient (+ (* (fel mat1) x) (sel mat1)) (lel mat1))) 

(define (adjust n)  ;;shift matrix (adjust a matrix for value taken)
  (makemat 10 (- (* 10 n)) 0 1))


;;final procedure of action
  ;; consume + produce
  (define (action a strim)
    (let ((q1 (checkx a 3))
          (q2 (checkx a 4)))
      (cond ((= q1 q2) ;;produce
             (cons-stream q1 (action (compose (adjust q1) a) strim)))
            (else ;consume
             (action (compose a (stream-car 
                                   strim)) (stream-cdr strim))))))

;; launch it on first matrix
(action firstmat (stream-cdr strm))
)
