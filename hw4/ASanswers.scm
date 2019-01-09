;; Anwesh Joshi
;; HW4 Part I


;;; 1
;; Version 1

(define make-account-lambda
         (lambda (balance)
           (define withdraw
             (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds"))
             )
           (define deposit
             (lambda (amount)
               (set! balance (+ balance amount))
               balance))
           (lambda (m)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request -- MAKE-ACCOUNT"
                                m))))))




;;;Version 2  -- > dont have to call defined procedure
;; using cond makes it call only one thing straight away
(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (lambda (amount)
        (cond ((eq? m 'withdraw)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount)) balance)
                   "Insufficient funds"))
              ((eq? m 'deposit)
               (set! balance (+ balance amount)) balance)
                            (else (error "Unknown request -- MAKE
ACCOUNT" m)))))))


;;;Version 3
;;pretty much version 2 with some editing on lambda amount part
(define make-account-inline-factored
  (lambda (balance)
    (lambda (m)
      (lambda (amount)
        (cond ((eq? m 'withdraw)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount)) balance)
                   "Insufficient funds"))
            ((eq? m 'deposit)
               (set! balance (+ balance amount)) balance)
              (else (error "Unknown request -- MAKE ACCOUNT" m)))))))



    
;;;; 3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (mf)
      (cond ((equal? mf 'how-many-calls?) count)
            ((equal? mf 'reset-count) (set! count 0))
            (else (set! count (+ count 1)) (f mf))))))


;;(define s (make-monitored sqrt))
;;(s 100)
;;(s 'how-many-calls?)


;;; 3.4
;;; used first version. password is the passed in value
;; and the message is only checked when the password provided
;; is correct or else not error message is given regarding
;; wrong password

(define make-pw-account
  (lambda (balance password)    
    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
      )
    (define deposit
      (lambda (amount)
        (set! balance (+ balance amount))
        balance))
    (lambda (try m)
      (cond ((equal? try password)
             (cond ((eq? m 'withdraw) withdraw)
                   ((eq? m 'deposit) deposit)
                   (else (error "Unknown request -- MAKE-ACCOUNT"
                                m))))
           (else (error "Incorrect password " try))))))
            
;(define acc (make-pw-account 100 'bikesh))
;((acc 'asd 'withdraw) 40)
;((acc 'bikesh 'deposit) 50)
;((acc 'bikesh 'withdraw) 10)
