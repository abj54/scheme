(define (cons-stream exp env)
  (cons (cadr exp) (make-thunk (caddr exp) env)))

(define (stream-car strm) (car strm))
(define (stream-cdr strm) (force-thunk (cdr strm)))
(define the-empty-stream '())
(define (stream-null? strm) (eq? strm the-empty-stream))
