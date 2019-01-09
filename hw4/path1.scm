;(begin

;; read-file produces a list whose elements are the expressions in the file.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go:  read in the file that defines the graph

  (define data (with-input-from-file "dist.dat" read-file))

  ;_________________________________________________________________

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (lookup-child key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              record
              #f))
        #f)))


(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  value) ;;; maybe val would be better than ’ok

(define (make-table) (list 'table))
;_________________________________________________________________

(define cost-table (make-table))
(define my-rev-table (make-table))
(define my-table (make-table))
(define cheap-cost-table (make-table))

(define (insert-data datalist table)
    (cond ((null? datalist) "Done")
          (else
           (let ((oneedge (car datalist)))
             (insert! (car oneedge) (cadr oneedge) (caddr oneedge)
table)
             (insert-data (cdr datalist) table)))))

(define (insert-rev-data datalist table)
  (cond ((null? datalist) "Done")
        (else
         (let ((oneedge (car datalist)))
           (insert! (cadr oneedge) (car oneedge) (caddr oneedge)
                    table)
           (insert-rev-data (cdr datalist) table)))))



(insert-rev-data data my-rev-table)
(insert-data data my-table)
 ;----------------------------------------------------------------

(define min-list '())


(define (get-childtable node table)
  (let ((subtable (assoc node (cdr table))))
    (if subtable
        (cdr subtable)
        '())))
        

(define (get-child ctable)
  (cond ((not (null? ctable))
         (append  (list (caar ctable))
         (get-child (cdr ctable))))
        (else '())))
        

;(define (get-cost ctable)
;  (cond ((not (null? ctable))
;         (append  (list (car ctable))
;                  (get-cost (cdr ctable))))
;                (else '())))


(define (find-cost node clist table)
  (cond ((not (null? clist))
         (cons (singlecost node (car clist) table) (find-cost node (cdr clist) table)))))
         ;;(find-cost node (cdr clist) table))
                                   ;


(define (singlecost node child table)
  (cond ((equal? child 'end) (lookup node child table))
        (else (+ (lookup node child table) (naive-cost child)))))
       


(define (find-min lst)
  (cond ((null? (cdr lst)) (car lst))
        ((< (car lst) (find-min (cdr lst))) (car lst))
                  (else (find-min (cdr lst)))))


(define (find-min-pair lst)
  (cond ((null? (cdr lst)) (car lst))
        ((< (cdar lst) (cdar (find-min-pair (cdr lst)))) (car lst))
                          (else (find-min-pair (cdr lst)))))



(define (naive-cost node)
  (let ((val (lookup 'end node cost-table)))
    (if val
        val
        (begin
  (let ((clist (get-child (get-childtable node my-table))))
    (cond ((null? clist) 1000000)
          (else
            (find-min  (find-cost node clist my-table))
               )))))))
;; (append (list cost) min-list))))))
    
(define (cheap-path node clist)
 ;; (let ((clist (get-child (get-childtable node my-rev-table))))
(cond ((not (null? clist))
  (let ((nde (car clist)))
    (insert! node nde (naive-cost nde) cost-table)
    (cheap-path node (cdr clist))))
      (else
       (insert! node nde 1000000 cost-table))))
))

(define (rec-cheap-path node)
  (let ((clist (get-child (get-childtable node my-rev-table))))
    (cheap-path node clist)))

(define (keepdoing node)
  (cond ((not (equal? node 'start))
         (rec-cheap-path node)
         (let ((vtable  (get-childtable node cost-table)))
           (let ((cheap (car (find-min-pair vtable))))
             
           
           
         
            
  
;)
