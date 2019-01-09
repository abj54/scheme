(begin

;; read-file produces a list whose elements are the expressions in the
;; file.

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


(define my-table (make-table))

(define (insert-data datalist table)
    (cond ((null? datalist) "Done")
          (else
           (let ((oneedge (car datalist)))
             (insert! (car oneedge) (cadr oneedge) (caddr oneedge)
table)
             (insert-data (cdr datalist) table)))))

(insert-data data my-table)
 ;----------------------------------------------------------------


;;these 2 return table of children
;; and list of children


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
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Major helper functions

;; find-cost returns a list of cost from node
;; to it's child list

(define (find-cost node clist table)
  (cond ((not (null? clist))
         (cons (singlecost node (car clist) table) (find-cost
node (cdr clist) table)))))
     

;; it is the computation part: calculates cost
;;from one node to end (calls main function for help)
(define (singlecost node child table)
  (cond ((equal? child 'end) (lookup node child table))
        (else (+ (lookup node child table) (naive-cost child)))))


;; returns minimum for a list
;;used for list returned by find-cost
(define (find-min lst)
  (cond ((null? (cdr lst)) (car lst))
        ((< (car lst) (find-min (cdr lst))) (car lst))
                  (else (find-min (cdr lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;main-function

;;takes a node: gets it's children and
;; and calls find-cost to return a list
;;of costs from node to end
;; find min returns the minimum

(define (naive-cost node)
  (let ((clist (get-child (get-childtable node my-table))))
    (cond ((null? clist) 1000000)
          (else
           (find-min (find-cost node clist my-table))))))
;; (append (list cost) min-list))))))
    

;; find minimum from start to end    
 (naive-cost 'start)       
  
)
