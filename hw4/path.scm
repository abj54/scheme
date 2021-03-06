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

(define cost-table (make-table))   ;; keep track of cost
(define my-rev-table (make-table)) ;; reverse table (end to start)
(define my-table (make-table))     ;; regular table for dist.dat
(define cheap-table (make-table))  ;; final cheapest table

(define (insert-rev-data datalist table)
  (cond ((null? datalist) "Done")
        (else
         (let ((oneedge (car datalist)))
           (insert! (cadr oneedge) (car oneedge) (caddr oneedge)
                    table)
                        (insert-rev-data (cdr datalist) table)))))

(define (insert-data datalist table)
    (cond ((null? datalist) "Done")
          (else
           (let ((oneedge (car datalist)))
             (insert! (car oneedge) (cadr oneedge) (caddr oneedge)
table)
             (insert-data (cdr datalist) table)))))


(insert-rev-data data my-rev-table)
(insert-data data my-table)
 ;----------------------------------------------------------------

;;this portion is almost identical to naive-path
;; except that I have employed memoization through cost-table

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
        


(define (find-cost node nodee clist table)
  (cond ((not (null? clist))
         (cons (singlecost node nodee (car clist) table) (find-cost
node nodee (cdr clist) table)))))
                                   ;


(define (singlecost node nodee child table)
  (cond ((equal? child nodee) (lookup node child table))
        (else (+ (lookup node child table) (naive-cost child nodee
table)))))


(define (find-min lst)
  (cond ((null? (cdr lst)) (car lst))
        ((< (car lst) (find-min (cdr lst))) (car lst))
                  (else (find-min (cdr lst)))))


(define (find-min-pair lst)
  (cond ((null? (cdr lst)) (car lst))
        ((< (cdar lst) (cdr (find-min-pair (cdr lst)))) (car lst))
        (else (find-min-pair (cdr lst)))))


(define (naive-cost node nodee table)
  (cond ((check node nodee cost-table) (check node nodee cost-table))
        (else
         (let ((clist (get-child (get-childtable node table))))
           (cond ((null? clist) (insert! nodee node 1000000
cost-table))
                 (else
                  (insert! nodee node
                      (find-min (find-cost node nodee clist table))
cost-table)))))))


(define (check node nodee table)
  (let ((value (lookup nodee node table)))
    (if value
        value
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cost is stored in cost-table for memization this make this new
;; naive-cost much faster


;;here i emplyes a logic from reverse table such that i know which
;;node it passes through and which node doesnot



;; helper for checkall (takes 1node and a table and checks if that
;; node is in the cheapest path or not
(define (checkcheap total node table)
  (cond ((lookup 'end (car node) table)
         (if (=
              (+ (cdr node) (lookup 'end (car node) table)) total)
             (insert! 'start (car node) 1 cheap-table)))))


;;recursive for all the nodes in a small table
(define (checkall stable table total)
  (cond ((not (null? stable))
         (checkcheap total (car stable) table)
         (checkall (cdr stable) table total))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;final function: forms cost-table from start to end and end to start
;;with straight and reverse table and checks for all the node if they
;;can be in the cheapest path or not

(define (cost node)
  (naive-cost node 'end my-table)
  (let ((total (naive-cost 'end node my-rev-table)))
    (let ((stable (get-childtable 'start cost-table)))
      (checkall stable cost-table (naive-cost 'start 'end my-table)))
    (append (list total 'start) (get-child (get-childtable node
cheap-table)) (list 'end))))

      

(cost 'start)
)
