;Anwesh Joshi
;CS450
;HW 2

;;;  1
;;; follows with definition: empty list is list and
;;; a pair whose cdr is list is also a list

(define (is-list? object)
  (cond ((null? object) #t)
        ((pair? object) (is-list? (cdr object)))
        (else #f)))




;;;  2
;;; To reverse a list, I used recursion to add all the elements
;;; from end to a list such that first element goes to end and
;;;last goes to first
(define (my-reverse object)
  (cond ((null? object) '())
        (else (append (my-reverse (cdr object)) (list (car object))))))
  

;;;  3
;;; I used filter provided from the lecture for this problem.
;;; With filter and the remainder property to identify the first
;;; remainder, it was just the matter of going through the entire
;;; list to get the desired elements from list.

(define (filter pred seq)
  (cond ((null? seq) '())    
        ((pred (car seq))
         (cons (car seq) (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(define (same-parity x . y)
  (define rem (even? x))
  (cons x (filter  (lambda (z) (equal? rem (even? z))) y)))

;;(same-parity  2 3 4)
         
     
;;; 4

;;; if list is null, return empty list. Else, use tail-recursion
;;; to square first element and join it with the cdr of list.

(define (square-list items)
  (if (null? items)
      '()
      (cons ((lambda (x) (* x x))
             (car items)) (square-list (cdr items)))))

;;(square-list (list 1 2 3 4))

;;; it is pretty simple to use map as all the elements are mapped
;;; to it's square

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;;(square-list (list 1 2 3 4))

;;; 5
;;; my-for-each was a bit confusing as it kept printing
;;; an empty list at the end, which I later figured out
;;; is pretty normal with scheme. If the list is not null,
;;; call the procedure on the first element and recursively
;;; call it on rest of the element as well.

(define (my-for-each proc items)
  (cond ((not (null? items))
         (proc (car items)) (my-for-each proc (cdr items)))))
         
;;(my-for-each (lambda (x) (newline) (display x)) (list 1 2 3))


;;; 6

;; Did myself

;;; 7
;;; if one list is null, we need to have second list null as well to
;;; be equal. Or else, we will compare 1st element from each list
;;; and call my-equal method recursively on all the other elements
;;; until one of them is null

(define (my-equal? l1 l2)
  (cond ((null? l1) (null? l2))
        ((null? l2) #f)
        ((eqv? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2)))
        (else #f)))

;(my-equal? '(this is a list) '(this is a list))
;(my-equal? '(this is a list) '(this (is a) list))



;;; 8
;;; a.
;;; Recursively go through the entire list and check if the
;;; element satisfies the predicate or not.

(define (every? pred seq)
  (cond ((null? seq) #t)  
        ((pred (car seq)) (every? pred (cdr seq)))  
        (else #f) ))  

;;; b.
;;; From the definition of list, an empty list is still a
;;; list. It must meet the condition set by predicate i.e.
;;; should be true. The empty list should meet all the
;;; predicate values.

;;(define listA (list 2 3 4 5 6))
;;(define listB (list 12 14 1 31 4 6))

;;; 9
;;; It is pretty similar to intersection, only that the
;;; common element is not to be added to a new list. We will
;;; just return one of the existing list by adding the elements
;;; not in that list but earlier list.

(define (element-of? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of? x (cdr set)))))

(define (unordered-union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((element-of? (car set1) set2)
         (unordered-union-set (cdr set1) set2))
        (else (cons (car set1)
                    (unordered-union-set (cdr set1) set2)))))

;(unordered-union-set (list 2 3 5 4 8) (list 5 6 7))


;;; 10
;;; We compare each element of list and append that element
;;; to one of the list that we then return.

(define (ordered-union-set set1 set2)
  (cond ((and (null? set1 ) (null? set2)) '())
      ((null? set1) set2)
      ((null? set2) set1)
      (else (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1
               (ordered-union-set (cdr set1) (cdr set2))))
              ((< x1 x2) (cons x1
                           (ordered-union-set (cdr set1) set2)))
              ((> x1 x2) (cons x2
                           (ordered-union-set set1 (cdr set2)))))))))

;(ordered-union-set (list 2 3 5 4 8) (list 5 6 7))
;(ordered-union-set (list 1 2 3 11 12) (list 2 3 4 5 7 15))



;;; 11
;;; As long as the element is equal to the element in the list,
;;; we recursively call remove on rest of elements. But if it
;;; doesnot match it, call cons on that element and remove val
;;; on rest of the elements.

(define (remove-val x list)
  (cond ((null? list) '())
        (( = x (car list)) (remove-val x (cdr list) ))
        (else (cons  (car list) (remove-val x (cdr list))) )))
 

;(remove-val 7 '(7 2 3 4 2 2 7))
