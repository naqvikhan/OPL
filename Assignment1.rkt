
#lang racket

(provide (all-defined-out))
;;________________________________________
;;
;;
;; Programming Assignment 1 (CS 4337.001)
;;________________________________________

;; ________________________________________________________________
;; This function takes two integer args, and checks whether the
;; second one is evenly divisible by the first one

;1. divisible-by-x?

(define ((divisible-by-x? q) r)
  (if (= 0 (remainder q r))
      #t
      #f))
;; _______________________________________________________________
;; This fuction passes 9 as a function argument
;2. function-9

(define function-9
(lambda (integer)
(integer 9)))

;;________________________________________________________________
;; This function duplicates the functionality of the map from str
;; library
;3. my-map

(define (my-map funct list_of_vals)
    
    (cond ;;setting a condition to test    
        [(empty? list_of_vals) empty] ;;empty is returned when the list is empty
        [else (cons (funct (first list_of_vals)) ;;the values are appended to the list
        (my-map funct (rest list_of_vals)))]))

;;__________________________________________________________________
;; two list arguments are paired up with each corresponding value
;; in this function
;4 pair-up

(define (pair-up list_1 list_2)

  ;; check whether both the lists are empty, then return empty
  (cond ((null? list_1) '()) 
    ((null? list_2) '())
    
    (else
      ;; to obtain the first element from both lists
      (cons (list (car list_1) (car list_2))
      ;; function is called recursively to pair up thr obtained values
      (pair-up (cdr list_1) (cdr list_2))))))

;;_____________________________________________________________________
;; This function splits the items in a list as per their classification
;5 classify

(define (classify boolean main_list)
(cond ((null? main_list) '())
((equal? boolean even?) (list (even main_list) (odd main_list)))
((equal? boolean real?) (list (real main_list) (non-real main_list)))
((equal? boolean integer?) (list (integer main_list) (non-integer main_list)))))

;; A sub-function to find even elements 

(define (even main_list)
(cond ((null? main_list) '())
((even? (car main_list)) (cons (car main_list) (even (cdr main_list))))
(else (even (cdr main_list)))))

;; A sub-function to find odd elements 

(define (odd main_list)
(cond ((null? main_list) '())
((odd? (car main_list)) (cons (car main_list) (odd (cdr main_list))))
(else (odd (cdr main_list)))))

;;A sub-function to find real elements 

(define (real main_list)
(cond ((null? main_list) '())
((real? (car main_list)) (cons (car main_list) (real (cdr main_list))))
(else (real (cdr main_list)))))

;;A sub-function to find non-real elements 

(define (non-real main_list)
(cond ((null? main_list) '())
((real? (car main_list)) (non-real (cdr main_list)))
(else (cons (car main_list) (non-real (cdr main_list))))))

;;A sub-function to find integer elements 

(define (integer main_list)
(cond ((null? main_list) '())
((integer? (car main_list)) (cons (car main_list) (integer (cdr main_list))))
(else (integer (cdr main_list)))))

;;A sub-function to find non-integer elements 

(define (non-integer main_list)
(cond ((null? main_list) '())
((integer? (car main_list)) (non-integer (cdr main_list)))
(else (cons (car main_list) (non-integer (cdr main_list))))))

;; __________________________________________________________________________
;; This function takes two args, an expression and a list and returns true if
;; the expression is part of the list
;6 is-member?

(define (is-member? expr list)
    (if (null? list)
       #f
    (if (equal? expr (car list))
       #t
        (is-member? expr (cdr list)))))

;;___________________________________________________________________________
;; This function checks whether a list is sorted or not and returns true only
;; the comparator and the sorted list match
;7 my-sorted?

(define (my-sorted? compr list)
  (if (<= (length list)1)
      #t
      (and (compr (car list) (cadr list)) (my-sorted? compr (cdr list)))))

;;___________________________________________________________________________
;; This function performs the flatten functionality from the std lib, i.e.
;; removes all nesting to one arbitary level of nesting
;8 my-flatten

 (define (my-flatten arg_lst)
 (cond ((null? arg_lst) '())
 ((pair? arg_lst)
 (append (my-flatten (car arg_lst)) (my-flatten (cdr arg_lst))))
 (else (list arg_lst))))

;;__________________________________________________________________________
;;This function take an integer and a list and returns all the elements
;; in the list upto the limit of the integer
;9 upper-threshold

(define (upper-threshold arg_lst limit)
(cond ((null? arg_lst) '()) ;;returns empty if given list is empty
;;append to list if first element is less than threshold
((< (car arg_lst) limit) (cons (car arg_lst) (upper-threshold (cdr arg_lst) limit)))
;;to recursively call the next elements
(else (upper-threshold (cdr arg_lst) limit)))) 

;;____________________________________________________________________________
;; This function takes two args, an integer (index) and a list. It returns the
;; element specified by the integer (index) in the list
;10 my-list-ref

(define (my-list-ref arg_lst index)
(cond ((null? arg_lst) 'ERROR:Index-out-of-bounds) ;; Error message when index is out of bounds
((= index 0) (car arg_lst)) ;;return first element when index is 0
(else (my-list-ref (cdr arg_lst) (- index 1))))) ;; looks for specified index element by recursive call
