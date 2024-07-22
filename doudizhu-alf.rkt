;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname doudizhu-alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;Q4
;; ************************************************************************************
;; Mohammad Naael Ahmed
;; 21109392
;; QUESTION 4
;; ************************************************************************************'

;; -------------------------------------------------------------------------------------


;;(A)

 
(define (hand->ccal lst)
  (local [(define (occurence elem)
            (length (filter (lambda (x) (equal? elem x)) lst)))
          (define remove-duplicate (foldr (lambda (x y)
                                            (cons x (filter (lambda (z) (not (equal? z x))) y)))
                                          empty lst))
          (define each-occurence  (map (lambda (x) (occurence x)) remove-duplicate))]
    (map (lambda  (x y) (list x y)) remove-duplicate each-occurence)))

;;tests
 (check-expect (hand->ccal (list 3 3 3 7 'Jack 'Queen))
               (list (list 3 3) (list 7 1) (list 'Jack 1) (list 'Queen 1)))
;;------------------------------------------------------------------------------------

;;(B)


(define (find-kind num ccal)
  (local
    [(define (pred? ccal/elem)
       (>= (second ccal/elem) num))]
  (map (lambda (lst) (first lst)) (filter pred? ccal))))

;;tests
(check-expect (find-kind 2 (list (list 3 3) (list 4 2) (list 'Jack 1)))
              (list 3 4))
;;------------------------------------------------------------------------------------

;;(C)
;; trios from AO5
(define (trios tri/ccal)
  (local
    [(define (triosfinder ccalgiven)
     (>= (second ccalgiven) 3))]
    (map (lambda (lst) (list (first lst) (first lst) (first lst))) (filter triosfinder tri/ccal))))

;;tests
(check-expect
(trios (list (list 3 3) (list 4 1) (list 5 3) (list 6 4) (list 7 3) (list 8 2) (list 8 1)
            (list 'Queen 2)))
(list (list 3 3 3) (list 5 5 5) (list 6 6 6) (list 7 7 7)))
;;------------------------------------------------------------------------------------

;;(D)
;; card-foldr-n: (Card Nat X -> X) Card X Nat -> X
;; Requires: Nat >= 0
(define (card-foldr-n combine card base n)
(cond [(zero? n) base]
[else (combine card n (card-foldr-n combine card base (sub1
n)))]))

;;examples
 (check-expect (ccal->hand (list (list 3 3) (list 7 1) (list 'Jack 1) (list 'Queen 1)))
               (list 3 3 3 7 'Jack 'Queen))



(define (ccal->hand handccal)
  (foldr (lambda (x rror2) (append (card-foldr-n
                    (lambda (y z rror) (cons y rror)) (first x) empty (second x)) rror2))
         empty handccal))

(check-expect (ccal->hand (list (list 3 3) (list 4 1) (list 5 3) (list 6 4) (list 7 3)
                                (list 8 2) (list 8 1)
            (list 'Queen 2)))
              (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 8 'Queen 'Queen))
;;------------------------------------------------------------------------------------

;;(E)

(define (remove-one-of-each bighand)
  (cond
    [(empty? bighand) empty]
    [else (foldr (lambda (x rror) (cond
                               [(empty? rror) (cons x rror)]
                               [(equal? x (first rror)) (cons x rror)]
                               [else (cons x (rest rror))])) empty bighand)]))
;;------------------------------------------------------------------------------------

;;(F)
(define (remove-one-of-each2 loh)
  (ccal->hand (map (lambda (x)
           (list (first x) (sub1 (second x)))) (hand->ccal loh))))


;;examples
(check-expect (remove-one-of-each2 (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 8 'Queen 'Queen))
              (list 3 3 5 5 6 6 6 7 7 8 8 'Queen))

