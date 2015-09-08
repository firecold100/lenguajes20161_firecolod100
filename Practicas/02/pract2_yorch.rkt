#lang plai

;Ejercicio 1
(define-type Array
  [MArray (leng number?) (elements (listof number?))])

;(test (Array? (MArray 4 '(1 2 3))) #t)

;Ejercicio 2
(define-type MList
  [MEmpty]
  [MCons (value number?) (next MList?)])

;(test (MEmpty) (MEmpty))
;(test (MList? (MEmpty)) #t)
;(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
;(test (MList? (MCons 7 (MCons 4 (MCons 10 (MEmpty))))) #t)

;Ejercicio 3
(define-type NTree
  [TLEmpty]
  (NodeN (value number?) (sons (listof NTree?))))


;(NTree? (TLEmpty))
(NTree? (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))
;(NTree? (NodeN 1 (list (NodeN 2 (list (TLEmpty)))
;                        (NodeN 3 (list (TLEmpty)))
;                        (NodeN 4 (list (TLEmpty) (TLEmpty) (TLEmpty))))))

;Ejercicio 4
(define-type Position
  [2D-Point (x number?) (y number?)])

;(test (2D-Point 0 0) (2D-Point 0 0))
;(test (Position? (2D-Point 3 4.5)) #t)
;(test (2D-Point 1 (sqrt 2)) (2D-Point 1 1.4142135623730951))

;Ejercicio 5
(define-type Figure
  [Circle (point Position?) (radio number?)]
  [Square (point Position?) (len number?)]
  [Rectangle (point Position?) (height number?) (leng number?)])

;(Circle (2D-Point 2 2) 2)
;(Square (2D-Point 0 3) 3)
;(Rectangle (2D-Point 0 2) 2 3)

(define (coloca array position v count)
  (cond
    [(< count pos) (cons (car l) (coloca (cdr l) pos v (add1 count)))]
    [(> count pos) empty]
    [(= count pos) (cons v (cdr l))]))

