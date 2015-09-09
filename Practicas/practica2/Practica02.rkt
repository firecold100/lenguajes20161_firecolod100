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
;(NTree? (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))
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

;Ejercicio 6
(define (setvalueA array position value)
  (if (not(Array? array))
      (error 'setvalueA "Unknown Type")
      (if (> position (- (MArray-leng array) 1))
          (error 'setvalueA "Out of bounds")
          (coloca (MArray-elements array) position value 0))))

;funcion auxiliar
(define (coloca array position value count)
  (cond
    [(< count position) (cons (car array) (coloca (cdr array) position value (add1 count)))]
    [(> count position) empty]
    [(= count position) (cons value (cdr array))]))

;(define ar (MArray 5 '(0 0 0 0 0)))
;(setvalueA ar 2 5)

;Ejercicio 9
(define (concatML lst1 lst2)
  (cond
    [(MEmpty? lst1) lst2]
    [(MEmpty? lst2) lst1]
    [else (MCons (MCons-value lst1) (concatML (MCons-next lst1) lst2))]))

;Ejercicio 10
(define (lengthML lst)
  (cond
    [(MEmpty? lst) 0]
    [else (+ 1 (lengthML (MCons-next lst)))]))

  