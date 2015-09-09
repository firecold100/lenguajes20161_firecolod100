#lang plai

;Ejercicio 1
(define-type Array
  [MArray (leng number?) (elements list?)])
;(test (Array? (MArray 4 '(1 2 3))) #t)

;Ejercicio 2
(define-type MList
  [MEmpty]
  [MCons (value or/c) (next MList?)])

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

(define ar (MArray 5 '(0 0 0 0 0)))
(setvalueA ar 2 5)

;; Ejercicio 7 MArray2MList
;Dado un arreglo de tipo MArray, regresar una lista de tipo MList que contenga todos los
;elementos del arreglo original
(define (MArray2MList array)
  (cond
    [(empty? (MArray-elements array)) (MEmpty)]
    [else (MCons (car (MArray-elements array)) (MArray2MList (MArray (- (MArray-leng array) 1) (cdr (MArray-elements array)))))]))

;(define array1 (MArray 0 '()))
;(test (MArray2MList array1) (MEmpty))
;(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
  
  
; Ejercicio 8 printML
(define (printML lst)
  (cond
    [(MEmpty? lst) "[]"]
    [(MList? lst) (string-append "[" (saca lst))]))

(define (saca lst)
  (cond
    [(MEmpty? (MCons-next lst)) (string-append (~a (MCons-value lst)) "]")]
    [else (string-append (~a (MCons-value lst)) ", " (saca (MCons-next lst)))]))
(printML (MEmpty))
(printML (MCons 7 (MEmpty)))
(printML (MCons 7 (MCons 4 (MEmpty))))

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
    [(MList? lst) (+ 1 (lengthML (MCons-next lst)))]
    [else (error "No es del tipo MList")]))

;(lengthML (MEmpty))
;(lengthML (MCons 7 (MCons 4 (MEmpty))))

;; Ejercicio 11 mapML

;; Ejercicio 12 filterML

;; Definamos los siguientes tipos de datos y valores
(define-type Coordinates
  [GPS (lat number?)
       (long number?)])

(define-type Location
  [building (name string?)
            (loc GPS?)])

;; Coordenadas GPS
(define gps-satelite (GPS 19.510482 -99.23411900000002))
(define gps-ciencias (GPS 19.3239411016 -99.179806709))
(define gps-zocalo (GPS 19.432721893261117 -99.13332939147949))
(define gps-perisur (GPS 19.304135 -99.19001000000003))
(define plaza-satelite (building "Plaza Satelite" gps-satelite))
(define ciencias (building "Facultad de Ciencias" gps-ciencias))
(define zocalo (building "Zocalo" gps-zocalo))
(define plaza-perisur (building "Plaza Perisur" gps-perisur))

;; Ejercicio 13 haversine

;; Ejercicio 14 gps-coordinates

;; Ejercicio 15 closest-building

;; Ejercicio 16 buildings-at-distance

;; Ejercicio 17
(define (area x)
  (cond 
    [(Circle? x) (* pi (* (Circle-radio x) (Circle-radio x)))] ;pi * r^2
    [(Square? x) (* (Square-len x) (Square-len x))] ;cara * cara
    [(Rectangle? x)(*(Rectangle-leng x)(Rectangle-height x))]));base*altura

;; Ejercicio 18 in-figure?