#lang plai

;Ejercicio 1
(define-type Array
  [MArray (leng number?) (elements list?)])

;(Array? (MArray 4 '(1 2 3)))
;(test (Array? (MArray 4 '(1 2 3))) #t)
(test (MArray 3 '(1 2 3 )) (MArray 3 '(1 2 3 ))) 
(test (MArray 5 '(1 2 3 "a" 1 )) (MArray 5 '(1 2 3 "a" 1))) 
(test (MArray 1 '("a" )) (MArray 1 '("a"))) 
(test (MArray 0 '()) (MArray 0 '())) 
(test (MArray 2 '("a" "b" )) (MArray 2 '("a" "b" ))) 

;Ejercicio 2
(define-type MList
  [MEmpty]
  [MCons (value any/c) (next MList?)])

;(MList? (MEmpty))
;(MList? (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MEmpty) (MEmpty))
(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MCons "a" (MCons "b" (MEmpty))) (MCons "a" (MCons "b" (MEmpty))))
(test (MCons "a" (MCons "1" (MEmpty))) (MCons "a" (MCons "1" (MEmpty))))
(test (MCons "a"(MEmpty))(MCons "a" (MEmpty)))

;Ejercicio 3
(define-type NTree
  [TLEmpty]
  (NodeN (value number?) (sons (listof NTree?))))

;(NTree? (TLEmpty))
;(NTree? (NodeN 1 (list (TLEmpty) (TLEmpty) (TLEmpty))))
;(NTree? (NodeN 1 (list (NodeN 2 (list (TLEmpty)))
;                        (NodeN 3 (list (TLEmpty)))
;                        (NodeN 4 (list (TLEmpty) (TLEmpty) (TLEmpty))))))

(test [TLEmpty] [TLEmpty])
(test (NodeN 1 (list(NodeN 2(list (NodeN 2(list(TLEmpty)))))))
      (NodeN 1 (list(NodeN 2(list (NodeN 2(list(TLEmpty))))))))
(test (NodeN 0 (list (NodeN 2(list (TLEmpty))) (NodeN 2(list (TLEmpty))) (NodeN 2(list (TLEmpty))))) 
      (NodeN 0 (list (NodeN 2(list (TLEmpty))) (NodeN 2(list (TLEmpty))) (NodeN 2(list (TLEmpty))))))
(test (NodeN 1 (list (TLEmpty))) (NodeN 1 (list (TLEmpty))))
(test (NodeN 1 (list (NodeN 2 (list (NodeN 2 (list(TLEmpty)))))))
      (NodeN 1 (list (NodeN 2 (list (NodeN 2 (list(TLEmpty))))))))

;Ejercicio 4
(define-type Position
  [2D-Point (x number?) (y number?)])

;(Position? (2D-Point 0 0))
;(Position? (2D-Point 1 (sqrt 2)))
(test (2D-Point 0 0) (2D-Point 0 0))
(test (2D-Point 5 0) (2D-Point 5 0))
(test (2D-Point 5 -10) (2D-Point 5  -10))
(test (2D-Point 0 4) (2D-Point 0 4))
(test (2D-Point 1 (sqrt 2)) (2D-Point 1 1.4142135623730951))

;Ejercicio 5
(define-type Figure
  [Circle (point Position?) (radio number?)]
  [Square (point Position?) (len number?)]
  [Rectangle (point Position?) (height number?) (leng number?)])

;(Figure? (Circle (2D-Point 2 2) 2))
;(Figure? (Square (2D-Point 0 3) 3))
;(Figure? (Rectangle (2D-Point 0 2) 2 3)
(test (Circle (2D-Point 2 2) 2) (Circle (2D-Point 2 2) 2))
(test (Circle (2D-Point 0 0) 1) (Circle (2D-Point 0 0) 1))
(test (Square (2D-Point 0 3) 3) (Square (2D-Point 0 3) 3))
(test (Rectangle (2D-Point -1 2) 2 3) (Rectangle (2D-Point -1 2) 2 3))
(test (Rectangle (2D-Point 0 0) 2 5) (Rectangle (2D-Point 0 0) 2 5))

;Ejercicio 6
(define (setvalueA array position value)
  (cond
    [(MArray? array) (if (> position (- (MArray-leng array) 1))
          (error 'setvalueA "Out of bounds")
          (MArray (MArray-leng array) (cambia (MArray-elements array) position value 0)))]
    [else (error "No es del tipo MArray")]))

;auxiliar function to change the value at position given
(define (cambia array position value count)
  (cond
    [(< count position) (cons (car array) (cambia (cdr array) position value (add1 count)))]
    [(> count position) empty]
    [(= count position) (cons value (cdr array))]))

(define ar (MArray 5 '(0 0 0 0 0)))
(test (setvalueA ar 2 5) (MArray 5 '(0 0 5 0 0)))
(test (setvalueA ar 4 2) (MArray 5 '(0 0 0 0 2)))
(test (setvalueA (MArray 5 '(1 1 1 1 1)) 1 3) (MArray 5 '(1 3 1 1 1)))
(test (setvalueA (MArray 7 '(1 1 1 1 1 0 0)) 6 3) (MArray 7 '(1 1 1 1 1 0 3)))
(test/exn (setvalueA (MArray 3 '(0 1 1)) 4 3) "setvalueA: Out of bounds")

;; Ejercicio 7 MArray2MList
;Let give an MArray returns a MList with all parameters from the original array
(define (MArray2MList array)
  (cond
    [(empty? (MArray-elements array)) (MEmpty)]
    [else (MCons (car (MArray-elements array)) (MArray2MList (MArray (- (MArray-leng array) 1) (cdr (MArray-elements array)))))]))

(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
(test (MArray2MList (MArray 3 '(1 2 3))) (MCons 1 (MCons 2 (MCons 3(MEmpty)))))
(test (MArray2MList (MArray 5 '("a" "b" 1 2))) (MCons "a" (MCons "b" (MCons 1 (MCons 2 (MEmpty))))))
(test (MArray2MList (MArray 4 '(0 () "hola" 3))) (MCons 0 (MCons '() (MCons "hola" (MCons 3 (MEmpty))))))
  
; Ejercicio 8 printML
(define (printML lst)
  (cond
    [(MEmpty? lst) "[]"]
    [(MList? lst) (string-append "[" (saca lst))]))

(define (saca lst)
  (cond
    [(MEmpty? (MCons-next lst)) (string-append (~a (MCons-value lst)) "]")]
    [else (string-append (~a (MCons-value lst)) ", " (saca (MCons-next lst)))]))

(test (printML (MEmpty)) "[]")
(test (printML (MCons 7 (MEmpty))) "[7]")
(test (printML (MCons 7 (MCons 4 (MEmpty)))) "[7, 4]")
(test (printML (MCons "a" (MCons 4 (MEmpty)))) "[a, 4]")
(test (printML (MCons "a" (MCons 4 (MCons "Hola" (MEmpty))))) "[a, 4, Hola]")

;Ejercicio 9
(define (concatML lst1 lst2)
  (cond
    [(MEmpty? lst1) lst2]
    [(MEmpty? lst2) lst1]
    [else (MCons (MCons-value lst1) (concatML (MCons-next lst1) lst2))]))

(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MCons 10 (MEmpty)))) (MCons 7 (MCons 4 (MCons 1 (MCons 10 (MEmpty))))))
(test (concatML (MCons 1 (MEmpty)) (MEmpty)) (MCons 1 (MEmpty)))
(test (concatML (MEmpty) (MCons "a" (MEmpty))) (MCons "a" (MEmpty)))
(test (concatML (MCons 1 (MEmpty)) (MCons "a" (MEmpty))) (MCons 1 (MCons "a" (MEmpty))))

;Ejercicio 10
(define (lengthML lst)
  (cond
    [(MEmpty? lst) 0]
    [(MList? lst) (+ 1 (lengthML (MCons-next lst)))]
    [else (error "No es del tipo MList")]))

(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)
(test (lengthML (MCons "a" (MEmpty))) 1)
(test (lengthML (MCons 7 (MCons 4 (MCons 5 (MCons 6(MEmpty)))))) 4)
(test (lengthML (MCons 7 (MCons 4 (MCons "a" (MCons "b" (MCons 1 (MEmpty))))))) 5)

;; Ejercicio 11 mapML
(define (mapML fun lst)
  (cond 
    [(MEmpty? lst) (MEmpty)]
    [else[MCons (fun [MCons-value lst]) (mapML fun (MCons-next lst))]]))

(test (mapML add1 (MCons 7 (MCons 4 (MEmpty)))) (MCons 8 (MCons 5 (MEmpty))))
(test (mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 100 (MCons 9 (MEmpty))))
(test (mapML sqrt (MCons 4 (MCons 9 (MEmpty)))) (MCons 2 (MCons 3 (MEmpty))))
(test (mapML car (MCons '(1 2) (MEmpty))) (MCons 1 (MEmpty)))
(test (mapML cdr (MCons '(1 2) (MEmpty))) (MCons '(2) (MEmpty)))
     
;; Ejercicio 12 filterML
;define the below data types and values
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

(define plazas (MCons plaza-satelite (MCons plaza-perisur (MEmpty))))
(define plazas1 (MCons ciencias (MCons plaza-perisur (MEmpty))))
(define plazas2 (MCons ciencias (MCons zocalo (MEmpty))))
(define plazas3 (MCons plaza-perisur (MCons plaza-satelite (MEmpty))))

;;Auxiliar function
;; it takes a number given in degrees and returns the number given in radians
;; (number)->(number)
(define (degtorad n)
  [* n (/ pi 180)])

;; Ejercicio 13 haversine
(define (haversine gps1 gps2)
  (cond
    [(not (GPS? gps1)) error "The first param is not of type GPS" ]
    [(not (GPS? gps2)) error "The second param is not of type GPS" ]
    [else (let* ([lat1 (degtorad (GPS-lat gps1))] ;We convert from degrees to radians the param lat of "the object" gps1
                 [long1 (degtorad (GPS-long gps1))] ;We convert from degrees to radians the param long of "the object" gps1
                 [lat2 (degtorad(GPS-lat gps2))] ;We convert from degrees to radians the param lat of "the object" gps2
                 [long2 (degtorad(GPS-long gps2))] ;We convert from degrees to radians the param long of "the object" gps2
                 [deltalat (- lat2 lat1)] ;Difference between two latitudes
                 [deltalong (- long2 long1)] ;Difference between two lengths
                 [a (* (sin (/ deltalat 2)) (sin (/ deltalat 2)) )];square of sin(deltalat/2)
                 [t1 (cos lat1)] ;Cosine of latitude 1
                 [t2 (cos lat2)] ; Cosine of latitude 2
                 [t3 (* (sin (/ deltalong 2)) (sin (/ deltalong 2)))] ;square of sin(deltalong/2)
                 [t (* t1 t2 t3)]; multiplication of t1, t2 and t3
                 [b (+ a t)] ;Sum of a and t
                 [r 6367] ;Radius of earth?
                 [raiz (sqrt b)] ;The "big root" in formula
                 [result (* 2 r (asin (sqrt b)))])
            result)]))

(test (haversine gps-ciencias gps-zocalo) 13.033219276117368)
(test (haversine gps-ciencias gps-perisur) 2.44727738966455)
(test (haversine gps-satelite gps-perisur) 23.391736010506026)
(test (haversine gps-satelite gps-zocalo) 13.644610757254002)
(test (haversine gps-ciencias gps-satelite) 21.496697489798446)

;; Ejercicio 14
(define (gps-coordinates lst)
  (cond 
    [(MEmpty? lst)(MEmpty)]
    [else[MCons (building-loc(MCons-value lst))(gps-coordinates (MCons-next lst))]]))

(test (gps-coordinates (MEmpty)) (MEmpty))
(test (gps-coordinates plazas) 
      (MCons (GPS 19.510482 -99.23411900000002) (MCons (GPS 19.304135 -99.19001000000003) (MEmpty))))
(test (gps-coordinates plazas1)
(MCons (GPS 19.3239411016 -99.179806709) (MCons (GPS 19.304135 -99.19001000000003) (MEmpty))))
(test (gps-coordinates plazas2) 
      (MCons (GPS 19.3239411016 -99.179806709) (MCons (GPS 19.432721893261117 -99.13332939147949) (MEmpty))))
(test (gps-coordinates plazas3)
      (MCons (GPS 19.304135 -99.19001000000003) (MCons (GPS 19.510482 -99.23411900000002) (MEmpty))))


;; Ejercicio 15 closest-building

;; Ejercicio 16 buildings-at-distance

;; Ejercicio 17
(define (area x)
  (cond 
    [(Circle? x) (* pi (* (Circle-radio x) (Circle-radio x)))] ;pi * r^2
    [(Square? x) (* (Square-len x) (Square-len x))] ;length * length
    [(Rectangle? x)(*(Rectangle-leng x)(Rectangle-height x))]));base*height

(test (area (Circle (2D-Point 5 5) 4)) 50.26548245743669)
(test (area (Square (2D-Point 0 0) 20)) 400)
(test (area (Rectangle (2D-Point 3 4) 5 10)) 50)
(test (area (Square (2D-Point 5 6) 5)) 25)
(test (area (Rectangle (2D-Point 1 4) 10 30)) 300)

; Ejercicio 18 in-figure?
(define (distancia punto1 punto2)
    (sqrt (+ (expt (- (2D-Point-x punto2) (2D-Point-x punto1)) 2) (expt (- (2D-Point-y punto2) (2D-Point-y punto1)) 2))))

(define (in-figure? figura punto)
  (cond
    [(Square? figura) (and (esta-x (Square-point figura) (Square-len figura) punto) (esta-y (Square-point figura) (Square-len figura) punto))]
    [(Rectangle? figura) (and (esta-x (Rectangle-point figura) (Rectangle-leng figura) punto) (esta-y (Rectangle-point figura) (Rectangle-height figura) punto))]
    [(Circle? figura) (< (distancia (Circle-point figura) punto) (Circle-radio figura))]))

(define (esta-x punto1 distancia punto2)
  (and (>= (+ (2D-Point-x punto1) distancia) (2D-Point-x punto2)) (<= (2D-Point-x punto1) (2D-Point-x punto2))))
(define (esta-y punto1 distancia punto2)
  (and (<= (- (2D-Point-y punto1) distancia) (2D-Point-y punto2)) (>= (2D-Point-y punto1) (2D-Point-y punto2))))

(test (in-figure? (Circle (2D-Point 0 0) 3) (2D-Point 4 0)) #f)
(test (in-figure? (Circle (2D-Point 0 1) 5) (2D-Point 2 1)) #t)
(test (in-figure? (Square (2D-Point 3 4) 4) (2D-Point 1 1)) #f)
(test (in-figure? (Rectangle (2D-Point 2 2) 2 4) (2D-Point 3 0)) #t)
(test (in-figure? (Rectangle (2D-Point 2 2) 2 4) (2D-Point 1 1)) #f)
