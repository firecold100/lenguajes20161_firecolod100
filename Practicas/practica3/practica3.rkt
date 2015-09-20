#lang plai

(require "practica3-base.rkt")
;(print-only-errors true)
;Ejercicio 1 zones
(define (zones rest max)
  (define (aux i range rest lstzones);Begin of auxiliar function
    (cond
      [(= i 0) (let ([lst (append lstzones (list (resting rest (- (+ rest (* range (+ 0.5 (* 0.1 i)))) 1))))])
                 (aux (+ i 1) range rest lst))]
      [(= i 1 )(let ([lst (append lstzones (list (warm-up (+ rest (* range (+ 0.5 (* 0.1 (- i 1)))))  
                                         (- (+ rest (* range (+ 0.5 (* 0.1 i)))) 1))))])
                 (aux (+ i 1) range rest lst))]
      [(= i 2 )(let ([lst (append lstzones (list (fat-burning (+ rest (* range (+ 0.5 (* 0.1 (- i 1)))))  
                                         (- (+ rest (* range (+ 0.5 (* 0.1 i)))) 1))))])
                 (aux (+ i 1) range rest lst))]
      [(= i 3 )(let ([lst (append lstzones (list (aerobic (+ rest (* range (+ 0.5 (* 0.1 (- i 1)))))  
                                         (- (+ rest (* range (+ 0.5 (* 0.1 i)))) 1))))])
                 (aux (+ i 1) range rest lst))]
      [(= i 4 )(let ([lst (append lstzones (list (anaerobic (+ rest (* range (+ 0.5 (* 0.1 (- i 1)))))  
                                         (- (+ rest (* range (+ 0.5 (* 0.1 i)))) 1))))])
                 (aux (+ i 1) range rest lst))]
      [(= i 5 )(let ([lst (append lstzones (list (maximum (+ rest (* range (+ 0.5 (* 0.1 (- i 1)))))  
                                         (+ rest (* range (+ 0.5 (* 0.1 i)))) )))])
                 lst)]));End of auxiliar function
  (cond
    [(not (number? rest)) error "The first param is not of type number"]
    [(not (number? max)) error "The second param is not of type number"]
    [else (aux 0 (- max rest) rest '())]))

;;Tests
(test (zones 50 180) (list
 (resting 50 114.0)
 (warm-up 115.0 127.0)
 (fat-burning 128.0 140.0)
 (aerobic 141.0 153.0)
 (anaerobic 154.0 166.0)
 (maximum 167.0 180.0)))
(test (zones 60 160) (list
 (resting 60 109.0)
 (warm-up 110.0 119.0)
 (fat-burning 120.0 129.0)
 (aerobic 130.0 139.0)
 (anaerobic 140.0 149.0)
 (maximum 150.0 160.0)))
(test (zones 50 170) (list
 (resting 50 109.0)
 (warm-up 110.0 121.0)
 (fat-burning 122.0 133.0)
 (aerobic 134.0 145.0)
 (anaerobic 146.0 157.0)
 (maximum 158.0 170.0)))
(test (zones 40 200) (list
 (resting 40 119.0)
 (warm-up 120.0 135.0)
 (fat-burning 136.0 151.0)
 (aerobic 152.0 167.0)
 (anaerobic 168.0 183.0)
 (maximum 184.0 200.0)))
(test (zones 60 130) (list
 (resting 60 94.0)
 (warm-up 95.0 101.0)
 (fat-burning 102.0 108.0)
 (aerobic 109.0 115.0)
 (anaerobic 116.0 122.0)
 (maximum 123.0 130.0)))

;Ejercicio 2 get-zone
(define my-zones (zones 50 180));Constant defined for the exercise

(define (get-zone sym lst)
  (cond
    [(not (symbol? sym)) error "The first param is not a symbol"]
    [(empty? lst) error "The symbol is not in my-zones list"]
    [(and (eq? 'resting sym) (resting? (car lst))) (car lst)]
    [(and (eq? 'warm-up sym) (warm-up? (car lst))) (car lst)]
    [(and (eq? 'fat-burning sym) (fat-burning? (car lst))) (car lst)]
    [(and (eq? 'aerobic sym) (aerobic? (car lst))) (car lst)]
    [(and (eq? 'anaerobic sym) (anaerobic? (car lst))) (car lst)]
    [(and (eq? 'maximum sym) (maximum? (car lst))) (car lst)]
    [else (get-zone sym (cdr lst))]))

;;Tests

(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180.0))

;Ejercicio 3 bmp->zone

(define (bpm->zone lstf lstz)
  (define (aux1 frec lst)
   (cond
     [(empty? lst) "No coincidences found"]
     [(and (resting? (car lst)) 
           (<= (resting-low (car lst)) frec) 
           (>= (resting-high (car lst)) frec)) (car lst)]
     [(and (warm-up? (car lst)) 
           (<= (warm-up-low (car lst)) frec) 
           (>= (warm-up-high (car lst)) frec)) (car lst)]
     [(and (fat-burning? (car lst)) 
           (<= (fat-burning-low (car lst)) frec) 
           (>= (fat-burning-high (car lst)) frec)) (car lst)]
     [(and (aerobic? (car lst)) 
           (<= (aerobic-low (car lst)) frec) 
           (>= (aerobic-high (car lst)) frec)) (car lst)]
     [(and (anaerobic? (car lst)) 
           (<= (anaerobic-low (car lst)) frec) 
           (>= (anaerobic-high (car lst)) frec)) (car lst)]
     [(and (maximum? (car lst)) 
           (<= (maximum-low (car lst)) frec) 
           (>= (maximum-high (car lst)) frec)) (car lst)]
     [else (aux1 frec (cdr lst))]))
  (define (aux2 lstf lstz mzones) ;lstf:- list of frecuencies. lstz:-list of zones. mzones:- list of zones of coincidence
    (cond
        [(empty? lstf) '()]
        [else (append (list (aux1 (car lstf) lstz)) (aux2 (cdr lstf) lstz mzones) )]))
  (cond
    [(not (list? lstf)) error "The first param is not a list"]
    [(not (list? lstz)) error "The second param is not a list"]
    [else (aux2 lstf lstz '())]))

;;Tests
(test (bpm->zone '() '()) '())
(test (bpm->zone '(50 117 130 150) my-zones) 
      (list (resting 50 114.0) 
            (warm-up 115.0 127.0) 
            (fat-burning 128.0 140.0) 
            (aerobic 141.0 153.0)))
(test (bpm->zone '(50 60 70 80) my-zones)
      (list (resting 50 114.0) 
            (resting 50 114.0) 
            (resting 50 114.0) 
            (resting 50 114.0)))
(test (bpm->zone '(50 60 70 80) '())
      '("No coincidences found" 
        "No coincidences found" 
        "No coincidences found" 
        "No coincidences found"))
(test (bpm->zone '(180) my-zones)
      (list (maximum 167.0 180.0)))

;Ejercicio 4 create trackpoints
(define (create-trackpoints raw-data-lst zones-lst)
  (define (create-trackpoint raw zones-lst)
    (trackpoint (GPS (first (second raw)) (second (second raw))) (third raw) (car (bpm->zone (list (third raw)) zones-lst)) (first raw)))
  (cond
    [(empty? raw-data-lst) empty]
    [else (append (list (create-trackpoint (car raw-data-lst) zones-lst)) 
                  (create-trackpoints (cdr raw-data-lst) zones-lst))]))
;;Tests
(test (create-trackpoints (take raw-data 1) my-zones)
      (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)))
(test (create-trackpoints (take raw-data 2) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)))
(test (create-trackpoints (take raw-data 4) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (create-trackpoints (take raw-data 5) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)))
(test (create-trackpoints (cdr (cdr (cdr (take raw-data 9)))) my-zones)
      (list
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.490702 -99.2413217) 115 (warm-up 115.0 127.0) 1425619678)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (warm-up 115.0 127.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (warm-up 115.0 127.0) 1425619685)))

;Ejercicio 5 total-distance
(define (degtorad n)
  [* n (/ pi 180)])

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
                 [a (* (sin (/ deltalat 2)) (sin (/ deltalat 2)) )];square of sin(Δlat/2)
                 [t1 (cos lat1)] ;Cosine of latitude 1
                 [t2 (cos lat2)] ; Cosine of latitude 2
                 [t3 (* (sin (/ deltalong 2)) (sin (/ deltalong 2)))] ;square of sin(Δlong/2)
                 [t (* t1 t2 t3)]; multiplication of t1, t2 and t3
                 [b (+ a t)] ;Sum of a and t
                 [r 6367] ;Radius of earth
                 [raiz (sqrt b)] ;The "big root" in formula
                 [result (* 2 r (asin (sqrt b)))])
            result)]))

(define (total-distance lst)
  (define (distancias gps1 gps-lst)
    (cond
      [(empty? gps-lst) 0]
      [else (+ (haversine (trackpoint-loc gps1) (trackpoint-loc (car gps-lst))) (distancias (car gps-lst) (cdr gps-lst)))]))
  (cond
    [(empty? lst) 0]
    [else (distancias (car lst) (cdr lst))]))

(test (total-distance (create-trackpoints (take raw-data 100) my-zones)) 0.9509291243812747)
(test (total-distance (create-trackpoints raw-data my-zones)) 5.051934549322941)
(test (total-distance (create-trackpoints (take raw-data 10) my-zones)) 0.057104023456293194)
(test (total-distance (create-trackpoints (take raw-data 1) my-zones)) 0)
(test (total-distance (create-trackpoints (take raw-data 4) my-zones)) 0.00785990254990468)

;Ejercicio 6 avarage-hr

;Ejercicio 7 max-hr

;Ejercicio 8 collapse-trackpoints
(define (collapse-trackpoints lst e)
  (define (collapse pas lst trackp e)
    (cond
      [(empty? lst) (append (list trackp) pas)]
      [(if (and (< (haversine (trackpoint-loc (car lst)) (trackpoint-loc trackp)) e) (eq? (trackpoint-hr (car lst)) (trackpoint-hr trackp)))
            (append pas lst)
            (collapse (append pas (list (car lst))) (cdr lst) trackp e))]))
  (cond
    [(empty? lst) empty]
    [else (collapse '() (collapse-trackpoints (cdr lst) e) (car lst) e)]))

;;Tests
(test (collapse-trackpoints (create-trackpoints (take raw-data 4) my-zones) 0.01)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (collapse-trackpoints (create-trackpoints (take raw-data 2) my-zones) 0.7)
      (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)))
(test (collapse-trackpoints '() 0) '())
(test (collapse-trackpoints (create-trackpoints (take raw-data 3) my-zones) 0.21)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)))
(test (collapse-trackpoints (create-trackpoints (take raw-data 10) my-zones) 1.1)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (warm-up 115.0 127.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (warm-up 115.0 127.0) 1425619685)
       (trackpoint (GPS 19.4906861 -99.2415517) 119 (warm-up 115.0 127.0) 1425619690)))


;Ejercicio 9 ninBT
(define (ninBT tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type tree"]
    [(EmptyBT? tree) 0]
    [(and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree))) 0]
    [else (+ 1 (ninBT (BNode-l tree)) (ninBT (BNode-r tree)))]
    )
  )
;;Tests
(test (ninBT arb1) 0)
(test (ninBT arb2) 1)
(test (ninBT arb3) 3)
(test (ninBT arb4) 7)
(test (ninBT arbol-base) 5)

;Ejercicio 10 nlBT

(define (nlBT tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type BTree"]
    [(EmptyBT? tree) 0]
    [(and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree))) 1]
    [else (+ (nlBT (BNode-l tree)) (nlBT(BNode-r tree)) )]))

;;Tests
(test (nlBT arb1) 1)
(test (nlBT arb2) 2)
(test (nlBT arb3) 4)
(test (nlBT arb4) 8)
(test (nlBT arbol-base) 4)

;Ejercicio 11 nnBT

(define (nnBT tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type BTree"]
    [(EmptyBT? tree) 0]
    [(and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree))) 1]
    [(EmptyBT? (BNode-l tree)) (+ 1 (nnBT (BNode-r tree)))]
    [(EmptyBT? (BNode-l tree)) (+ 1 (nnBT (BNode-l tree)))]
    [else  (+ (nnBT (BNode-l tree)) (nnBT (BNode-r tree))  1) ]))

(test (nnBT arb1) 1)
(test (nnBT arb2) 3)
(test (nnBT arb3) 7)
(test (nnBT arb4) 15)
(test (nnBT arbol-base) 9)

;Ejercicio 12 mapBT

(define (mapBT fun tree)
  (cond
    [(EmptyBT? tree) (EmptyBT)]
    [else (BNode < (mapBT fun (BNode-l tree)) (fun (BNode-e tree)) (mapBT fun (BNode-r tree)))]))

;;Tests
(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT))))
      (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT))))
      (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT (lambda (x) (* x 2)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT))))
      (BNode < (EmptyBT) 6 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT sqrt (BNode < (BNode < (EmptyBT) 4 (EmptyBT)) 9 (BNode < (EmptyBT) 16 (EmptyBT))))
      (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 3 (BNode < (EmptyBT) 4 (EmptyBT))))

;Ejercicio 13 in-order, pre-order, pos-order.

;;in-order

(define (in-order tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type BTree"]
    [(EmptyBT? tree) '()]
    [else (append (in-order (BNode-l tree)) (list (BNode-e tree)) (in-order (BNode-r tree)))]))


;;per-order
(define (pre-order tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type BTree"]
    [(EmptyBT? tree) '()]
    [else (append (list (BNode-e tree)) (pre-order (BNode-l tree)) (pre-order (BNode-r tree)))]))


;;pos-order
(define (pos-order tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type BTree"]
    [(EmptyBT? tree) '()]
    [else (append (pos-order (BNode-l tree)) (pos-order (BNode-r tree))  (list (BNode-e tree)) )]))