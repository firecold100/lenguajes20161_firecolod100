#lang plai

(require "practica3-base.rkt")

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

#|
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
|#

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

#|
(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180.0))
|#

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
     [else (aux1 frec (cdr lst))]
    ))
  (define (aux2 lstf lstz mzones) ;lstf:- list of frecuencies. lstz:-list of zones. mzones:- list of zones of coincidence
    (cond
        [(empty? lstf) '()]
        [else (append (list (aux1 (car lstf) lstz)) (aux2 (cdr lstf) lstz mzones) )]
     ))
  (cond
    [(not (list? lstf)) error "The first param is not a list"]
    [(not (list? lstz)) error "The second param is not a list"]
    [else (aux2 lstf lstz '())]))

;;Tests
#|
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
|#

;Ejercicio 4 create trackpoints

;Ejercicio 5 total-distance

;Ejercicio 6 avarage-hr

;Ejercicio 7 max-hr

;Ejercicio 8 collapse-trackpoints

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
#|
(test (ninBT arb1) 0)
(test (ninBT arb2) 1)
(test (ninBT arb3) 3)
(test (ninBT arb4) 7)
(test (ninBT arbol-base) 5)
|#

;Ejercicio 10 nlBT

(define (nlBT tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type BTree"]
    [(EmptyBT? tree) 0]
    [(and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree))) 1]
    [else (+ (nlBT (BNode-l tree)) (nlBT(BNode-r tree)) )]))

;;Tests
#|
(test (nlBT arb1) 1)
(test (nlBT arb2) 2)
(test (nlBT arb3) 4)
(test (nlBT arb4) 8)
(test (nlBT arbol-base) 4)
|#

;Ejercicio 11 nnBT

(define (nnBT tree)
  (cond
    [(not (BTree? tree)) error "The first param is not of type BTree"]
    [(EmptyBT? tree) 0]
    [(and (EmptyBT? (BNode-l tree)) (EmptyBT? (BNode-r tree))) 1]
    [(EmptyBT? (BNode-l tree)) (+ 1 (nnBT (BNode-r tree)))]
    [(EmptyBT? (BNode-l tree)) (+ 1 (nnBT (BNode-l tree)))]
    [else  (+ (nnBT (BNode-l tree)) (nnBT (BNode-r tree))  1) ]))

#|
(test (nnBT arb1) 1)
(test (nnBT arb2) 3)
(test (nnBT arb3) 7)
(test (nnBT arb4) 15)
(test (nnBT arbol-base) 9)
|#

;Ejercicio 12 mapBT

;Ejercicio 13 in-order, pre-order, pos-order.
