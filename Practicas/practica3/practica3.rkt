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

#|(test (zones 50 180) (list
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

;Ejercicio 4 create trackpoints

;Ejercicio 5 total-distance

;Ejercicio 6 avarage-hr

;Ejercicio 7 max-hr

;Ejercicio 8 collapse-trackpoints

;Ejercicio 9 ninBT

;Ejercicio 10 nlBT

;Ejercicio 11 nnBT

;Ejercicio 12 mapBT

;Ejercicio 13 in-order, pre-order, pos-order.
