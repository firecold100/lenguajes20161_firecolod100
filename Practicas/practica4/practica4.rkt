#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

;;Ejercicio 1 desugar
(define (desugar expr)
  (type-case FAES expr
    [idS (s) (id s)]
    [numS (n) (num n)]
    [withS (l b) (app (fun (map (lambda (x) (bind-name x)) l)
                                     (desugar b))
                                (list (desugar (bind-val (car l)))))]
    [with*S (l b) (app (fun (map (lambda (x) (bind-name x)) l)
                                     (desugar b))
                                (map (lambda (x) (desugar (bind-val x))) l))]
    [funS (p b) (fun p (desugar b))]
    [appS (f a) (app (desugar f) (map desugar a))]
    [binopS (o l r) (binop o 
                           (desugar l)
                           (desugar r))]))


(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))

(define (cparse sexp)
  (desugar (parse sexp)))

;;Ejercicio 2 multi-param Adecuar el interp para que sea multiparametrico.

;;Ejercicio 3 with*

(define (opbin op l r)
  (cond
    [(numV (op (numV-n l) (numV-n r)))]))
(define (lookup name env)
  (env name))
;;Ejercicio 4 interp
(define (interp expr env)
  (type-case FAE expr
  [num (n) (numV n)]
  [id (name) (lookup name env)]
  [fun (params body) (closureV params body env)]
  [app (fun args) '()]
  [binop (f l r) (opbin f (rinterp l) (rinterp r))]))

(define (rinterp expr)
  (interp expr (mtSub)))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))
