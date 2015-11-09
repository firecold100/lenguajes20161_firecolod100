#lang plai

(require "practica5-base.rkt")

(print-only-errors true)

(define (desugar expr)
  (type-case RCFAELS expr
    [idS (s) (id s)]
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [MListS (l) (MList (desugarMlist l))]
    [withS (l b) (app (fun (map (lambda (x) (bind-name x)) l)
                                     (desugar b))
                                (map (lambda (x) (desugar (bind-val x))) l))]
    [with*S (l b) (withsmulti l b)]
    [funS (params body) (fun params 
                             (desugar body))]
    [appS (f args) (app (desugar f) 
                        (map(lambda (arg) (desugar arg)) args))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [recS (name named-expr body) (rec name                
                                   (desugar named-expr)
                                   (desugar body))]
    [ifS (t v f) (ifR (desugar t)
                      (desugar v)
                      (desugar f))]
    [equal?S (prim seg) (equal?R (desugar prim) (desugar seg))]
    [unopS (f arg) (unop f (desugar arg))]    
    ))

(define (desugarMlist l)
  (cond
    [(MEmptyS? l) (MEmpty)]
    [(MCons? (desugar (MConsS-mcar l)) (desugarMlist (MConsS-mcdr l)))]
    [else "error"]))


(define (withsmulti bindings body)
  (cond
    [(if (empty? bindings)
         (desugar body)
         (app (fun (list (bind-name (car bindings)))
                    (withsmulti (cdr bindings) body)) 
               (list (desugar (bind-val (car bindings))))))]))


(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))

(define (cparse sexp)
  (desugar (parse sexp)))

(define (opbin op l r env)
  (cond
    [(numV (op (numV-n (interp l env)) (numV-n (interp r env))))]))

(define (lookup name env)
  (cond
    [(mtSub? env) (error 'lookup "x symbol is not in the env")]
    [(aSub? env) (if (symbol=? (aSub-name env) name) 
                     (aSub-value env)
                     (lookup name (aSub-env env)))]))

(define (creaEnv params args env closureEnv)
  (if (empty? args)
      closureEnv
      (creaEnv (cdr params)
               (cdr args)
               env
               (aSub (car params)
                     (interp (car args) env)
                     closureEnv))))
;;Ejercicio 4 interp
;;Ejercicio 2 multi-param Adecuar el interp para que sea multiparametrico.
(define (interp expr env)
  (type-case RCFAEL expr
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [id (name) (lookup name env)]
    [fun (params body) (closureV params body env)]
    [app (fun args) (local([define fun-val (interp fun env)])
                    (interp (closureV-body fun-val)
                            (creaEnv (closureV-param fun-val) 
                                     args 
                                     env 
                                     (closureV-env fun-val))))]
    [binop (f l r) (opbin f l r env)]
    [unop (op arg) ((eligeUn op) (interp arg env))]
    [ifR (test truth falsity)
         (if (boolV-b (interp test env))
             (interp truth env)
             (interp falsity env))]
    [equal?R (prim seg) (error 'interp "no implementado")]
    [rec (bound-id named-expr bound-body) (error 'interp "no implementado")] 
    [MList (l) (error 'interp "no implementado")]))

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
(test/exn (rinterp (cparse '{{fun {x y} y} 3 {+ 2 x}})) "x symbol is not in the env")

(test (rinterp (cparse 'true)) (boolV true))
(test (rinterp (cparse 'false)) (boolV false))
(test (rinterp (cparse '{MList {MEmpty}})) (MListV (MEmptyV)))
(test (rinterp '{MList {MCons {+ 2 3} {MCons {+ 5 10} {MEmpty}}}}) (MListV (MConsV (numV 5) (MConsV (numV 15) (MEmptyV)))))
(test (rinterp '{MList {MCons {or true false} {MEmpty}}}) (MListV (MConsV (boolV #t) (MEmptyV))))
(test (rinterp '{MList {MCons {with* {{x 10} {x 20}} x} {MEmpty}}}) (MListV (MConsV (numV 20) (MEmptyV))))
(test (rinterp '{MList {MCons {if {> 13 7} 15 8} {MCons (rec (fac (fun (n)
                               (if (zero? n)
                                   1
                                   (* n (fac (dec n)))))) (fac 5)) {MCons {{fun {x} x} 3} {MEmpty}}}}}) (MListV (MConsV (numV 15) (MConsV (numV 120) (MConsV (numV 3) (MEmptyV))))))
;;If
(test (rinterp '(if (> 13 7) 15 8)) (numV 15))
(test (rinterp '(with ((n 13)) (if (and (> n 7) (< n 15)) 42 8))) (numV 42))
(test (rinterp '(with ((n 0)) (if (and (> n 7) (< n 15)) 42 8))) (numV 8))
;;Equal
;(test (interpEqual? (numV 5) (numV 5)) (boolV #t))
;(test (interpEqual? (boolV #t) (boolV #f)) (boolV #f))
;(test (interpEqual? (MListV (MEmptyV)) (MListV (MEmptyV))) (boolV #t))
;(test (interpEqual? (MListV (MConsV (numV 10) (MEmptyV))) (MListV (MEmptyV))) (boolV #f))
;(test/exn (interpEqual? (numV 4) (MListV (MEmptyV))) "La aplicacion de equal? no es adecuada")
;;Op
(test (rinterp '(neg true)) (boolV #f))
(test (rinterp '(inc 7)) (numV 8))
(test (rinterp '(dec 14)) (numV 13))
(test (rinterp '(neg true)) (boolV #f))
(test (rinterp '(bool? false)) (boolV #t))
;;Binop
(test (rinterp '(and (< 7 13) (> 15 8))) (boolV true))
(test (rinterp '(or (< 13 7) (> 15 8))) (boolV true))
;; Recursion con cajas
(test (rinterp '(rec (fac (fun (n)
                               (if (zero? n)
                                   1
                                   (* n (fac (dec n)))))) (fac 5))) (numV 120))