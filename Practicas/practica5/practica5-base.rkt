#lang plai

(define-type Binding
  [bind (name symbol?) (val RCFAELS?)])

;Definición de RCFAELS
(define-type RCFAELS
  [numS (n number?)]
  [boolS (b boolean?)]
  [MListS (l MListaS?)]
  [withS (bindings (listof bind?))
         (body RCFAELS?)]
  [with*S (bindings (listof bind?))
          (body RCFAELS?)]
  [idS (name symbol?)]
  [funS (params (listof symbol?))
        (body RCFAELS?)]
  [appS (fun RCFAELS?)
        (args (listof RCFAELS?))]
  [binopS (op procedure?)
         (l RCFAELS?)
         (r RCFAELS?)]
  [recS (name symbol?)
        (named-expr RCFAELS?)
        (body RCFAELS?)]
  [ifS (condition RCFAELS?)
       (then RCFAELS?)
       (else RCFAELS?)]
  [equal?S (fir RCFAELS?)
           (sec RCFAELS?)]
  [unopS (op procedure?)
         (arg RCFAELS?)]
  )

;Definición de RCFAEL,
(define-type RCFAEL
  [id (name symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [MList (l MLista?)]
  [rec (name symbol?)
        (named-expr RCFAEL?)
        (body RCFAEL?)]
  [fun (params (listof symbol?))
       (body RCFAEL?)]
  [ifR (condition RCFAEL?)
       (then RCFAEL?)
       (else RCFAEL?)]
  [equal?R (fir RCFAEL?)
           (sec RCFAEL?)]
  [app (fun RCFAEL?)
       (args (listof RCFAEL?))]
  [binop (op procedure?)
         (l RCFAEL?)
         (r RCFAEL?)]
  [unop (op procedure?)
        (arg RCFAEL?)]
  )

(define-type MLista
  [MEmpty]
  [MCons (mcar RCFAEL?) (mcdr MLista?)])
(define-type MListaS
  (MEmptyS)
  (MConsS [mcar RCFAELS?] [mcdr MListaS?]))

;Definición de RCFAEL-Value, parse, desugar o interp
(define-type RCFAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [MListV (l MListVal?)]
  [closureV (param (listof symbol?))
            (body RCFAEL?)
            (env Env?)])
(define-type MListVal
  (MEmptyV)
  (MConsV [mcar RCFAEL-Value?] [mcdr MListVal?]))

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value RCFAEL-Value?) 
        (env Env?)])

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

;para operador binario
(define (eligeBin s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) (lambda (x y) (and x y))]
    [(or) (lambda (x y) (or x y))]))

;para operador unario
(define (eligeUn s)
  (case s
    [(inc) add1]
    [(dec) sub1]
    [(zero?) zero?]
    [(num?) num?]
    [(neg) not]
    [(bool?) boolean?]
    [(first) first]
    [(rest) rest]
    [(empty?) empty?]
    [(list?) list?]))
  
;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x est� en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> FAES
(define (parse sexp)
  (cond
    [(symbol? sexp) (if (equal? sexp 'true) ;primero verificamos si se pasa como parametro 'true
                        (boolS true) ;si sí regresamos el valor true
                        (if (equal? sexp 'false) ;verificamos si se pasa como parametro 'false
                            (boolS false) ;si sí regresamos el valor false
                            (idS sexp)))] ;si no fue 'true ni 'false fue otro id y regresamos el id
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(with) (withS (parse-bindings (cadr sexp) #f) 
                      (parse (caddr sexp)))]
       [(with*) (with*S (parse-bindings (cadr sexp) #t)
                        (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) 
                    (parse (caddr sexp)))]
       [(+ - / * < > <= >= and or) (binopS (eligeBin (car sexp)) 
                                           (parse (cadr sexp)) 
                                           (parse (caddr sexp)))]
       [(if) (ifS (parse (cadr sexp))
                  (parse (caddr sexp))
                  (parse (cadddr sexp)))]
       [(equal?) (equal?S (parse (cadr sexp)) 
                          (parse (caddr sexp)))]
       [(inc dec zero? num? neg bool? first rest empty? list?) (unopS (eligeUn (car sexp)) 
                                                                      (parse (cadr sexp)))]
       [(rec) (recS (caadr sexp)
                    (parse (cadadr sexp))
                    (parse (caddr sexp)))]
       [(MList) (MListS (parse (cadr sexp)))]
       [(MCons) (MConsS (parse (cadr sexp)) (parse (caddr sexp))) ];;falta corregir que lea bien las listas de la entrada...
       [(MEmpty) (MEmptyS)]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
