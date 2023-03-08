#lang plait


(module+ test
  (print-only-errors #t))

;; abstrat syntax ---------------------------------

(define-type Op
  (add) (sub) (mult) (less_or_equal))

(define-type Exp
  (numE (number : Number))
  (varE (variable : Symbol))
  (opE (left_val : Exp) (op : Op) (right_val : Exp))
  (ifE (bool : Exp) (if_t : Exp) (if_f : Exp))
  (letE (var1 : Symbol) (val : Exp) (exp : Exp))
  (appE (sym_fun : Symbol) (argu : (Listof Exp))))

(define-type Declaration
  (funE (name : Symbol) (variables : (Listof Symbol)) (result : Exp)))

(define-type Program
  (progE (declar : (Listof Declaration)) (app : Exp)))

; parsery -----------------------------------------

(define (parseExp [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `(ifz ANY then ANY else ANY) s)
     (ifE (parseExp (second (s-exp->list s)))
          (parseExp (fourth (s-exp->list s)))
          (parseExp (fourth (rest (rest (s-exp->list s))))))]
    [(s-exp-match? `(let SYMBOL be ANY in ANY) s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parseExp (fourth (s-exp->list s)))
           (parseExp (fourth (rest (rest (s-exp->list s))))))]
    [(s-exp-match? `(SYMBOL (ANY ...)) s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parseExpList (s-exp->list (second (s-exp->list s)))))]
    [(s-exp-match? `(ANY SYMBOL ANY) s)
     (opE (parseExp (first (s-exp->list s)))
          (symbol->op (s-exp->symbol (second (s-exp->list s))))
          (parseExp (third (s-exp->list s))))]
    [else (error 'parse "Unkown pattern to parse to expresion")]
    ))

(define (parseExpList (lista : (Listof S-Exp))) : (Listof Exp)
  (if (empty? lista)
      empty
      (cons (parseExp (first lista)) (parseExpList (rest lista)))))

(define (symbol->op (sym : Symbol)) : Op
  (cond
    [(equal? sym '+) (add)]
    [(equal? sym '-) (sub)]
    [(equal? sym '*) (mult)]
    [(equal? sym '<=) (less_or_equal)]
    [else (error 'op "Unkown operation")]))

(define (parseDeclaration [s : S-Exp]) : Declaration
  (cond
    [(s-exp-match? `(fun SYMBOL (SYMBOL ...) = ANY) s)
     (funE (s-exp->symbol (second (s-exp->list s)))
           (parseVarList (s-exp->list (third (s-exp->list s))))
           (parseExp (fourth (rest (s-exp->list s)))))]
    [else (error 'parse "Unknown patern to parse the definition")]))

(define (parseVarList (lista : (Listof S-Exp))) : (Listof Symbol)
  (if (empty? lista)
      empty
      (if (find-copies-arguments (s-exp->symbol (first lista)) (rest lista))
          (error 'parse "At least two identical arguments in function")
          (cons (s-exp->symbol (first lista))
                (parseVarList (rest lista))))))

(define (find-copies-arguments name lista)
  (if (empty? lista)
      #f
      (if (equal? (s-exp->symbol (first lista)) name)
          #t
          (find-copies-arguments name (rest lista)))))

(define (parseProg [s : S-Exp]) : Program
  (cond
    [(s-exp-match? `(define (ANY ...) for ANY) s)
     (progE (parseDecList (s-exp->list (second (s-exp->list s))))
            (parseExp (fourth (s-exp->list s))))]
    [else (error 'parse "Unkown patern to parse the program")]))

(define (parseDecList (lista : (Listof S-Exp))) : (Listof Declaration)
  (if (empty? lista)
      empty
      (cons (parseDeclaration (first lista))
            (parseDecList (rest lista)))))

(define (parse [s : S-Exp])
  (parseProg s))

(module+ test
  (test (parse `(define () for (2 + 2)))
        (progE (list) (opE (numE 2) (add) (numE 2))))
  (test (parse `{define
                  {[fun fact (n) = {ifz n then 1 else {n * {fact ({n - 1})}}}]}
                  for
                  {fact (5)}})
        (progE (list
                (funE 'fact
                      (list 'n)
                      (ifE (varE 'n)
                           (numE 1)
                           (opE (varE 'n)
                                (mult)
                                (appE 'fact
                                      (list (opE
                                             (varE 'n)
                                             (sub)
                                             (numE 1))))))))
               (appE 'fact (list (numE 5)))))
 (test (parse `{define
                 {[fun even (n) = {ifz n then 0 else {odd ({n - 1})}}]
                  [fun odd (n) = {ifz n then 42 else {even ({n - 1})}}]}
                 for
                 {even (1024)}})
       (progE
        (list (funE 'even
                    (list 'n)
                    (ifE (varE 'n) (numE 0) (appE 'odd
                                                  (list (opE (varE 'n) (sub) (numE 1))))))
              (funE 'odd
                    (list 'n)
                    (ifE (varE 'n) (numE 42) (appE 'even
                                                  (list (opE (varE 'n) (sub) (numE 1)))))))
        (appE 'even (list (numE 1024)))))
                    
  )

; evaluation ------------------------------------------------------------------------------

(define-type Value
  (numV [n : Number]))


(define-type Storable
  (value [v : Value])
  (fun [args : (Listof Symbol)] [e : Exp] [env : Env])
  (undef))

(define-type Binding
  (bind [name : Symbol]
        [ref : (Boxof Storable)]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env-undef [env : Env] [x : Symbol]) : Env
  (cons (bind x (box (undef))) env))

(define (extend-env-value [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x (box (value v))) env))

#|
define {func} for e
func nazwy argu ciała
env
nazwe undef
update! nazwa argu ciało środowisko
nazwa argu ciało środowisko
odd <-> even
rekursja
|#

(define (extend-env-fun [env : Env] [x : Symbol] [body : Exp] [arg : (Listof Symbol)])
  (cons (bind x (box (fun arg body env))) env))

(define (find-var [env : Env] [x : Symbol]) : (Boxof Storable)
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? x (bind-name b))
                         (bind-ref b)]
                        [else
                         (find-var rst-env x)])]))

(define (lookup-env [x : Symbol] [env : Env]) : Storable
  (let ([elem (unbox (find-var env x))])
    (type-case Storable elem
      [(undef) (error 'lookup-env "undefined variable")]
      [else elem])))

(define (update-env-value! [env : Env] [x : Symbol] [v : Value]) : Void
  (set-box! (find-var env x) (value v)))

(define (upade-env-fun! [env : Env] [x : Symbol] [sym : (Listof Symbol)] [body : Exp])
  (set-box! (find-var env x) (fun sym body  env)))

(define (evalExp [e : Exp] [env : Env])
  (type-case Exp e
    [(numE n) (numV n)]
    [(varE x)
     (type-case Storable (lookup-env x env)
       [(value v) v]
       [else (error 'eval "You find function this is not a number. Evaluator cannot evaluate it.")])]
    [(opE e1 op e2)
     (apply-op op (evalExp e1 env) (evalExp e2 env))]
    [(ifE bool e1 e2)
     (let ([val (evalExp bool env)])
       (if (equal? (numV 0) val)
           (evalExp e1 env)
           (evalExp e2 env)))]
    [(letE var1 val exp)
     (let ([new-env (extend-env-value env var1 (evalExp val env))])
       (evalExp exp new-env))]
    [(appE sym_fun args)
     (apply (lookup-env sym_fun env) (ListExp->ListVal args env))]
    ))

(define (apply-op [op : Op] [v1 : Value] [v2 : Value]) : Value
  ((op->func_val op) v1 v2))

(define (op->func_val op) : (Value Value -> Value)
  (type-case Op op
    [(add) (λ (x y)
             (type-case Value x
               ([numV n1]
                (type-case Value y
                  ([numV n2]
                   (numV (+ n1 n2)))))))]
    [(sub) (λ (x y)
             (type-case Value x
               ([numV n1]
                (type-case Value y
                  ([numV n2]
                   (numV (- n1 n2)))))))]
    [(mult) (λ (x y)
             (type-case Value x
               ([numV n1]
                (type-case Value y
                  ([numV n2]
                   (numV (* n1 n2)))))))]
    [(less_or_equal) (λ (x y)
             (type-case Value x
               ([numV n1]
                (type-case Value y
                  ([numV n2]
                   (if (<= n1 n2)
                       (numV 0)
                       (numV 1)))))))]))

(define (apply (function : Storable) (lista : (Listof Value)))
  (type-case Storable function
    ([fun args body env]
     (let ([new-env (make-append env lista args)])
       (evalExp body new-env)))
    (else (error 'apply "You want to give arguments to something that is not a function"))))

(define (ListExp->ListVal (args : (Listof Exp)) (env : Env))
  (if (empty? args)
      empty
      (cons (evalExp (first args) env) (ListExp->ListVal (rest args) env))))

(define (make-append env lista args)
  (if (empty? lista)
      (if (empty? args)
          env
          (error 'apply "You have not given enough values"))
      (if (empty? args)
          (error 'apply "You have given too much values")
          (make-append
           (extend-env-value env (first args) (first lista))
           (rest lista)
           (rest args)))))

(define (evalFun [lista : (Listof Declaration)] [env : Env]) : Env
  (let ([new-env (put-symbols env lista)])
    (set-body new-env lista)))

(define (put-symbols (env : Env) (lista : (Listof Declaration)))
  (if (empty? lista)
      env
      (if (find-copies-in-name (first lista) (rest lista))
          (error 'evalFun "Multipule definition of function with the same name")
          (type-case Declaration (first lista)
            ([funE name variables  result]
             (put-symbols (extend-env-undef env name) (rest lista)))))))

(define (set-body [env : Env] [lista : (Listof Declaration)]) : Env
  (if (empty? lista)
      env
      (begin
        (type-case Declaration (first lista)
          ([funE name variables result]
           (upade-env-fun! env name variables result)))
        (set-body env (rest lista)))))

(define (find-copies-in-name val lista)
  (if (empty? lista)
      #f
      (type-case Declaration val
        ([funE name variables result]
         (type-case Declaration (first lista)
           ([funE name2 variables2 result2]
            (if (equal? name name2)
                #t
                (find-copies-in-name val (rest lista)))))))))

(define (evalProgram [program : Program] [env : Env])
  (type-case Program program
    ([progE declar app]
     (let ([new-env (evalFun declar env)])
       (evalExp app new-env)))))

(define (run [input : S-Exp])
  (let ([result (evalProgram (parseProg input) empty)])
    (type-case Value result
      ((numV v) v))))
(module+ test
  (test (run `{define
                {[fun fact (n) = {ifz n then 1 else {n * {fact ({n - 1})}}}]}
                for
                {fact (5)}})
        120)
  (test (run `{define
                {[fun even (n) = {ifz n then 0 else {odd ({n - 1})}}]
                 [fun odd (n) = {ifz n then 42 else {even ({n - 1})}}]}
                for
                {even (1024)}})
        0)
  (test (run `{define
                {[fun gcd (m n) = {ifz n
                                       then m
                                       else {ifz {m <= n}
                                                 then {gcd (m {n - m})}
                                                 else {gcd ({m - n} n)}}}]}
                for
                {gcd (81 63)}})
        9)
  (test/exn (run `{define
                {[fun gcd (m n) = {ifz n
                                       then m
                                       else {ifz {m <= n}
                                                 then {gcd (m {n - m})}
                                                 else {gcd ({m - n} n)}}}]}
                for
                {gcd (81)}})
            "You have not given enough values")
  (test/exn (run `{define
                {[fun gcd (m n) = {ifz n
                                       then m
                                       else {ifz {m <= n}
                                                 then {gcd (m {n - m})}
                                                 else {gcd ({m - n} n)}}}]}
                for
                {gcd (81 29 30)}})
            "You have given too much values")
  )