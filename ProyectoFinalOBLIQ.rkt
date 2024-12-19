#lang eopl
(require racket/system)
(require racket/base)
#|
<programa>                       ::= <expresion>
<expresion>                      ::= <bool-expresion>
                                 ::= <identificador>
                                 ::= <numero>
                                 ::= <caracter>
                                 ::= <cadena>
                                 ::= ok  
                                 ::= var {<identificador> = <expresioni>}*(,) in <expresion> end
                                 ::= let {<identificador> = <expresioni>}*(,) in <expresion> end
                                 ::= letrec {<identificador> ({<identificadori>}*(,)) = <expresion>}* in <expresion> end
                                 ::= set <identificador> := <expresion>
                                 ::= begin <expresion> {<expresion>}*(;) end
                                 ::= <primitiva> ({<expresion>}*)
                                 ::= if <bool-expresion> then <expresion> {elseif <bool-expresion> then <expresion>}* else <expresion> end
                                 ::= proc ({<identificador>}*(,)) <expresion> end
                                 ::= apply <identificador> ({<expresion>}*(,))
                                 ::= meth (<identificador>, {<identificador>}*(,)) <expresion> end
                                 ::= for <identificador> = <expresion> to <expresion> do <expresion> end
                                 ::= object {{<identificador> => <expresion>}*} end
                                 ::= get <identificador>.<identificador>
                                 ::= send <identificador>.<identificador>({<identificador>}*(,))
                                 ::= update <identificador>.<identificador> := <expresion>
                                 ::= clone (<identificador> {<identificador>}*(,))

<bool-expresion>                 ::= true
                                 ::= false
                                 ::= <bool-primitiva>({<expresion>}*)
                                 ::= <bool-oper>({<bool-expresion>}*)
                                                     
<primitiva>                      ::= + |- |* |/ | % | & 
<bool-primitiva>                 ::= < | > | <= | >= |is 
<bool-oper>                      ::= not |and |or 

|#



;; Especificación léxica corregida
(define scanner-spec-simple-interpreter
  '((whitespace (whitespace) skip)
    (comment ("(*" (arbno (not "*)"))) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (character ("#\\" any) character)
    (string ("\"" (arbno (not "\"")) "\"") string)))

;; Especificación de la gramática
(define grammar-simple-interpreter
  '((program (expression) a-program)
    
    (expression (identifier) var-exp)
    (expression (number) lit-exp)
    (expression (character) char-exp)
    (expression (string) string-exp)
    (expression ("true") bool-true-exp)
    (expression ("false") bool-false-exp)
    (expression ("ok") ok-exp)
    
    (expression ("var" (arbno identifier "=" expression) "in" expression "end") vari-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression "end") let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) 
                "in" expression "end") letrec-exp)
    
    (expression ("set" identifier ":=" expression) set-exp)
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    
    (expression ("if" expression "then" expression 
                (arbno "elseif" expression "then" expression) 
                "else" expression "end") if-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression "end") proc-exp)
    (expression ("apply" expression "(" (separated-list expression ",") ")") apply-exp)
    
    (expression ("meth" "(" identifier "," (separated-list identifier ",") ")" expression "end") method-exp)
    (expression ("for" identifier "=" expression "to" expression "do" expression "end") for-exp)
    
    (primitive ("+") add-prim)
    (primitive ("-") subtract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("%") mod-prim)))

;; Funciones nuevas añadidas
(define show-the-datatypes
  (lambda () 
    (sllgen:list-define-datatypes 
      scanner-spec-simple-interpreter 
      grammar-simple-interpreter)))

(define just-scan
  (sllgen:make-string-scanner 
    scanner-spec-simple-interpreter 
    grammar-simple-interpreter))

;; Crea tipo de datos
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;; El FrontEnd (Análisis léxico y sintáctico integrados)
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;; Definición del entorno
(define empty-env '())

(define (extend-env ids vals env)
  (append (map cons ids vals) env))

(define (my-error msg)
  (error msg))

(define (apply-env env id)
  (cond
    ((null? env) (my-error (string-append "Unbound identifier: " (symbol->string id))))
    ((eq? id (car (car env))) (cdr (car env)))
    (else (apply-env (cdr env) id))))

;; Función auxiliar para evaluar expresiones
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

;; Intérprete
(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)                        
        (eval-expression body (init-env))))))

;; Ambiente Inicial
(define init-env 
  (lambda ()
    empty-env))

;; Funciones para aplicar primitivas
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args)  (cadr args))) 
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (div-prim  () (/ (car args) (cadr args)))
      (mod-prim  () (modulo (car args) (cadr args))))))

;; Evaluador de expresiones
(define eval-expression 
  (lambda (exp env)
    (cases expression exp
      (var-exp (id) 
        (apply-env env id))
      (lit-exp (datum) 
        datum)     
      (char-exp (char) 
        char) 
      (string-exp (string) 
        string)
      (bool-true-exp () 
        'true)
      (bool-false-exp () 
        'false) 
      (ok-exp () 
        'ok)
      (vari-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (set-exp (id rhs-exp)  
        (begin
          (set! (apply-env env id) (eval-expression rhs-exp env))
          1))           
      (begin-exp (exp1 exp2)
        (let ((acc (eval-expression exp1 env))
              (exp2 exp2))
          (if (null? exp2) 
              acc
              (eval-expression exp2 env))))       
      (primapp-exp (prim rands)              
        (let ((args (eval-rands rands env))) 
          (apply-primitive prim args)))
      (if-exp (test-exp then-exp else-exps then-exps else-exp)
        (if (eval-expression test-exp env)
            (eval-expression then-exp env)
            (eval-expression else-exp env)))
      (proc-exp (ids body) 
        (lambda (args)
          (eval-expression body (extend-env ids args env))))
      (apply-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands rands env)))
          (proc args)))
      (for-exp (id start-exp end-exp body)
        (let loop ((i (eval-expression start-exp env))
                  (end (eval-expression end-exp env)))
          (if (> i end)
              'ok
              (begin
                (eval-expression body (extend-env (list id) (list i) env))
                (loop (+ i 1) end)))))
      (else (my-error (string-append "Unknown expression type: " (format "~a" exp)))))))

;; Función para iniciar el intérprete
(define interpretador
  (sllgen:make-rep-loop "-->"
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;; Iniciar el intérprete
(interpretador)