#lang eopl
#|
 Gramatica del BNF para el lenguaje obliq


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
                                 ::= if <bool-expresion> then <expresion> {elseif <bool-expresion> then <expresion> }* else <expresion> end
                                 ::= proc ({<identificador>}*(,)) <expresion> end
                                 ::= apply <identificador> ({<expresion>}*(,))
                                 ::= meth (<identificador>, {<identificador>}*(,)) <expresion> end
                                 ::= for <identificador> = <expresion> to <expresion> do <expresion> end
                                 ::= object {{<identificador> => <expresion> }*} end
                                 ::= get <identificador>.<identificador>
                                 ::= send <identificador>.<identificador>({<identificador>}*(,))
                                 ::= update <identificador>.<identificador> := <expresion>
                                 ::= clone (<identificador> {<identificador>}*(,))
                                  
<bool-expresion>                 ::= true
                                 ::= false
                                 ::= <bool-primitiva>({<expresion>}*(,))
                                 ::= <bool-oper>({<bool-expresion>}*(,))
                                    
                     
<primitiva>                      ::= + |- |* |/ | % | & 
<bool-primitiva>                 ::= < | > | <= | >= |is 
<bool-oper>                      ::= not |and |or 
|#
#|
; Especificacion lexica
(define scanner-spec-simple-interpreter
  '((whitespace (whitespace) skip)
    (comment ("(*"(arbno (not #\newline))"*)") skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number (digit (arbno digit)) number)
    (string ("\"" (arbno (or letter digit whitespace "_" "-" "?" "." ",")) "\"") string)
    (char ("\'" (or letter digit) "'") symbol)
    ))

; Especificacion de la gramatica del lenguaje.
(define grammar-simple-interpreter
  '((program (expression) a-program)
     (expression (identifier) var-exp)
     (expression (number) lit-exp)
     (expression (char) char-exp)
     (expression (string) string-exp)
     
     ;; Expresiones booleanas
     (expression ("true") bool-true-exp)
     (expression ("false") bool-false-exp)
     (expression ("ok") ok-exp)
     (expression ("var" (arbno identifier "=" expression) "in" expression "end") var-exp)
     (expression ("let" (arbno identifier "=" expression) "in" expression "end") let-exp)
     (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")") "=" expression "in" expression "end") letrec-exp)
     
     ;; Asignaciones y control de flujo
     (expression ("set" identifier ":=" expression) set-exp)
     (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
     (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
     (expression (bool-oper "(" (separated-list expression ",") ")" ) operapp-exp)   
     (expression (bool-prim "(" (separated-list expression ",") ")" ) expboolapp-exp)
     (expression ("if" bool-exp "then" expression (arbno "elseif" bool-exp "then" expression) "else" expression "end") if-exp)
     (expression ("proc" "(" (separated-list identifier ",") ")" expression "end") proc-exp)
     (expression ("apply" expression "(" (arbno expression) ")") apply-exp)
     (expression ("meth" "(" identifier "," (separated-list identifier ",") ")" expression "end") method-exp)
     (expression ("for" identifier "=" expression "to" expression "do" expression "end") for-exp)
     (expression ("object" "{" (arbno identifier "=>" expression) "}" "end") object-exp)
     (expression ("get" identifier "." identifier) get-exp)
     (expression ("send" identifier "." identifier "(" (separated-list expression ",") ")") send-exp)
     (expression ("update" identifier "." identifier ":=" expression) update-exp)
     (expression ("clone" "(" identifier (arbno identifier ",") ")") clone-exp)
     
     ;; Operaciones booleanas
    (expression ("true") bool-true-exp)
    (expression ("false") bool-false-exp)
    (expression (bool-prim "(" (separated-list expression ",") ")") boolapp-exp)
    (expression (bool-oper "(" (separated-list expression ",") ")") booloperapp-exp)

    ;; Primitivas
    (primitive ("+") add-prim)
    (primitive ("-") sub-prim)
    (primitive ("*") mul-prim)
    (primitive ("/") div-prim)
    (primitive ("%") mod-prim)
    (primitive ("&") and-prim)
    ;; Primitivas booleanas
    (bool-prim ("=") equal?-prim)
    (bool-prim ("zero?") zero?-prim)
    (bool-prim (">") mayor?-prim)
    (bool-prim ("<") menor?-prim)
    (bool-prim (">=") mayorigual?-prim)
    (bool-prim ("<=") menorigual?-prim)
    (bool-prim ("is") is?-prim)

    ;; Operadores booleanos
    (bool-oper ("not") bool-oper-not)
    (bool-oper ("and") bool-oper-and)
    (bool-oper ("or") bool-oper-or)
  ))
;; Llamar al Sllegen

;; Se construyen los tipos de datos automáticamente por medio del sllgen
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;; Procedimiento para mostrar los datatypes 
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;; El FrontEnd (Análisis léxico )
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))
;; FrontEnd
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

(display "OBLIQ --> ")

;;inicio interpretador

(define interpretador
  (sllgen:make-rep-loop "OBLIQ -->" 
    (lambda (pgm) (eval-program pgm))  
    (sllgen:make-stream-parser  
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;;Tipos de dato

(define-datatype object object?
  (an-object
   (ids (list-of symbol?))
   (exps vector?)))

;;Metodos
(define-datatype method method?
  (a-method
   (id-obj symbol?)
   (ids (list-of symbol?))
   (body expression?)))

;;bool_expresion
(define-datatype bool-exp bool-exp?
  (boolapp-exp
   (prim bool prim?)
   (rands (list-of expression?)))
  (booloperapp-exp
   (prim bool-oper?)
   (rands (list-of expression?)))
  (bool-true-exp)
  (bool-false-exp))

;;Primitivas booleanas
(define-datatype bool-prim bool-prim?
  (equal?-prim)
  (zero?-prim)
  (mayor?-prim)
  (menor?-prim)
  (mayorigual?-prim)
  (menorigual?-prim)
  (is?-prim)
  )

(define-datatype bool-oper bool-oper?
  (bool-oper-and)
  (bool-oper-or)
  (bool-oper-not)  
  )

;;Procval
(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

;;Environment
(define-datatype environment environment?
  (empty-env-record)           
  (extended-env-record         
    (syms (list-of symbol?))   
    (vec vector?)              
    (env environment?))        
(extended-mutable-env-record
    (syms (list-of symbol?))
    (vals vector?)
    (env environment?))
  )
;;Referencia
(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec) (vector-set! vec pos val)))))

(define deref 
  (lambda (ref)
    (primitive-deref ref))) 

(define setref! 
  (lambda (ref val)
    (primitive-setref! ref val)))

(interpretador)
|#
;;;cambioa