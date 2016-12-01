;;Funciones de listas
(define (hoja? lista)
  (not(list? lista))
)

;;Funciones de arboles
(define (hoja-tree? tree)
  (null? (cdr tree))
)

(define (hijos-tree tree)
 (cdr tree) 
)

(define (dato-tree tree)
  (car tree))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (suma-listas l1 l2)
  (cond
    ((null? l1)'())
    ((hoja? l1)(list (+ l1 l2)))
    (else (append (suma-listas(car l1)(car l2))(suma-listas(cdr l1)(cdr l2)))
     )
  )
)

(suma-listas '(1 2(3(4)(5(6 7)))3)
'(4 2(1(8)(7(2 3)))2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tree '(1(2(8))(3(5(1)))(4(7))))

(define (sumarama tree)
  (if(null? tree)
     0
     (if(hoja-tree? tree)
        (dato-tree tree)
        (+ (dato-tree tree)(sumarama (cadr tree)))
     )
  )
)

(define (aplicafunc f lista)
  (if(null? lista)
     '()
     (cons (f (car lista))(aplicafunc f (cdr lista))))
  )

(define (suma-max-tree tree)
  (apply max (map (lambda (x)(+ (dato-tree tree) x))(aplicafunc sumarama (hijos-tree tree))))
)

(suma-max-tree tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(cuenta-elem-bosque bosque elem)
  (if(null? bosque)
     0
     (+ (cuenta-elem-tree (car bosque) elem)
        (cuenta-elem-bosque (cdr bosque) elem)
      )
   )
)
(define (cuenta-elem-tree tree elem)
  (if (null? tree)
      0
      (if(equal? (dato-tree tree)elem)
      (+ 1 (cuenta-elem-bosque (hijos-tree tree) elem))
      (cuenta-elem-bosque (hijos-tree tree) elem)
      )
  )
)

(define letras '(a(b(a))(a(r(e)))(c(a))))

(cuenta-elem-tree letras 'a)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x (cons '()'()))
(set-car! x (cons 5 '()))
(set-cdr! (car x) x)
(set-cdr! x (cons (cons 2 (car x))'()))
(set-cdr! (cdr x)(cadr x))
x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'otro
(set-cdr! (cadr x)(caadr x))
x
(set-car! (cadr x) (cdr x))
x
(set-cdr! x (cddr x))
x
(set-car! (cadr x) x)
x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;