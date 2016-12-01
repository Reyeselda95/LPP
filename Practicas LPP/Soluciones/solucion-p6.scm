;;-------------------------
;; Solución práctica 6
;; Fecha: 14/04/2015
;--------------------------


(define (hoja? dato)
  (not (list? dato)))

(define (make-tree dato lista-hijos) (cons dato lista-hijos))
(define (make-hoja-tree dato) (make-tree dato '()))
(define (dato-tree tree)  (car tree))
(define (hijos-tree tree) (cdr tree))
(define (hoja-tree? tree) (null? (hijos-tree tree)))



;--------------------------
; Ejercicio 1
;--------------------------

; 1.a)

(define lista '((a b) d (c (e f) (g) h)))


; 1.b)
(define arbol '(40 (5 (2) (3)) (10) (20 (12) (15 (13) (14)) (17) (19 (18)))))



;--------------------------
; Ejercicio 2
;--------------------------

; 2.a)
(define (suma-lista lista)
  (cond ((null? lista) 0)
        ((hoja? lista) lista)
        (else (+ (suma-lista (car lista))
                 (suma-lista (cdr lista))))))
      
  

; Versión con recursión mutua
(define (suma-tree tree)
  (+ (dato-tree tree)
     (suma-bosque (hijos-tree tree))))

(define (suma-bosque bosque)
  (if (null? bosque)
      0
      (+ (suma-tree (car bosque))
         (suma-bosque (cdr bosque)))))


; Versión con funciones de orden superior
(define (suma-tree tree)
  (if (hoja-tree? tree)
      (dato-tree tree)
      (+ (dato-tree tree)
         (apply + (map suma-tree (hijos-tree tree))))))
  
  
; Pruebas
"Ejercicio 2.a"
(define lista '(1 (2 (3 ((((4)))) 5) 6)))
(define tree '(3 (5 (6) (7)) (4)))
(suma-lista lista) ; => 21
(suma-tree tree)   ; => 25
(suma-lista tree)  ; => 25 
;(suma-tree lista)  ; => error


; 2.b)
(define (aplana lista)
  (cond ((null? lista) '())
        ((hoja? lista) (list lista))
        (else (append (aplana (car lista))
                      (aplana (cdr lista))))))
                              
 
; Pruebas
"Ejercicio 2.b"
(aplana '(1 (2 (3) (4 (5 (6) 7)))))  ; => (1 2 3 4 5 6 7)



; 2.c)
(define (diff-listas l1 l2)
  (cond ((null? l1) '())
        ((hoja? l1) (if (equal? l1 l2)
                        '()
                        (list (cons l1 l2))))
        (else (append (diff-listas (car l1) (car l2))
                      (diff-listas (cdr l1) (cdr l2))))))              
 
; Pruebas
"Ejercicio 2.c"
(diff-listas '(a (b ((c)) d e) f) '(1 (b ((2)) 3 4) f)) ; => ((a.1)(c.2)(d.3)(e.4))
(diff-listas '() '())                                  ; => ()
(diff-listas '((a b) c) '((a b) c))                    ; => ()


;--------------------------
; Ejercicio 3
;--------------------------

; 3.a)

; Versión con recursión mutua y funciones de orden superior
(define (ordenado-tree? tree)
  (if (hoja-tree? tree)
      #t
      (and (apply < (append (map dato-tree (hijos-tree tree))
                            (list (dato-tree tree))))
           (ordenado-bosque? (hijos-tree tree)))))

(define (ordenado-bosque? bosque)
  (if (null? bosque)
      #t
      (and (ordenado-tree? (car bosque))
           (ordenado-bosque? (cdr bosque)))))


; Versión con funciones de orden superior
(define (apply-and lista)
  (if (null? (cdr lista))
      (car lista)
      (and (car lista)
           (apply-and (cdr lista)))))


(define (ordenado-tree? tree)
  (if (hoja-tree? tree)
      #t
      (and (apply < (append (map dato-tree (hijos-tree tree))
                            (list (dato-tree tree))))  
           (apply-and (map ordenado-tree? (hijos-tree tree))))))

; Pruebas
"Ejercicio 3.a"
(ordenado-tree? '(10 (5) (7)))                      ; => #t
(ordenado-tree? '(50 (10 (4) (6) (8)) (25 (15))))   ; => #t 
(ordenado-tree? '(10 (8) (7)))                      ; => #f
(ordenado-tree? '(6 (5) (7)))                       ; => #f
(ordenado-tree? '(50 (10 (4) (6) (11)) (25) (15)))  ; => #f
(ordenado-tree? '(50 (10 (4) (6) (11)) (25) (35)))  ; => #f


; 3.b)

(define (operator op)
      (cond
          ((equal? op '+) +)
          ((equal? op '-) -)
          ((equal? op '*) *)
          ((equal? op '/) /)
          (else (error "Operador desconocido: " op))))


; Versión con funciones de orden superior
(define (calcula-tree tree)
  (if (hoja-tree? tree)
      (dato-tree tree)
      (apply (operator (dato-tree tree)) 
             (map calcula-tree (hijos-tree tree)))))


; Pruebas
"Ejercicio 3.b"
(calcula-tree '(+ (- (5) (2)) (3)))                                 ; => 6
(calcula-tree '(* (- (2) (+ (3) (* (4) (- (6) (2)) (3)) (1))) (1))) ; => -50




;--------------------------
; Ejercicio 4
;--------------------------

; 4.a)

; Versión sin añadir argumento para el nivel

(define (mas-uno-si-no-es-cero x)
  (if (> x 0) (+ x 1) x))

(define (nivel-hoja dato lista)
  (cond ((or (null? lista)
             (hoja? lista)) 0)
        ((and (hoja? (car lista))
              (equal? (car lista) dato)) 1)
        (else (max (mas-uno-si-no-es-cero (nivel-hoja dato (car lista)))
                   (nivel-hoja dato (cdr lista))))))

; Versión iterativa, añadiendo un argumento para el nivel

(define (nivel-hoja dato lista)
  (aux-nivel-hoja dato lista 1))

(define (aux-nivel-hoja dato lista n)
  (cond ((null? lista) n)
        ((equal? (car lista) dato) n)
        ((hoja? (car lista)) (aux-nivel-hoja dato (cdr lista) n))
        (else (max (aux-nivel-hoja dato (car lista) (+ n 1))
                   (aux-nivel-hoja dato (cdr lista) n)))))
  

; Pruebas
"Ejercicio 4.a"
(nivel-hoja 'b '(a b (c)))      ; => 1
(nivel-hoja 'b '(a (b) c))      ; => 2 
(nivel-hoja 'b '(a c d ((b))))  ; => 3￼


; 4.b)

; Versión 1: con recursión pura y funciones de orden superior

(define (nivel-dato-tree dato tree)
    (cond
      ((equal? (dato-tree tree) dato) 0)
      ((null? (hijos-tree tree)) -1)
      (else
       (+ 1 (nivel-dato-bosque dato (hijos-tree tree))))))

(define (nivel-dato-bosque dato bosque)
    (apply max (map (lambda (arbol)
                      (nivel-dato-tree dato arbol)) 
                    bosque)))

; Versión 2: iterativa (tail-recursion) y con recursión mutua

(define (nivel-dato-tree dato tree)
  (aux-nivel-dato-tree dato tree 0))

(define (aux-nivel-dato-tree dato tree n)
  (cond ((equal? (dato-tree tree) dato) n)
        (else (nivel-dato-bosque dato (hijos-tree tree) n))))

(define (nivel-dato-bosque dato bosque n)
  (if (null? bosque)
      0
      (max (aux-nivel-dato-tree dato (car bosque) (+ n 1))
           (nivel-dato-bosque dato (cdr bosque) n))))


;Versión 3: iterativa (tail-recursion) y con funciones de orden superior

(define (nivel-dato-tree dato tree)
  (aux-nivel-dato-tree dato tree 0))

(define (aux-nivel-dato-tree dato tree n)
  (cond ((equal? (dato-tree tree) dato) n)
        ((hoja-tree? tree) 0)
        (else (apply max (map (lambda(x) (aux-nivel-dato-tree dato x (+ n 1)))
                              (hijos-tree tree))))))



; Pruebas
"Ejercicio 4.b"
(nivel-dato-tree 30 '(20 (18) (19 (30) (32)) (4))) ; => 2
(nivel-dato-tree 20 '(20 (18) (19 (30) (32)) (4))) ; => 0
(nivel-dato-tree 4 '(20 (18) (19 (30) (32)) (4)))  ; => 1



;--------------------------
; Ejercicio 5
;--------------------------

(define (transformar plantilla lista)
  (cond ((null? plantilla) '())
        ((hoja? (car plantilla)) (cons (list-ref lista (car plantilla))
                                 (transformar (cdr plantilla) lista)))
        (else (cons (transformar (car plantilla) lista)
                    (transformar (cdr plantilla) lista)))))
  
(transformar '((0 1) 4 (2 3)) '(hola que tal estas hoy))            ; => ((hola que) hoy (tal estas))
(transformar '(1 4 3 2 5 (0)) '(vamos todos a aprobar este examen)) ; => (todos este aprobar a examen (vamos))

