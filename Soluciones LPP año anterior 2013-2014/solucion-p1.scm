;;
;; Solución práctica 1
;; 

;--------------------------
; Ejercicio 1
;--------------------------

"Ejercicio 1"

(define lista '(4 2 3 (6 5 1) (9) (7 (10 (11 13 (14))))))
(car (cdr (cdr (cdr (cdr lista))))) ; => (9)
(car (cdr (cdr (cdr (cdr (cdr lista)))))) ; => (7 (10 (11 13 (14)))))
(car (cdr (car (cdr (cdr (cdr lista)))))) ; => 5
(car (car (cdr (car (cdr (cdr (cdr (cdr (cdr lista))))))))) ; => 10


;--------------------------
; Ejercicio 2 (versión 1)
;--------------------------
; Versión 1 con 3 comparaciones; también se podría hacer con un "cond"

(define (mayor-de-tres n1 n2 n3)
  (if (and (> n1 n2) 
           (> n1 n3)) 
      n1
      (if (> n2 n3) 
          n2
          n3)))

;--------------------------
; Ejercicio 2 (versión 2)
;--------------------------
; Versión 2 con llamadas anidadas a una función "mayor" definida por nosotros
; que devuelve el mayor de 2 números


(define (mayor n1 n2)
  (if (> n1 n2)
      n1
      n2))

(define (mayor-de-tres n1 n2 n3)
  (mayor n1 (mayor n2 n3)))

; Pruebas

"Ejercicio 2"

(mayor-de-tres 8 2 1) ; => 8
(mayor-de-tres 2 8 1) ; => 8
(mayor-de-tres 2 1 8) ; => 8
(mayor-de-tres 3 0 3) ; => 3

;--------------------------
; Ejercicio 3
;--------------------------

(define (engloba? a1 a2 b1 b2)
  (or (and (>= a1 b1) (<= a2 b2)) 
          (and (>= b1 a1) (<= b2 a2))))

;
; Esto es equivalente:
;
;   (define (engloba? a1 a2 b1 b2)
;      (if (or (and (>= a1 b1) (<= a2 b2))
;              (and (>= b1 a1) (<= b2 a2)))
;         #t
;   	  #f))
;


; Pruebas

"Ejercicio 3"

(engloba? 4 10 5 9) ; => #t
(engloba? 4 9 4 15) ; => #t
(engloba? 2 6 4 8) ; => #f


;--------------------------
; Ejercicio 4
;--------------------------
;
;            |----------|
;            a1         a2
;                   |----------|
;                   b1         b2
;      |----------|
;      b1         b2

; Versión 1: comprueba si b1 está entre a1 y a2 o
; b2 está entre a1 y a2

(define (intersectan? a1 a2 b1 b2)
  (or (and (>= b1 a1) (<= b1 a2))
      (and (>= b2 a1) (<= b2 a2))))

; Versión 2: comprueba si los intervalos están completamente
; separados y devuelve el "not" de eso
(define (intersectan? a1 a2 b1 b2) 
  (not (or (< a2 b1) (< b2 a1))))

; Versión 3: sin utilizar not,
; se obtiene simplificando la 
; expresión (not (or (< a2 b1) (< b2 a1))
;
(define (intersectan? a1 a2 b1 b2) 
   (and (<= b1 a2) (<= a1 b2)))

;
; Función principal: intersección
;
(define (interseccion a1 a2 b1 b2)
  (if (intersectan? a1 a2 b1 b2)
      (cons (max a1 b1) (min a2 b2))
      '()))

;Pruebas

"Ejercicio 4"

(intersectan? 4 7 5 12)  ; => #t
(intersectan? 4 9 12 15)  ; =>  #f
(intersectan? 2 5 5 8)  ; =>  #t

(interseccion 4 7 5 12) ; => (5 . 7)
(interseccion 4 9 12 15) ; => ()
(interseccion 2 5 5 8) ; => (5 . 5)

;--------------------------
;; Ejercicio 5
;--------------------------

; Versión 1: Las funciones cyan, magenta y yellow se
; definen con 2 argumentos, valor de blanco y valor de
; r, g o b. La llamada a white para calcular el valor
; de blanco se hace antes de llamar a cyan, magenta o yellow

(define (white red green blue)
  (max (/ red 255) (/ green 255) (/ blue 255)))

(define (cyan red white)
  (if (= white 0) 0
      (/ (- white (/ red 255)) white)))

(define (magenta green white)
  (if (= white 0) 0
      (/ (- white (/ green 255)) white)))

(define (yellow blue white)
  (if (= white 0) 0
      (/ (- white (/ blue 255)) white)))

(define (black white)
  (- 1 white))

(define (rgb->cmyk red green blue)
  (list 
      (cyan red (white red green blue))
      (magenta green (white red green blue))
      (yellow blue (white red green blue))
      (black (white red green blue))))

; Versión 2: definimos las funciones cyan, magenta, yellow y black
; con 3 argumentos (red, gree, blue) y se define una función 
; auxiliar que realiza la operación del cálculo de la componente
;

(define (white r g b)
  (max (/ r 255) (/ g 255) (/ b 255)))

(define (componente comp-rgb w)
  (if (= w 0)
      0
      (/ (- w (/ comp-rgb 255)) w)))
         
(define (cyan r g b)
  (componente r (white r g b)))

(define (magenta r g b)
  (componente g (white r g b)))

(define (yellow r g b)
  (componente b (white r g b)))
   
(define (black r g b)
  (- 1 (white r g b)))

(define (rgb->cmyk red green blue)
  (list 
      (cyan red green blue)
      (magenta red green blue)
      (yellow red green blue)
      (black red green blue)))

;Pruebas

"Ejercicio 5"

(rgb->cmyk 75 0 130) ; => (11/26 1 0 25/51)
(rgb->cmyk 150 10 255) ; => (7/17 49/51 0 0)
(rgb->cmyk 255 255 255) ; => (0 0 0 0)
(rgb->cmyk 0 0 0)  ; => (0 0 0 1)