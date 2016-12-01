;;
;; Ejercicio 1
;; Dada la lista (4 2 3 (6 5 1) (9) (7 (10 (11 13 (14))))), escribe una expresión en Scheme para cada uno de los siguientes apartados:
;;
(define lista (list 4 2 3 (list 6 5 1) (list 9) (list 7 (list 10 (list 11 13 (list 14))))))
;;
;; Que devuelva el 5º elemento
;;
(car(cdr(cdr(cdr(cdr lista)))))
;;
;; Que devuelva el 6º elemento
;;
(car(cdr(cdr(cdr(cdr(cdr lista))))))
;;
;; Que devuelva el 2º elemento del 4º elemento
;;
(car(cdr(car(cdr(cdr(cdr lista))))))
;;
;; Que devuelva el 1º elemento del 2º elemento del 6º elemento
;;
(car(car(cdr(car(cdr(cdr(cdr(cdr(cdr lista)))))))))

;;
;; Ejercicio 2
;; Escribe el procedimiento (mayor-de-tres n1 n2 n3) que reciba tres números como argumento y devuelva el mayor de los tres, 
;; intentando que el número de condiciones sea mínima.
;; (mayor-de-tres 2 8 1) ⇒ 8 (mayor-de-tres 3 0 3) ⇒ 3
;;
(define (mayor-de-tres n1 n2 n3) 
 (if (> n1 n2) 
     (if (> n1 n3) n1 n3) 
     (if(> n2 n3) n2 n3)
  )
)

;;
;; Ejercicio 3
;; Escribe la función (engloba? a1 a2 b1 b2) que recibe dos intervalos de números enteros definidos por los valores de inicio y fin de cada uno de ellos: 
;; [a1, a2] para el primer intervalo y [b1, b2] para el segundo. La función debe comprobar si uno de los intervalos engloba al otro.
;; No hay que comprobar errores, asumimos que siempre se van a realizar llamadas correctas a la función en las que siempre se va a cumplir que a1 <= a2 y b1 <= b2.
;; (engloba? 4 10 5 9) ⇒ #t --[-[----]-]-  4 5    9 10
;; (engloba? 4 9 4 15) ⇒ #t --[[------]----]-  4       9   15
;; (engloba? 2 6 4 8) ⇒ #f --[--[--]--]-  2  4  6  8 
;; 
(define (engloba? a1 a2 b1 b2) 
  (if
    (or (and (<= a1 b1) (<= b2 a2)) (and (<= b1 a1) (<= a2 b2))) #t #f
   )
)

;;
;; Ejercicio 4
;; Escribe la función (interseccion a1 a2 b1 b2) que recibe dos intervalos de números enteros definidos por los valores de inicio y 
;; fin de cada uno de ellos: [a1, a2] para el primer intervalo y [b1, b2] para el segundo. 
;; La función debe devolver una pareja con el intervalo resultante de la intersección o la lista vacía en el caso en que no intersecten.
;; Es recomendable construir una función auxiliar que compruebe si los intervalos intersectan.
;;(interseccion 4 7 5 12) ⇒ (5 . 7) --[-[--]-----]-  4 5  7    12
;;(interseccion 4 9 12 15) ⇒ () --[-----]---[----]-  4     9   12  15
;;(interseccion 2 5 5 8) ⇒ (5 . 5) --[---[]---]  2   5    8
;;
(define (interseccion a1 a2 b1 b2)
   (if (and (<= b1 a2) (<= a1 b2))
      (cons (max a1 b1) (min a2 b2))
      '())
 )

;;
;; Ejercicio 5
;; Existen muchos formatos para representar el color. 
;; El más conocido es el RGB, que especifica el nivel de rojo (R), verde (G) y azul (B), en una escala de 0 a 255. 
;; Otro formato conocido es el CMYK, que especifica el nivel de cyan (C), magenta (M), amarillo (Y) y negro (K) en una escala de 0.0 a 1.0. 
;; Escribe el procedimiento (rgb->cmyk r g b) que toma los 3 valores RGB y devuelve una lista con los valores convertidos a las cuatro componentes CMYK.
;; (rgb->cmyk 75 0 130) ⇒ (11/26 1 0 25/51) 
;; (rgb->cmyk 150 10 255) ⇒ (7/17 49/51 0 0) 
;; (rgb->cmyk 255 255 255) ⇒ (0 0 0 0) 
;; (rgb->cmyk 0 0 0) ⇒ (0 0 0 1)
;; La forma de conversión es la siguiente: si los valores RGB son todos 0, entonces los CMY son todos 0 y el K (negro) es 1. 
;; 
(define (w r g b)
  (max (/ r 255)(/ g 255)(/ b 255))
)

(define (c w r)
  (/ (- w (/ r 255)) w)
)

(define (m w g)
  (/ (- w (/ g 255)) w)
)

(define (y w b)
  (/ (- w (/ b 255)) w)
)

(define (k w) 
  (- 1 w)
)

(define (rgb->cmyk r g b)
 (if(and(= r 0)(= g 0)(= b 0)) (list 0 0 0 1) ;; Si RGB son 0
  (list (c (w r g b) r) (m (w r g b) g) (y (w r g b) b) (k (w r g b))))
)