;;
;;Ejercicio 1
;;En clase de teoría hemos visto que el símbolo cond es una forma especial. 
;;Vamos a crear una función new-cond que tome como argumentos 2 booleanos ( expr1 y expr2) y 3 valores a devolver ( then1, then2 y else):
(define (new-cond expr1 then1 expr2 then2 else)     
  (cond
    (expr1 then1)
    (expr2 then2)
    (else else)))
;;En principio, parece que la función new-cond es equivalente a la forma especial cond. 
;;Por ejemplo, el resultado de evaluar las siguientes expresiones es el mismo:
;;(cond ((= 2 1) (+ 1 1)) ((> 3 2) (+ 2 3)) (else (- 10 3)))  
;;(new-cond (= 2 1) (+ 1 1) (> 3 2) (+ 2 3) (- 10 3))
;;Sin embargo, no siempre pasa esto. Por ejemplo, en las siguientes expresiones:
;;(cond ((= 2 2) 1) ((> 3 2) 2) (else (/ 3 0)))  
;;(new-cond (= 2 2) 1 (> 3 2) 2 (/ 3 0))
;; a) Explica detalladamente por qué cond y new-cond funcionan de forma distinta.
;;
;; Cond, al ser una forma especial, evalúa la primera condición que se encuentra y , si es verdadera, devuelve su valor y no continúa evaluando las demás, 
;; en caso contrario, comenzará a evaluar el resto de las condiciones hasta que encuentre alguna verdadera o llegue al final, a la particula else. Sin embargo
;; new-cond, al ser una función realizada con define, evalúa todos sus argumentos antes de ejecutar el cuerpo de la funión, es por esto que encuentra un error en los
;; argumentos y no llega a ejecutar el cuerpo siquiera de la función.
;;
;; b) Comprueba ahora el funcionamiento de las primitivas and y or. ¿Son formas especiales o funciones? ¿Por qué? Pon ejemplos y explica tu respuesta detalladamente
;;
(and #t #f (/ 3 0))
;(and #t #t (/ 3 0))
(and #f #f)
(or #t #f (/ 3 0))
(or #t #t)
;(or #f #f (/ 3 0))
;; 
;; Tanto la primitiva and como la primitiva or son formas especiales, ya que no necesitas saber el número de parámetros que se pueden o no pasarseles y evalúan sus
;; argumentos dentro del cuerpo de su definición.
;; Según su definición, or devolverá true #t si como mínimo uno de sus argumentos es verdadero, en el momento en el que encuentra que el primer argumento es verdadero
;; deja de evaluar el resto, sin llegar a evaluar el argumento con el error, esto ya dentro del cuerpo de su definición. Por el contrario, and devolverá true si y solo
;; si todos y cada uno de sus argumentos son verdaderos. Cuando ejecutas (and #t #f (/ 3 0)) ó (or #t #f (/ 3 0)) nos encontramos con que no se producen errores, ya que
;; las funciones terminan su ejecución antes de encontrar el error, lo que no pasa con (and #t #t (/ 3 0)) o (or #f #f (/ 3 0)) en los que la ejecución continúa hasta
;; que se encuentra un error y se corta el hilo de ejecución.
;;


;;
;; Ejercicio 2
;;
;; Escribe el predicado (maximo lista) que reciba una lista numérica como argumento y devuelva el mayor número de la lista.
;; Suponemos listas de 1 o más elementos. No puedes utilizar la función max de Scheme. 
;; Puedes definir y utilizar una función auxiliar mayor.
;; La formulación recursiva del caso general podemos expresarla de la siguiente forma:
;;
;; Formulación recursiva de (maximo lista):
;;
;; El máximo de los elementos de una lista es el mayor entre  
;; el primer elemento de la lista y el máximo del resto de la lista 
;;
;; (maximo '(2 3 4 5 6 7 8)) ⇒ 8  
;; (maximo '(2 3 4 8 5 6 7)) ⇒ 8
;;

(define (maximo lista)
  (if (null? (cdr lista)) (car lista) 
      (if(> (car lista) (maximo(cdr lista)))  (car lista) (maximo(cdr lista)))
  )
)

;;
;;Pruebas
;;
(maximo '(2 3 4 5 6 7 8))  
(maximo '(2 3 4 8 5 6 7)) 
(maximo '(1 58 12 5 8 115 5 7 2 -85))
(maximo '(2 3 4 8 5 6 7 12 -5 48 0 20))

;;
;; Ejercicio 3
;;
;; a) Diseña la función recursiva (ordenada? lista-nums) que recibe como argumento una lista de números. 
;; Devuelve #t si los números de la lista están ordenados de forma creciente y #f en caso contrario.
;; Suponemos listas de 1 o más elementos.
;; Escribe primero la formulación recursiva del caso general, y después realiza la implementación en Scheme.
;; Ejemplos:
;; (ordenada? '(-1 23 45 59 100)) ⇒ #t  
;; (ordenada? '(12 45 -1 293 1000)) ⇒ #f  
;; (ordenada? '(3)) ⇒ #t
;;

(define (ordenada? lista-nums)
  (if(null? (cdr lista-nums )) #t 
     (if(<= (car lista-nums) (car(cdr lista-nums))) (ordenada? (cdr lista-nums)) #f)
  )
)

;;
;; Pruebas
;;
(ordenada? '(-1 23 45 59 100))
(ordenada? '(12 45 -1 293 1000))
(ordenada? '(3))

(ordenada? '(3 4 5 6 881))
(ordenada? '(12 3))

;;
;; b) Diseña la función recursiva (ordenada-palabra? pal) que recibe como argumento una cadena y devuelve #t si los caracteres de la cadena 
;; están ordenados de forma ascendente (alfabéticamente). 
;; Ejemplos:
;; (ordenada-palabra? "abcdefg") ⇒ #t
;; (ordenada-palabra? "hola") ⇒ #f
;;
(define (primera palabra);;Devuelve la primera letra de una palabra
  (string-ref palabra 0)
 )

(define (resto palabra);;Devuelve una palabra sin su primera letra
 (substring palabra 1)
 )

(define (ordenada-palabra? palabra);; Como recibe una cadena no podemos usar null, por lo que comparamos con cadena vacía
  (if(equal? "" (resto palabra)) #t
     (and(char<=? (primera palabra) (primera(resto palabra)))
        (ordenada-palabra? (resto palabra))
     )
  )
)

;;
;; Pruebas
;;
(ordenada-palabra? "abcdefg") 
(ordenada-palabra? "hola") 

(ordenada-palabra? "emo")
(ordenada-palabra? "ppsy") 
(ordenada-palabra? "psychotic") 

;;
;; Ejercicio 4
;;
;; a) Define el predicado de la práctica anterior utilizando parejas como parámetros en lugar de números. 
;; Define para ello la nueva función (engloban-intervalos? a b). 
;; Puedes usar la función engloba? definida en la práctica 1 y copiar su definición en esta.
;; Ejemplos:
;; (define i1 (cons 4 9))  
;; (define i2 (cons 3 10))
;; (define i3 (cons 12 15))
;; (define i4 (cons 8 19))
;; (engloban-intervalos? '(4 . 10) '(5 . 9)) ⇒ #t
;; (engloban-intervalos? i1 i2) ⇒ #t  
;; (engloban-intervalos? i1 i4) ⇒ #f

(define (engloban-intervalos? a b) 
  (if
    (or (and (<= (car a) (car b)) (<= (cdr b) (cdr a))) (and (<= (car b) (car a)) (<= (cdr a) (cdr b)))) #t #f
   )
)

;;
;; Pruebas
;;

(define i1 (cons 4 9))  
(define i2 (cons 3 10))
(define i3 (cons 12 15))
(define i4 (cons 8 19))
(engloban-intervalos? '(4 . 10) '(5 . 9)) 
(engloban-intervalos? i1 i2)   
(engloban-intervalos? i1 i4) 

(engloban-intervalos? '(4 . 10) '(10 . 13)) 
(engloban-intervalos? '(5 . 9) '(4 . 10)) 
(engloban-intervalos? '(56 . 58) '(57 . 58)) 
;; No podemos comprobar correctamente los intervalos cuando uno de ellos o parte de alguno de ellos es el conjunto vacío

;; b) Define las funciones (union-intervalos a b) e (interseccion-intervalos a b).
;; La función (union-intervalos a b) debe devolver el intervalo (pareja) que englobe a los dos. 
;; La función (interseccion-intervalos a b) devolverá la intersección de los intervalos a y b.
;; En el caso en que no exista intersección, se deberá devolver el símbolo 'vacio. 
;; Puedes usar la función definida en la práctica 1 y copiar su definición en esta.
;; 
;; Ejemplos de union-intervalos:
;; (union-intervalos '(4 . 10) '(3 . 8)) ⇒ (3 . 10)  
;; (union-intervalos i2 i3) ⇒ (3 . 15)
;; 
;; Ejemplos de interseccion-intervalos:
;; (interseccion-intervalos '(4 . 10) '(8 . 15)) ⇒ (8 . 10)  
;; (interseccion-intervalos i1 i2)) ⇒ (4 . 9)  
;; (interseccion-intervalos i1 i3)) ⇒ 'vacio
;;
;; El símbolo 'vacio también podrá utilizarse como parámetro en las funciones union-intervalos y interseccion-intervalos para representar un intervalo vacío.
;; Funcionará de la siguiente forma. Supongamos un intervalo cualquiera, por ejemplo '(8 . 20):
;; (union-intervalos 'vacio '(8 . 20)) ⇒ (8 . 20)  
;; (union-intervalos '(8 . 20) 'vacio) ⇒ (8 . 20)  
;; (union-intervalos 'vacio 'vacio) ⇒ 'vacio  
;; (interseccion-intervalos 'vacio '(8 . 20)) ⇒ 'vacio 
;; (interseccion-intervalos '(8 . 20) 'vacio) ⇒ 'vacio

(define (union-intervalos a b)
  (if(null? a) b ;; Si a es '()
      (if(null? b) a ;; Si b es '()
  (if (and (<= (car a) (car b)) (<= (cdr b) (cdr a))) a
        (if(and (<= (car b) (car a)) (<= (cdr a) (cdr b))) b ;;Si uno engloba a otro completamente
          (cons (min(car a) (car b)) (max(cdr a)(cdr b))) )
   )))
)

(define (interseccion-intervalos a b)
   (if(null? a) '() ;; Si a es '()
      (if(null? b) '() ;; Si b es '()
         (if (and (<= (car b)(cdr a)) (<= (car a)(cdr b)))
      (cons (max (car a)(car b)) (min (cdr a)(cdr b))) '())))
)

;;
;;Pruebas
;;
;; Se presupone que la pareja del intervalo está ordenada de menor a mayor
;;
;; Union
;;
(union-intervalos '(4 . 10) '(3 . 8))
(union-intervalos i2 i3)

(union-intervalos '(2 . 3) '(7 . 11));; Dos intervalos completamente separados se unen de este modo, no lo especifica en la practica por lo que lo tomo así

;;
;;Intersección
;;
(interseccion-intervalos '(4 . 10) '(8 . 15))   
(interseccion-intervalos i1 i2)   
(interseccion-intervalos i1 i3) 

(interseccion-intervalos '(25 . 30) '(-8 . 20))
(interseccion-intervalos '(1 . 15) '(10 . 12))
(interseccion-intervalos '(1 . 15) '(10 . 129))


;;
;;Unión e intersección con vacio
;;
(union-intervalos '() '(8 . 20))   
(union-intervalos '(8 . 20) '())
(union-intervalos '() '())  
(interseccion-intervalos '() '(8 . 20)) 
(interseccion-intervalos '(8 . 20) '())

;;no se pueden hacer más comprobaciones ya que nos las proporcionais todas ...

;; 
;; Ejercicio 5
;;
;; Define utilizando la recursión las funciones (union-lista-intervalos lista-intervalos) e (interseccion-lista-intervalos lista-intervalos)
;; que devuelven el intervalo (pareja) resultante de la suma o intersección de una lista de intervalos. 
;; Debes utilizar las funciones definidas en el ejercicio anterior.
;; Escribe primero la formulación recursiva del caso general y realiza después la implementación en Scheme.
;; Ejemplo:
;; (union-lista-intervalos '((2 . 12) (-1 . 10) (8 . 20))) ⇒ (-1 . 20)  
;; (interseccion-lista-intervalos '((12 . 30) (-8 . 20) (13 . 35))) ⇒ (13 . 20)  
;; (interseccion-lista-intervalos '((25 . 30) (-8 . 20) (13 . 35))) ⇒ 'vacio

;; Formulación recursiva de (union-lista-intervalos lista-intervalos):
;;
;; La unión de una lista de intervalos es la union de los dos primeros intervalos
;; de la lista con la union del resto de los intervalos de la lista
;;

(define (union-lista-intervalos lista-intervalos)
 (if(null? (cdr lista-intervalos)) (car lista-intervalos)
   (union-lista-intervalos (append (list (union-intervalos (car lista-intervalos) (car(cdr lista-intervalos)))) (cdr(cdr lista-intervalos))))
  )
)

;;
;;Pruebas
;
;(union-lista-intervalos '((2 . 12) (-1 . 10) (8 . 20)))
(union-lista-intervalos '((7 . 50) (-1 . 10) (8 . 20) (-8 . 16) (2 . 52)))
(union-lista-intervalos '(() (-1 . 10) (8 . 20) (-8 . 16) ()))

;; Formulacion recursiva de (interseccion-lista-intervalos lista-intervalos):
;;
;; La intersección de una lista de intervalos es la intersección de los dos primeros intervalos
;; de la lista con la intersección del resto de los intervalos de la lista
;;

(define (interseccion-lista-intervalos lista-intervalos)
 (if(null? (cdr lista-intervalos)) (car lista-intervalos)
   (interseccion-lista-intervalos (append (list (interseccion-intervalos (car lista-intervalos) (car(cdr lista-intervalos)))) (cdr(cdr lista-intervalos))))
  )
)
;;
;; Pruebas
;;
(interseccion-lista-intervalos '((12 . 30) (-8 . 20) (13 . 35)))
(interseccion-lista-intervalos '((25 . 30) (-8 . 20) (13 . 35)))

(interseccion-lista-intervalos '((13 . 30) (-8 . 20) (25 . 35) (5 . 6)))
(interseccion-lista-intervalos '((-8 . 20) (13 . 35) ()))