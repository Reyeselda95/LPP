;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creado por Mauricio Trujillo para LPP UA 2015  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  EL LENGUAJE DEBE SER 'Muy Grande' o 'Pretty Big' ;;;;;;

;---Alguna librería-------------------------------------------------------

(require graphics/turtles) 

;---Algunas funciones auxiliares------------------------------------------
(define (initTurtle)
  (turtles #t))

(define (clearTurtle)
  (turtles #t)
  (clear))

(define (CerrarTortuga2)
  (clear)
  (turtles #f))

(define (salir2)
  (turtles #f)
  (send ventana show #f))


;---Figuras de la práctica 5 ----------------------------------------------
;
;Ejercicio 2 - El árbol -
(define (arbol l) 
  (if (> l 0)
      (begin
       (draw l)
       (turn 30)
       (arbol (- l 10))
       (turn -60)
       (arbol (- l 10))
       (turn 30)
       (move (- (* l 1))))))   

  
(define (pintarArbol n)
  (initTurtle)
  (clearTurtle)
  
  (turn 90)
  (arbol n)
  (turn -90))


; Ejercicio 3 - La espiral -----------------------------------------------
(define (trazo lado angulo)
  (draw lado)
  (turn angulo))
 
(define (espiral lado inc angulo max-lado)
    (if (< lado max-lado)
        (begin
          (trazo lado angulo)
          (espiral (+ lado inc) inc angulo max-lado))))


;---Ejercicio 3 -- Girador ------------------------------------------------
(define (cuadrado n)
  (draw n)
  (turn 90)
  (draw n)
  (turn 90)
  (draw n)
  (turn 90)
  (draw n)
  (turn 90))

(define (cuadrado-girado lado giro)
    (turn giro)
    (cuadrado lado))

(define (girador lado inc giro)
  (clearTurtle)
  (initTurtle)
  
  (girador-aux lado inc giro 0))
   
(define (girador-aux lado inc giro girado)
  (if (< girado 360)
      (begin
        (cuadrado-girado lado giro)
        (girador-aux (+ lado inc) inc giro (+ girado giro)))))



;---Figuras del foro moodle LPP 2015-------------------------------------------
;
; Figura polígonos - Gracias a Alejandro Reyes Albillar -

(define (dibuja-poligono lados lado angulo)
  (if (not(= lados 0))
    (begin
    (draw lado)
    (turn angulo)
    (dibuja-poligono (- lados 1) lado angulo))) 
)

(define (poligono lados lado)
  (dibuja-poligono lados lado (/ 360 lados))
)

(define (dibujarPoligono)        
  (clearTurtle)
  (initTurtle)
         
  (poligono 20 50)
  (poligono 18 50)
  (poligono 16 50)
  (poligono 14 50)
  (poligono 12 50)
  (poligono 10 50)
  (poligono 9 50)
  (poligono 8 50)
  (poligono 7 50)
  (poligono 6 50)
  (poligono 5 50)
  (poligono 4 50)
  (poligono 3 50)
  (poligono 2 50)
  (poligono 1 50)

  (turn 180)
  (poligono 20 50)
  (poligono 18 50)
  (poligono 16 50)
  (poligono 14 50)
  (poligono 12 50)
  (poligono 10 50)
  (poligono 9 50)
  (poligono 8 50)
  (poligono 7 50)
  (poligono 6 50)
  (poligono 5 50)
  (poligono 4 50)
  (poligono 3 50)
  (poligono 2 50)
  (poligono 1 50)
  (turn 180))

;-----Figura pétalos - Gracias a Alejandro Reyes Albillar --------------------

(define (poligono-recur lados lado)  
  (dibuja-poligono lados lado (/ 360 lados))
  
  (if (not(= lados 2))
      (poligono-recur (- lados 1) lado)))

(define (flower-draw petalos repet)
  (if(not(= repet 0))
   (begin
    (turn (/ 360 petalos))
    (poligono-recur 80 20)
    (turn 180)
    (flower-draw petalos (- repet 1)))))

;;Dibuja una flor de 'X' pétalos mayor que 2 ----------------------------------
(define(flower petalos)
  (clearTurtle)
  (initTurtle)
  
  (if (> petalos 2)
      (if(odd? petalos)
         (flower-draw (* petalos 2) petalos)
         (flower-draw petalos  petalos))))



;------Figura el ojo de sauron -- Gracias a Antonio Miguel Rodríguez García ----
;
; la función cuadrado ya está definida en Girador-

(define (cuadrado-girado lado giro)
  (turn giro)
  (cuadrado lado)) 

(define (girador2 lado grado)
  (if (>= grado 360) 
      (cuadrado-girado lado grado)
      (girador2 (+ lado 0) (+ 0.2 grado)))
  (cuadrado-girado lado grado))



;----------------------------------------------------------------------------------
;----------------------------------------------------------------------------------
;            CÓDIGO DE LA INTERFAZ
;----------------------------------------------------------------------------------
;----------------------------------------------------------------------------------

;-----librerías-----------
(require racket/gui/base)
(require racket/draw)
(require racket/gui)

;; barra de título de ventana principal
(define ventana (new frame% 
                     [label "El caloret del Racket"]
                     [width 250]
                     [height 335]))

;; título
(define titulo (new canvas% 
                    [parent ventana]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-scale 1 1)
                       (send dc set-text-foreground "blue")
                       (send dc draw-text "Keep Calm" 98 0)
                       (send dc draw-text "and" 123 20)
                       (send dc draw-text "Trust in the Recursivity" 50 40))]))

;;definimos las pestañas
(define opciones (list "Práctica 5" "Foro LPP"))

;;función que actualiza los estados de los eventos en la ventana principal
;;cada vez que hacemos clic una de las pestañas
(define tab (new tab-panel%
                 [parent ventana]
                 [choices opciones]
                 [callback
                 (lambda (t c)
                   (let* ([ix (send tab get-selection)]
                          [ix-str (number->string (+ ix 1))]
                          [nombre-ficha (send tab get-item-label ix)])
                     (send mensaje set-label (string-append "Estás en la pestaña " ix-str 
                                                            " con nombre '" nombre-ficha "'"))
                     (send tab delete-child (first (send tab get-children)))
                     (send tab add-child (list-ref lista-paneles ix))))]))


;;función que actualiza los estados de los eventos en la ventana principal
;;cada vez que hacemos clic em un botón
(define mensaje (new message%
                     [parent ventana]
                     [label "Aquí se mostrarán los eventos de los objetos"]
                     [auto-resize #t]))

;;renglón horizontal para poner los botones de 'cerrar tortuga' y 'salir'
;;de la ventana principal
(define botonera (new horizontal-panel% [parent ventana] [alignment '(center center)]))

;;botón en la ventana principal
(define cerrarTortuga (new button%
                           [parent botonera]
                           [label "Cerrar Tortuga"]
                           [callback (lambda (button event) (CerrarTortuga2))]))

;;botón en la ventana principal
(define Salir (new button%
                   [parent botonera]
                   [label "Salir"]
                   [callback (lambda (button event) (salir2))]))

;;créditos
(define mensaje2 (new message%
                     [parent ventana]
                     [label "Creado por Mauricio Trujillo para LPP UA 2015"]
                     [auto-resize #t]))

;;para poder usar las pestañas correctamente 
;; y que los botones no salgan repetidos en ambas
(define panel0 (new vertical-panel% [parent tab]))
(define panel1 (new vertical-panel% [parent tab][style '(deleted)]))

(define lista-paneles (list panel0 panel1))



;;-----PESTAÑA PRÁCTICA 5-----------------------------------------------------------

;crear un renglón para poner los botones de las figuras en la misma línea
(define panel-botones (new horizontal-pane% 
                           [parent panel0]
                           [alignment '(center center)]))

;----------------------------------------------------------------------------------------

;--Botón principal para el árbol---------------------------------------------------------
(define boton1 (new button%
                    [parent panel-botones]
                    [label "Árbol"]
                    [callback 
                     (lambda (b e)
                       (let* ()
                         (send mensaje set-label (string-append "Dibujando '" (send b get-label) "'"))
                         (send ventana-de-dialogo show #t)))]))


;-----------ventana para preguntar el tamaño del árbol------------------------------------

;crear la ventana y agregarle el título
(define ventana-de-dialogo (new dialog% 
                                [label "Dibujar el árbol"]))

;crear el campo de escritura, valor por defecto y añadirlo a la ventana
(define tamArbol (new text-field%                      
                      [parent ventana-de-dialogo]
                      [label "Longitud aŕbol: "]
                      [init-value "80"]))

;crear un renglón para poner los botones 'dibujar...' y 'salir' en la misma línea
(define panelArbol (new horizontal-pane% 
                            [parent ventana-de-dialogo]
                            [alignment '(center center)]))

;crear el botón de dibujar con su evento y agregarlo a la ventana
(define botonTor (new button% 
                   [label "Dibujar con Tortuga"]
                   [parent panelArbol]
                   [callback 
                    (lambda (button event)
                      (let* ()
                        (pintarArbol (string->number (send tamArbol get-value)))
                        (send ventana-de-dialogo show #f)))]))

;crear el botón salir con su evento y agregarlo a la ventana
(define salirArb (new button% [label "Salir"]
                   [parent panelArbol]
                   [callback 
                    (lambda (button event) 
                      (send ventana-de-dialogo show #f))]))

;nota con aclaraciones de uso para pintar la figura
(define nota1 (new message%
                     [parent ventana-de-dialogo]
                     [label "Uso: Entero > 0 \nPor defecto '80'."]
                     [auto-resize #t]))
;;-----------------------------------------------------------------------------------------


;---Botón principal para la Espiral----------------------------
(define boton2 (new button%
                    [parent panel-botones]
                    [label "Espiral"]
                    [callback 
                     (lambda (b e)
                       (let* ()
                         (send mensaje set-label (string-append "Dibujando '" (send b get-label) "'"))
                         (send ventana-de-dialogo2 show #t)))]))
                         

;-----------ventana para preguntar el tamaño de la Espiral------------------------------------

;crear la ventana y agregarle el título
(define ventana-de-dialogo2 (new dialog% 
                                [label "Dibujar la Espiral"]))

;crear el campo de escritura, valor por defecto y añadirlo a la ventana
(define tamEspiral (new text-field%                      
                      [parent ventana-de-dialogo2]
                      [label "Datos: "]
                      [init-value "3 2 060 200"]))

;crear un renglón para poner los botones 'dibujar...' y 'salir' en la misma línea
(define panelEspiral (new horizontal-pane% 
                            [parent ventana-de-dialogo2]
                            [alignment '(center center)]))

;crear el botón de dibujar con su evento y agregarlo a la ventana
(define botonTor2 (new button% 
                   [label "Dibujar con Tortuga"]
                   [parent panelEspiral]
                   [callback 
                    (lambda (button event)
                      (let* ([cadena (send tamEspiral get-value)])
                        (clearTurtle)
                        (initTurtle)
                        (espiral
                         (string->number (string (string-ref cadena 0)))
                         (string->number (string (string-ref cadena 2)))
                         (string->number (substring cadena 4 7))
                         (string->number (substring cadena 8 11)))
                        (send ventana-de-dialogo2 show #f)))]))

;crear el botón salir con su evento y agregarlo a la ventana
(define salirEsp (new button% [label "Salir"]
                   [parent panelEspiral]
                   [callback 
                    (lambda (button event) 
                      (send ventana-de-dialogo2 show #f))]))

;nota con aclaraciones de uso para pintar la figura
(define nota2 (new message%
                     [parent ventana-de-dialogo2]
                     [label "Uso: 'lado inc angulo lado-max' separado por un espacio.\nPor defecto '3 6 060 200'\nlado -> un entero\ninc   -> un entero\nangulo   --> tres enteros. Si '60' -> '060'\nlado-max -> idem angulo"]
                     [auto-resize #t]))
;;----------------------------------------------------------------------------------------



;;----Botón principal para el girador --------------------------------------------------
(define boton3 (new button%
                    [parent panel-botones]
                    [label "Girador"]
                    [callback 
                     (lambda (b e)
                       (let* ()
                         (send mensaje set-label (string-append "Dibujando '" (send b get-label) "'"))
                         (send ventana-de-dialogo3 show #t)))]))


;-----------ventana para preguntar el tamaño de la Espiral------------------------------------

;crear la ventana y agregarle el título
(define ventana-de-dialogo3 (new dialog% 
                                [label "Dibujar el Girador"]))

;crear el campo de escritura, valor por defecto y añadirlo a la ventana
(define tamGirador (new text-field%                      
                      [parent ventana-de-dialogo3]
                      [label "Datos: "]
                      [init-value "20 2 06"]))

;crear un renglón para poner los botones 'dibujar...' y 'salir' en la misma línea
(define panelGirador (new horizontal-pane% 
                            [parent ventana-de-dialogo3]
                            [alignment '(center center)]))

;crear el botón de dibujar con su evento y agregarlo a la ventana
(define botonGir2 (new button% 
                   [label "Dibujar con Tortuga"]
                   [parent panelGirador]
                   [callback 
                    (lambda (button event)
                      (let* ([cadena (send tamGirador get-value)])
                        (clearTurtle)
                        (initTurtle)
                        (girador
                         (string->number (substring cadena 0 2))
                         (string->number (string (string-ref cadena 3)))
                         (string->number (substring cadena 5 7)))
                        (send ventana-de-dialogo3 show #f)))]))

;crear el botón salir con su evento y agregarlo a la ventana
(define salirGir (new button% [label "Salir"]
                   [parent panelGirador]
                   [callback 
                    (lambda (button event) 
                      (send ventana-de-dialogo3 show #f))]))

;nota con aclaraciones de uso para pintar la figura
(define nota3 (new message%
                     [parent ventana-de-dialogo3]
                     [label "Uso: 'lado inc giro' separado por un espacio.\nPor defecto '20 2 06'.\nlado -> dos enteros. Si  '1' -> '01'\ninc   -> un entero de 1 a 9 \ngiro  -> idem lado "]
                     [auto-resize #t]))

;;----------------------------------------------------------------------------------------





;;-------PESTAÑA FORO LPP 2015 ------------------------------------------------------


;; creamos un renglón horizontal para poner los botones principales
(define panel-botones2 (new horizontal-pane% 
                            [parent panel1]
                            [alignment '(center center)]))


;-------Botón principal Polígonos ---------------------------------------------------
;;
; a esta figura en principio no se le pasaban parámetros. Se podría adaptar.

(define boton4 (new button%
                    [parent panel-botones2]
                    [label "Polígonos"]
                    [callback 
                     (lambda (b e)
                       (let* ()
                         (send mensaje set-label (string-append "Dibujando '" (send b get-label) "'"))
                         (dibujarPoligono)))]))

;;-------Botón principal Pétalos ----------------------------------------------------

(define boton5 (new button%
                    [parent panel-botones2]
                    [label "Pétalos"]
                    [callback 
                     (lambda (b e)
                       (let* ()
                         (send mensaje set-label (string-append "Dibujando '" (send b get-label) "'"))
                         ;(flower 4)
                         (send ventana-de-dialogo4 show #t)))]))

;-----------ventana para preguntar el tamaño de los pétalos------------------------------------

;crear la ventana y agregarle el título
(define ventana-de-dialogo4 (new dialog% 
                                [label "Dibujar los Pétalos"]))

;crear el campo de escritura, valor por defecto y añadirlo a la ventana
(define tamPetalos (new text-field%                      
                      [parent ventana-de-dialogo4]
                      [label "Cantidad Pétalos: "]
                      [init-value "4"]))

;crear un renglón para poner los botones 'dibujar...' y 'salir' en la misma línea
(define panelPetalos (new horizontal-pane% 
                            [parent ventana-de-dialogo4]
                            [alignment '(center center)]))

;crear el botón de dibujar con su evento y agregarlo a la ventana
(define botonPet2 (new button% 
                   [label "Dibujar con Tortuga"]
                   [parent panelPetalos]
                   [callback 
                    (lambda (button event)
                      (flower (string->number (send tamPetalos get-value)))
                      (send ventana-de-dialogo4 show #f))]))

;crear el botón salir con su evento y agregarlo a la ventana
(define salirPet (new button% [label "Salir"]
                   [parent panelPetalos]
                   [callback 
                   (lambda (button event) 
                     (send ventana-de-dialogo4 show #f))]))

;nota con aclaraciones de uso para pintar la figura
(define nota4 (new message%
                     [parent ventana-de-dialogo4]
                     [label "Uso: Mínimo nº de pétalos = 3\nPor defecto '4'"]
                     [auto-resize #t]))

;;----------------------------------------------------------------------------------------


;;--------------Botón principal del ojo de sauron--------------------------------------
(define boton6 (new button%
                    [parent panel-botones2]
                    [label "Ojo Sauron"]
                    [callback 
                     (lambda (b e)
                       (let* ()
                         (send mensaje set-label (string-append "Dibujando '" (send b get-label) "'"))
                         ;(clearTurtle)
                         ;(initTurtle)
                         ;(girador2 10 300)
                         (send ventana-de-dialogo5 show #t)))]))

;-----------ventana para preguntar el tamaño del ojo sauron------------------------------------

;crear la ventana y agregarle el título
(define ventana-de-dialogo5 (new dialog% 
                                [label "Dibujar el ojo de Sauron"]))

;crear el campo de escritura, valor por defecto y añadirlo a la ventana
(define tamOjo (new text-field%                      
                      [parent ventana-de-dialogo5]
                      [label "Datos: "]
                      [init-value "250 000"]))

;crear un renglón para poner los botones 'dibujar...' y 'salir' en la misma línea
(define panelOjo (new horizontal-pane% 
                            [parent ventana-de-dialogo5]
                            [alignment '(center center)]))

;crear el botón de dibujar con su evento y agregarlo a la ventana
(define botonOjo2 (new button% 
                   [label "Dibujar con Tortuga"]
                   [parent panelOjo]
                   [callback 
                    (lambda (button event)
                      (let* ([cadena (send tamOjo get-value)])
                        (clearTurtle)
                        (initTurtle)
                        (girador2
                         (string->number (substring cadena 0 3))
                         (string->number (substring cadena 4 7)))
                        (send ventana-de-dialogo5 show #f)))]))

;crear el botón salir con su evento y agregarlo a la ventana
(define salirOjo (new button% [label "Salir"]
                   [parent panelOjo]
                   [callback 
                    (lambda (button event) 
                      (send ventana-de-dialogo5 show #f))]))

;nota con aclaraciones de uso para pintar la figura
(define nota5 (new message%
                     [parent ventana-de-dialogo5]
                     [label "Uso: 'tamaño retina'\nTamaño -> Rango [1,999]. Si  '1' -> '001'\nRetina -> Rango [0,999]. Si '0' -> '000'\nPor defecto '250 000'"]
                     [auto-resize #t]))

;;----------------------------------------------------------------------------------------


;ejecutar la ventana principal de la aplicación
(send ventana show #t)