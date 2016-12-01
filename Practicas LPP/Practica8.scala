/**
 * @author ALEJANDRO REYES ALBILLAR
 * grupo MIERCOLES 13:00-15:00
 *
 */

// Ejercicio 1
// Implementa en Scala la función recursiva ordenada(lista: List[Int]): Boolean que recibe como argumento 
// una lista de valores de tipo Int y devuelve true si los números de la lista están ordenados de forma creciente 
// y false en caso contrario. Suponemos listas de 1 o más elementos.

def ordenada(lista: List[Int]): Boolean = {
  if (lista.tail.isEmpty)
    true
  else 
    if(lista.head<lista.tail.head)
      ordenada(lista.tail)
    else false
}
//
// Pruebas
//
assert(ordenada(List(1,2,3,4,5,6))==true)
assert(ordenada(List(1,2,3,7,5,6))==false)
assert(ordenada(List(1))==true)
//assert(ordenada(Nil)) => ERROR!!
//
    
// Ejercicio 2
// Escribe las funciones
// englobados(int1: (Int, Int), int2: (Int, Int)): Boolean 
// intersectan(int1:(Int, Int), int2: (Int, Int)): (Int, Int)
// que comprueban si dos intervalos están englobados y devuelven el intervalo de intersección de uno con el otro (ver ejemplos en la 
// práctica 1 de la asignatura). 
// Los intervalos se representan como tuplas de dos Int. 
// En el caso en que los dos intervalos no intersecten se debe devolver null.
    
// La función englobados definida en scheme es:
/*    (define (engloban-intervalos? a b) 
  (if
    (or (and (<= (car a) (car b)) (<= (cdr b) (cdr a))) (and (<= (car b) (car a)) (<= (cdr a) (cdr b)))) #t #f
   )
)
// Y la función intresectan:
    (define (interseccion-intervalos a b)
   (if(null? a) '() ;; Si a es '()
      (if(null? b) '() ;; Si b es '()
         (if (and (<= (car b)(cdr a)) (<= (car a)(cdr b)))
      (cons (max (car a)(car b)) (min (cdr a)(cdr b))) '())))
)*/
// Lo que equivale a:
    
def englobados(int1: (Int, Int), int2: (Int, Int)):Boolean={
    if (((int1._1  <= int2._1 ) && (int2._2 <= int1._2))||((int2._1  <= int1._1 ) && (int1._2 <= int2._2)))
        true
    else false
}

def max(int1:Int,int2:Int):Int={
    if(int1>=int2) int1 else int2
}

def min(int1:Int,int2:Int):Int={
    if(int1<=int2) int1 else int2
}

//Para esta función se supone que nunca se le puede pasar algo vacío
def intersectan(int1: (Int, Int), int2: (Int, Int)):(Int, Int)={
    if((int2._1 <= int1._2) && (int1._1 <= int2._2))
        (max(int1._1, int2._1),min(int1._2, int2._2))
    else null
}

//
// Pruebas
//
assert((intersectan((25 , 30) ,(-8 , 20)))==null)
assert((intersectan((1 , 15), (10 , 12)))==(10,12))
assert((intersectan((1 , 15), (10 , 129)))==(10,15))
    
// Englobados
def i1= (4, 9)  
def i2= (3, 10)
def i3= (12, 15)
def i4= (8, 19)
assert(englobados ((4 , 10) ,(5 , 9))== true) 
assert(englobados (i1, i2)==true)  
assert(englobados (i1, i4)==false)
assert(englobados((4 , 10), (10 , 13))==false) 
assert(englobados((5 , 9), (4 , 10))==true)
assert(englobados((56 , 58), (57 , 58))==true) 
//
    
// Ejercicio 3
// Escribe una versión recursiva pura y otra con recursión por la cola de la función cuadradoLista(lista: List[Int]): List[Int] 
// que devuelve una lista con los números de la lista original elevados al cuadrado.

// Recursiva pura
def cuadradoLista(lista: List[Int]):List[Int]={
    if (lista.tail.isEmpty)
        List(lista.head * lista.head)
    else
        List(lista.head*lista.head):::cuadradoLista(lista.tail)
}

def cuadradoListaAux(lista:List[Int]):List[Int]={
    if (lista.tail.isEmpty)
        List(lista.head * lista.head)
    else
        List(lista.head*lista.head):::cuadradoListaAux(lista.tail)
}

def cuadradoListaTail(lista:List[Int]):List[Int]={
    cuadradoListaAux(lista)
}

//
// Pruebas
//
assert(cuadradoLista(List(1,2,3,4,5,6,7,8))==List(1,4,9,16,25,36,49,64))
assert(cuadradoListaTail(List(1,2,3,4,5,6,7,8))==List(1,4,9,16,25,36,49,64))
//
    
    
// Ejercicio 4
// Implementa la función contienePatron(frase: String, patron: String): List[String] que recibe una frase 
// (cadena con palabras separadas por espacios) y un patrón a buscar en la frase. 
// El patrón es sencillamente una cadena. Tiene que devolver una lista de palabras que contienen el patrón dado.
    
def contienePatron(frase:String, patron:String):List[String]={
    val palabras=frase.split(' ')
    if (frase.isEmpty) Nil 
    else 
        if (palabras.head.contains(patron))
            List(palabras.head)::: contienePatron(palabras.tail.mkString(" "), patron)
        else 
            contienePatron(palabras.tail.mkString(" "), patron)
}

//
// Pruebas
//
assert (contienePatron("El perro de roque no es un robot", "ro")==List("perro","roque","robot"))
assert (contienePatron("El peperoni de pepe estaba perdiendo su color por uno peligroso","pe")==List("peperoni","pepe","perdiendo","peligroso"))
//
    
// Ejercicio 5
// Implementa la función cuentaOcurrencias(lista1: List[String], lista2: List[String]): List[(String, Int, Int)] que recibe dos listas de cadenas y
// devuelve una lista de tuplas de tipo (String, Int, Int). Las tuplas representan el número de veces que aparece la cadena en la primera y la segunda lista.
    
//Función cuenta: devolverá parejas, el primer elemento te dirá cual es el numero de veces que aparece el elemento en la lista, el segundo es el resto de palabras de la lista sin contar los elementos buscados
    
def cuenta(elem: String, lista: List[String], listasol: List[String], veces:Int):(Int, List[String])={
    if(lista.isEmpty)
        (veces, listasol)
    else 
        if (elem == lista.head)
            cuenta(elem, lista.tail, listasol, veces+1)
        else
            cuenta(elem, lista.tail, listasol:::List(lista.head), veces)        
}

//Función cuentaAux
def cuentaAux(sol:List[(String, Int, Int)],l1: List[String],l2: List[String]):List[(String, Int, Int)]={
    if(l1.isEmpty && l2.isEmpty)
        sol //Devuelves la solución
    else
        if(l1.isEmpty)
        //Comprobar segunda lista si la primera esta vacia
            cuentaAux(sol:::List((l2.head, cuenta(l2.head, l1, Nil, 0)._1, cuenta(l2.head, l2, Nil, 0)._1)), cuenta(l2.head, l1, Nil, 0)._2, cuenta(l2.head, l2, Nil, 0)._2)
        else
        //Comprobar primera lista
            cuentaAux(sol:::List((l1.head, cuenta(l1.head, l1, Nil, 0)._1, cuenta(l1.head, l2, Nil, 0)._1)), cuenta(l1.head, l1, Nil, 0)._2, cuenta(l1.head, l2, Nil, 0)._2)
    
}

//Función cuentaOcurrencias    
def cuentaOcurrencias (lista1: List[String], lista2: List[String]):List[(String, Int, Int)]={
    cuentaAux(Nil, lista1, lista2)
}


//
// Pruebas
//

assert(cuentaOcurrencias(List("pera", "melón", "pera", "manzana", "orégano"),     
                         List("perejil", "pera", "hierbabuena", "orégano", "perejil")) ==      
       List(("pera", 2, 1), 
            ("melón", 1, 0), 
            ("manzana", 1, 0), 
            ("orégano", 1, 1),  
            ("perejil", 0, 2),
            ("hierbabuena", 0, 1)))
    
assert(cuentaOcurrencias(List("mio", "kingdom", "hearts", "heart", "my", "kingdom", "is", "my", "wisdom"),
                         List("Caminante","no","hay","camino","se","hace","camino","al","andar","ese", "tesoro","es","mio"))==
       List(("mio",1,1),
            ("kingdom",2,0),
            ("hearts",1,0),
            ("heart",1,0),
            ("my",2,0),
            ("is",1,0),
            ("wisdom",1,0),
            ("Caminante",0,1),
            ("no",0,1),
            ("hay",0,1),
            ("camino",0,2),
            ("se",0,1),
            ("hace",0,1),
            ("al",0,1),
            ("andar",0,1),
            ("ese",0,1),
            ("tesoro",0,1),
            ("es",0,1)))