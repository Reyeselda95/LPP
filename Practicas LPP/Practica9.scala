/**
 * @author ALEJANDRO REYES ALBILLAR
 * grupo MIERCOLES 13:00-15:00
 *
 */

// Ejercicio 1
// a) Define en Scala la función recursiva pura (sin tail recursion) aplica3D que reciba una lista de coordenadas 3D en forma de tupla, 
// una función unaria y una coordenada (“x”, “y” ó “z”). 
// Deberá aplicar la función a los elementos correspondientes a esa coordenada y devolver las nuevas coordenadas. Ejemplo:
// def suma2(x: Int) = x + 2  
// assert(aplica3D(List((1,2,3), 
//                      (4,5,6), 
//                      (7,8,9), 
//                      (10,11,12)), suma2 _, “y”) ==    
//                 List((1,4,3), 
//                      (4,7,6), 
//                      (7,10,9), 
//                      (10,13,12)))

def aplica3D(lista: List[(Int, Int, Int)], func:(Int)=>Int, coor: String):List[(Int, Int, Int)] = {
    if(lista.tail.isEmpty)
        coor match{
            case "x"=> List((func(lista.head._1),lista.head._2, lista.head._3))
        
            case "y"=> List((lista.head._1, func(lista.head._2), lista.head._3))
        
            case "z"=> List((lista.head._1,lista.head._2, func(lista.head._3)))
    
        }
    else
        coor match{
            case "x"=> List((func(lista.head._1),lista.head._2, lista.head._3)):::aplica3D(lista.tail, func, coor)
            case "y"=> List((lista.head._1, func(lista.head._2), lista.head._3)):::aplica3D(lista.tail, func, coor)
            case "z"=> List((lista.head._1, lista.head._2, func(lista.head._3))):::aplica3D(lista.tail, func, coor)
        }
    
}
//
// Pruebas
//
def suma2(x: Int) = x + 2 
assert(aplica3D(List((1,2,3), (4,5,6), (7,8,9), (10,11,12)), suma2 _,"y") ==    List((1,4,3), (4,7,6), (7,10,9), (10,13,12)))
assert(aplica3D(List((1,2,3), (4,5,6), (7,8,9), (10,11,12)), suma2 _,"x") ==    List((3,2,3), (6,5,6), (9,8,9), (12,11,12)))
assert(aplica3D(List((1,2,3), (4,5,6), (7,8,9), (10,11,12)), suma2 _,"z") ==    List((1,2,5), (4,5,8), (7,8,11), (10,11,14)))
//

// b) Define la función anterior utilizando funciones de orden superior.
def aplica3Dmap(lista: List[(Int, Int, Int)], func:(Int)=>(Int), coor: String):List[(Int, Int, Int)] = {
    if(lista.isEmpty)
        Nil    
    else
        lista.map(
        (x)=>aplicatupla(x, func, coor)
        )
}

//
//Pruebas
//
def aplicatupla(x: (Int, Int, Int),func:(Int)=>Int, y: String):(Int, Int, Int) = {
    y match{
            case "x"=> (func(x._1),x._2, x._3)
        
            case "y"=> (x._1, func(x._2), x._3)
        
            case "z"=> (x._1,x._2,func(x._3))
    
    }
}

//
// Pruebas
//
assert(aplica3Dmap(List((1,2,3), (4,5,6), (7,8,9), (10,11,12)), suma2 _,"y") ==    List((1,4,3), (4,7,6), (7,10,9), (10,13,12)))
assert(aplica3Dmap(List((1,2,3), (4,5,6), (7,8,9), (10,11,12)), suma2 _,"x") ==    List((3,2,3), (6,5,6), (9,8,9), (12,11,12)))
assert(aplica3Dmap(List((1,2,3), (4,5,6), (7,8,9), (10,11,12)), suma2 _,"z") ==    List((1,2,5), (4,5,8), (7,8,11), (10,11,14)))
assert(aplica3Dmap(List(), suma2 _,"z") == List())
//

//Ejercicio 2
//a) Escribe en Scala utilizando funciones de orden superior la función intercambia(lista) que reciba una lista de tuplas de dos elementos de tipo entero 
// y devuelva una lista con las mismas tuplas pero con los elementos de cada tupla intercambiados.
//Ejemplo:
//assert(intercambia(List((1,2), (3,4), (5,6))) == List((2,1), (4,3), (6,5)))

def intercambia(lista:List[(Int, Int)]): List[(Int, Int)]= {
    if(lista.isEmpty)
        Nil
    else
    lista.map(
        (x)=>(x._2,x._1)
    )
}

//
// Pruebas
//
assert(intercambia(List((1,2), (3,4), (5,6))) == List((2,1), (4,3), (6,5)))
assert(intercambia(List()) == List())
assert(intercambia(List((1,1), (3,4), (5,5))) == List((1,1), (4,3), (5,5)))
//

//b) Escribe en Scala la función compruebaParejas(lista, func) que reciba una lista de enteros y una función que recibe un entero y devuelve un entero, y devuelva una lista de tuplas.
// La función debe devolver en forma de tupla aquellos números contiguos que cumplan que el número de la derecha es el resultado de aplicar la función al número de su izquierda.
// Ejemplo:
// assert(compruebaParejas(List(2, 4, 16, 5, 10, 100, 105), (x)=>{x*x}) ==        List((2,4), (4,16), (10,100)))

def compruebaParejas (lista:List[Int], func: (Int)=>Int) :List[(Int, Int)] ={
    if(lista.tail.isEmpty)
        Nil
    else
        if(lista.tail.head == func(lista.head))
            List((lista.head, lista.tail.head))::: compruebaParejas(lista.tail, func)
        else
            compruebaParejas(lista.tail, func)
}

//
// Pruebas
//
assert(compruebaParejas(List(2, 4, 16, 5, 10, 100, 105), (x)=>{x*x}) ==        List((2,4), (4,16), (10,100)))
assert(compruebaParejas(List(2, 4), (x)=>{x*x}) ==        List((2,4)))
assert(compruebaParejas(List(2), (x)=>{x*x}) ==        List())
assert(compruebaParejas(List(2, 4, 16, 5, 10, 100, 200), (x)=>{x+x}) ==        List((2,4), (5,10), (100,200)))
//

//Ejercicio 3
//a) Define utilizando un tipo genérico la función minimo que devuelve el valor mínimo de una lista.
//Un ejemplo de su funcionamiento:
//assert(minimo(List(100,-10,30,200), (x: Int, y: Int) => {x < y}) == -10) assert(minimo(List("hola", "adios", "mola"), (x: String, y: String) => {x < y})        == "adios")

def minimo[T](lista: List[T], func: (T,T)=>Boolean):T={
    if(lista.tail.isEmpty)
        lista.head
    else
        if(func(lista.head, lista.tail.head))
            minimo(List(lista.head):::lista.tail.tail, func)
        else
            minimo(lista.tail, func)
}

//
// Pruebas
//
assert(minimo(List(100,-10,30,200), (x: Int, y: Int) => {x < y}) == -10) 
assert(minimo(List("hola", "adios", "mola"), (x: String, y: String) => {x < y})        == "adios")
//

// b) Define utilizando tipos genéricos la función minimaTupla que recibe una lista de tuplas y una función toInt que convierte 
// una tupla en entero y que devuelve la tupla con la que toInt devuelve el entero más pequeño.
// Por ejemplo, podemos aplicar minimaTupla a una lista de tuplas (String, Int) pasando una función toInt que suma la longitud de la cadena al entero de la tupla:
// assert(minimaTupla(List(("hola",10),("pep",9),("supercalifragilistcoespialidoso",2)), (t: (String, Int)) => {t._1.length + t._2}) == ("pep",9))

def minimaTupla[S,T](lista: List[(S,T)], toInt:((S,T))=>(Int)) :(S,T) ={
    if(lista.tail.isEmpty)
        lista.head
    else
        if(toInt(lista.head)<toInt(lista.tail.head))
            minimaTupla(List(lista.head):::lista.tail.tail, toInt)
        else
            minimaTupla(lista.tail, toInt)
    
}

//
// Pruebas
//
assert(minimaTupla(List(("hola",10),("pep",9),("supercalifragilistcoespialidoso",2)), (t: (String, Int)) => {t._1.length + t._2}) == ("pep",9))
//

// Ejercicio 4
//Implementa utilizando funciones de orden superior la función masFrecuentes(s: String, n: Int): List[String]
//que recibe una frase con palabras separadas por espacios y devuelve una lista con las n cadenas que más veces aparecen en la frase,
//ordenadas de mayor número de apariciones a menor. En el caso en que las palabras aparezcan el mismo número de veces, aparecerán por orden alfabético. 
//Las palabras mayúsculas y minúsculas se considerarán iguales. 
//Puedes definir funciones auxiliares y cualquier método de la clase List
//Ejemplo:
//assert(masFrecuentes("En esta frase hay Mas de mas Frase en repetidas de mas De", 4)==  List("de", "mas", "en", "frase"))
//Pista 1: Puedes transformar la cadena en un cadena con todas las palabras en minúscula. 
//Pista 2: Para calcular las n palabras más frecuentes será necesario calcular una lista de tuplas (String, Int)
//que guarde la frecuencia de cada cadena.

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

//Devuelve una lista de parejas con los elementos de la lista y las veces en las que aparece en la lista
def cuentaveces(lista:List[String]):List[(String, Int)]={
    if(lista.isEmpty)
        Nil
    else
        List((lista.head, cuenta(lista.head, lista, Nil, 0)._1))::: cuentaveces(cuenta(lista.head, lista, Nil, 0)._2)
}

//Transforma un array de Strings en una lista
def toList(lista:Array[String]):List[String]={
    if(lista.isEmpty)
        Nil
    else
        List(lista.head):::toList(lista.tail)
}

//quita el último elemento de una lista de parejas (String, Int)
def quitaultimo(lista: List[(String, Int)]):List[(String, Int)]={
    if(lista.tail.isEmpty)
        Nil
    else
        lista.head::quitaultimo(lista.tail)
}

//Inserta en orden un elemento en una lista
def insertaorden(elem: (String, Int), lista: List[(String, Int)]):List[(String, Int)]={
    if(lista.isEmpty)
        List(elem)
    else
        if(elem._2>lista.head._2)
            elem::lista
        else
            lista.head::insertaorden(elem, lista.tail)
}

//Toma una lista de tuplas (String, Int) y las inserta en una solución de modo que dicha lista sea de un tamaño específico y esté ordenada de mayor a menor teniendo en cuenta el segundo elemento de la tupla
def ordenar(lista:List[(String, Int)], listasol:List[(String, Int)],tam: Int):List[(String,Int)]={
    if(lista.isEmpty && tam==0)
        listasol
    else
        if(listasol.isEmpty)
            ordenar(lista.tail, List(lista.head), tam-1)
        else
            if(tam>0)
                if(lista.head._2>=listasol.head._2)
                    ordenar(lista.tail, insertaorden(lista.head, listasol), tam-1)
                else
                    ordenar(lista.tail, insertaorden(lista.head, listasol), tam-1)
            else
                if(lista.head._2>=listasol.head._2)
                    ordenar(lista.tail, lista.head::quitaultimo(listasol), tam)
                else
                    ordenar(lista.tail, listasol, tam)
            
}

def masFrecuentes(s: String, n:Int):List[String]={
     val palabras=toList(s.toLowerCase().split(' '))
    if(s.isEmpty)
        Nil
    else
        ordenar(cuentaveces(palabras), Nil, n).map((x)=>x._1)

}

//
// Pruebas
//
assert(masFrecuentes("En esta frase hay Mas de mas Frase en repetidas de mas De", 4)==  List("de", "mas", "en", "frase"))
assert(masFrecuentes("En esta frase hay Mas de mas Frase en repetidas de mas De", 6 )==List( "mas","de", "en", "frase", "esta", "hay"))
//

// Ejercicio 5
// a) Implementa la función creaOperador que recibe un entero (valor inicial) y una función de dos enteros que devuelve otro
// entero (operacion) y devuelve una clausura de un argumento en la que se aplica operacion a valor inicial y el argumento.
// Ejemplo:
// assert(creaOperador(10, _ + _)(100) == 110) 
// assert(creaOperador(10, (x,y) => {y/x})(200) == 20)

def creaOperador(x: Int,func:(Int, Int)=>Int):(Int)=>Int={
    (y:Int)=> func(x,y)
}

//
// Pruebas
//
assert(creaOperador(10, _ + _)(100) == 110) 
assert(creaOperador(10, (x,y) => {y/x})(200) == 20)
//

//b) Implementa utilizando tipos genéricos la función creaIterador que recibe una frase (palabras separadas por espacios), 
// un caso base y una función (procesaPalabra) y devuelve una clausura con estado local (iterador). Cada invocación a iterador 
// aplica la función procesaPalabra a una palabra de la cadena, devuelve el resultado y modifica el estado local para que la cadena 
// avance y la próxima palabra sea la siguiente de la cadena. Cuando se ha procesado todo la cadena se devuelve el caso base.
// val it1 = creaIterador("En un lugar", '%', _.head) 
// assert(it1() == 'E') 
// assert(it1() == 'u')
// assert(it1() == 'l')
// assert(it1() == '%') 
// assert(it1() == '%')
// val it2 = creaIterador("En un lugar", -1, _.length)
// assert(it2() == 2) 
// assert(it2() == 2)
// assert(it2() == 5)
// assert(it2() == -1)
// assert(it2() == -1)

//Transforma un array de String a Lista de String
def toListString(lista:Array[String]):List[String]={
    if(lista.tail.isEmpty)
        List(lista.head)
    else
        List(lista.head):::toListString(lista.tail)
}

def creaIterador[T](frase:String, base:T, func:(String)=>T):()=>T ={
    var listapal=toListString(frase.split(' '))
    var aux=base
    ()=>{
        if(listapal.isEmpty)
            base
        else{
            aux=func(listapal.head)
            listapal=listapal.tail  
            aux
        }
    }
}

//
// Pruebas
//
val it1 = creaIterador("En un lugar", '%', _.head) 
assert(it1() == 'E') 
assert(it1() == 'u')
assert(it1() == 'l')
assert(it1() == '%') 
assert(it1() == '%')
val it2 = creaIterador("En un lugar", -1, _.length)
assert(it2() == 2) 
assert(it2() == 2)
assert(it2() == 5)
assert(it2() == -1)
assert(it2() == -1)
//