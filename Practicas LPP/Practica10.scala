/**
 * @author ALEJANDRO REYES ALBILLAR
 * grupo MIERCOLES 13:00-15:00
 *
 */

// Ejercicio 1
//Implementa la clase Rational tal y como se explica en el capítulo 6 de Odersky (pag. 111).
//Prueba todos los métodos de la clase usando asserts.

class Rational(n:Int, d:Int){
    require (d!=0)
    private val g =gcd(n.abs, d.abs)
    
    val numer= n/g
    val denom = d/g
    
    def this(n:Int)=this(n,1)
    
    def + (that:Rational):Rational={
        new Rational(numer * that.denom + that.numer * denom,
                     denom * that.denom)
    }
    
    def + (i:Int):Rational= new Rational(numer + i * denom, denom)
    
    def - (that:Rational):Rational={
        new Rational(numer * that.denom - that.numer * denom, 
                     denom * that.denom)
    }
    
    def - (i:Int):Rational= new Rational(numer - i * denom, denom)
    
    def * (that:Rational):Rational={
        new Rational(numer * that.numer, denom * that.denom)
    }
    
    def * (i:Int):Rational= new Rational(numer * i, denom)
    
    def / (that:Rational):Rational= new Rational(numer * that.denom, denom* that.numer)
    
    def / (i:Int):Rational= new Rational(numer, denom * i)
    
    override def toString = numer +"/"+denom
    
    def Equals(r:Rational):Boolean={
        if(r.numer==numer && r.denom==denom)
            true
        else
            false
    }
    
    private def gcd(a:Int, b:Int):Int = if (b == 0) a else gcd(b, a % b)
}

//
// Pruebas
//

val n1:Rational = new Rational(5,4)
val n2:Rational = new Rational(7,4)
val i:Int = 10
val j:Int = 4
val n3:Rational = new Rational(45,4)
val n4:Rational = new Rational(-11,4)

assert((n1+i).toString==n3.toString)
assert((n1+i).Equals(n3)==true)
assert((n1+n2).Equals(new Rational(3,1))==true)
assert((n1-j).Equals(n4)==true)
assert((n2-n1).Equals(new Rational(1,2))==true)
assert((n1*j).Equals(new Rational(5,1))==true)
assert((n2*n1).Equals(new Rational(35,16))==true)
assert((n1/j).Equals(new Rational(5,16))==true)
assert((n2/n1).Equals(new Rational(7,5))==true)
//

//Ejercicio 2
//Define, implementa y prueba la clase Intervalo como objeto funcional, que represente un 
//intervalo en la recta real con su punto de inicio y su punto de fin (defínelos como Double).
//Prueba todos los métodos de la clase usando asserts con valores distintos de los que hemos usado como ejemplo.
//Define los siguientes constructores:
//
//(inicio: Double, fin: Double): constructor por defecto.
//
//Debe cumplirse (require) que inicio <= fin. 
//Importante: no uses la palabra final como nombre del atributo que guarda el punto de finalización, porque es una palabra reservada de Scala. 
//
//(posicion: Double): devuelve un intervalo puntual, el intervalo con los puntos de inicio y de 
//finalización igual al número que se pasa como parámetro. 
//
//(otro: Intervalo): constructor auxiliar que construye un intervalo a partir de otro.
//
//Define los siguientes operadores y métodos:
//
//toString: para mostrar un intervalo con el formato [3.0, 4.3].
//
// +(otro: Intervalo): Intervalo: devuelve un nuevo intervalo que engloba a los dos. 
//Ejemplo: [2.0, 3.0] + [5.3, 6.1] => [2.0, 6.1] 

//+(desp: Double): Intervalo: suma el desp a la posición inicial y final del intervalo.
//Ejemplo: [2.0, 3.0] + 1.5 => [3.5, 4.5] 

//amplia(tamaño: Double): Intervalo: amplía el intervalo una cantidad. Resta el tamaño a la posición inicial y lo suma a la posición final. 
//Ejemplo: [2.0, 3.0] amplia 0.5 => [1.5, 3.5] 

//reduce(tamaño: Dobule): Intervalo: reduce el intervalo una cantidad. Suma el tamaño a la posición inicial y lo resta a la posición final. //Ejemplo: [2.0, 3.0] reduce 0.3 => [2.3, 2.7]. 
//Si la reducción “hace desaparecer” el intervalo se debe devolver un intervalo puntual situado en el punto medio de la posición inicial y final. //Ejemplo: [2.0, 3.0] reduce 0.6 => [2.5, 2.5] 
//
//intersecta(otro: Intervalo): Boolean, engloba(otro: Intervalo): Boolean: comprueba si self intersecta o engloba el intervalo que se pasa como parámetro. 
//Ejemplo [2.5, 6.2] engloba [2.5, 3.0] => true, 
//[2.5, 6.2] engloba [1.0, 6.5] => false, 
//[2.5, 6.2] intersecta [1.0, 6.5] => true 
//
//interseccion(otro: Intervalo): Intervalo: devuelve la intersección de los intervalos o null si no intersectan. 
//Ejemplo: [2.5, 6.2] interseccion [1.0, 6.5] => [2.5, 6.2], 
// [2.5, 6.2] interseccion [4.0, 10.0] => [4.0. 6.2],
//[2.5, 6.2] interseccion [10.0, 11.0] => null.

class Intervalo(inicio:Double, fin:Double){
    require(inicio<=fin)
    
    def min(a:Double, b:Double):Double={
        if(a<b)
            a
        else
            b
    }
    
    def max(a:Double, b:Double):Double={
        if(a>b)
            a
        else
            b
    }
    
    val init=inicio
    val finali=fin
    
    def this(posicion:Double)={
        this(posicion, posicion)
    }
    
    def this(otro:Intervalo)={
        this(otro.init, otro.finali)
    }
    
    override def toString="["+init+", "+finali+"]"
    
    def +(otro:Intervalo):Intervalo={
        new Intervalo(min(init, otro.init),max(finali, otro.finali))
    }
    
    def +(desp:Double):Intervalo={
        new Intervalo(init+desp,finali+desp)
    }
    
    def amplia(tamaño:Double):Intervalo={
        new Intervalo(init-tamaño,finali+tamaño)
    }
    
    def reduce(tamaño:Double):Intervalo={
        if((finali-tamaño)<(init+tamaño))
            new Intervalo(init+((finali-init)/2),finali-((finali-init)/2))
        else
            new Intervalo(init+tamaño,finali-tamaño)
    }
    
    def engloba(otro:Intervalo):Boolean={
        if(init<=otro.init && finali>=otro.finali)
            true
        else
            false
    }
    
    def intersecta(otro:Intervalo):Boolean={ 
        if((otro.init<=finali && init<=otro.finali) || this.engloba(otro)==true)
            true
        else
            false
    }
         
    def interseccion(otro:Intervalo):Intervalo={
        if(this.intersecta(otro)){
            new Intervalo(max(init, otro.init),min(finali, otro.finali))
        }
        else
            null
    }
    
    def Equals(otro:Intervalo):Boolean={
        if(otro.init==init && otro.finali==finali)
            true
        else
            false
    }
    
}

//
// Pruebas
//
assert(new Intervalo(4.0).Equals(new Intervalo(4.0,4.0)))
assert(new Intervalo(3.0,4.3).toString=="[3.0, 4.3]")
assert((new Intervalo(2.0, 3.0) + new Intervalo(5.3, 6.1)).Equals(new Intervalo(2.0, 6.1)))
assert((new Intervalo(2.0, 3.0) + 1.5).Equals(new Intervalo(3.5, 4.5)))
assert((new Intervalo(2.0, 3.0) amplia 0.5).Equals(new Intervalo(1.5, 3.5)))
assert((new Intervalo(2.0, 3.0) reduce 0.3).Equals(new Intervalo(2.3, 2.7)))
assert((new Intervalo(2.0, 3.0) reduce 0.6).Equals(new Intervalo(2.5, 2.5)))
assert((new Intervalo(2.5, 6.2) engloba new Intervalo(2.5, 3.0)) == true)
assert((new Intervalo(2.5, 6.2) engloba new Intervalo(1.0, 6.5)) == false)
assert((new Intervalo(2.5, 6.2) intersecta new Intervalo(1.0, 6.5)) == true)
assert((new Intervalo(2.5, 6.2) interseccion new Intervalo(1.0, 6.5)).Equals(new Intervalo(2.5, 6.2)))
assert((new Intervalo(2.5, 6.2) interseccion new Intervalo(4.0, 10.0)).Equals(new Intervalo(4.0, 6.2)))
assert((new Intervalo(2.5, 6.2) interseccion new Intervalo(10.0, 11.0)) == null)
//

// Ejercicio 3
//Implementa la clase árbol Tree de tipo Int como un objeto funcional con los mismos atributos que vimos 
//en Scheme (dato y listaHijos) y con los siguientes métodos:
//toList(): List[Int] ⇒ devuelve una lista con los enteros del árbol en pre-orden (primero el dato de la raíz y después los hijos,
//en el orden definido por la lista) 
//numNodos: Int ⇒ devuelve el número de nodos del árbol 
//sumTotal: Int ⇒ devuelva la suma de todos sus nodos
//Prueba todos los métodos de la clase usando asserts.

class Tree(dat:Int, lista:List[Tree]){
    val dato:Int =dat;
    val listaHijos:List[Tree] =lista;
    
    def this(dato:Int)={
        this(dato, Nil)
    }
    
    def this(arbol:Tree)={
        this(arbol.dato,arbol.listaHijos)
    }
    
    def this(dat:Int, arbol:Tree)={
        this(dat, List(arbol))
    }
    
    //Método auxiliar para transformar en lista
    def aplana(lista:List[Tree]):List[Int]={
        if(lista.tail.isEmpty)
            lista.head.toList
        else
            lista.head.toList:::aplana(lista.tail)
    }
    
    def toList():List[Int]={
        if(listaHijos.isEmpty)
            List(dato)
        else
            dato::aplana(listaHijos)
    }
    
    //Método auxiliar para calcular el tamaño de una lista
    def tamaño(lista:List[Int]):Int={
        if(lista.isEmpty)
            0
        else
            1+tamaño(lista.tail)
    }
    
    def numNodos:Int={
        tamaño(this.toList)
    }
    
    //Método auxiliar que suma los elementos de una lista
    def sumaLista(lista:List[Int]):Int={
        if(lista.isEmpty)
            0
        else
            lista.head+sumaLista(lista.tail)
    }
    
    def sumTotal:Int={
        sumaLista(this.toList)
    }
    
}

val a1= new Tree(1)
val a2= new Tree(2)
val a3= new Tree(3, new Tree(8))
val a4= new Tree(4)
val a5= new Tree(5)
val a6= new Tree(6,List(a1,a2,a3,a4,a5))
val a7= new Tree(7,a6)

//
// Pruebas
//
assert(a7.toList() == List(7,6,1,2,3,8,4,5))
assert(a7.sumTotal==36)
assert(a7.numNodos == 8)
//
