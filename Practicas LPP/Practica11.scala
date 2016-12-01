/**
 * @author ALEJANDRO REYES ALBILLAR
 * grupo MIERCOLES 13:00-15:00
 *
 */

// Ejercicio 1
// Modifica el código del ejemplo SerpientesEscaleras (en el tema 7) para que se pueda jugar con n jugadores. 
// Modifica también los métodos que necesites del trait PresentadorJuegoTablero y/o de la clase PresentadorTexto 
// para que se muestre la evolución de la nueva versión del juego. El juego termina cuando un jugador llega a la meta.

trait PresentadorJuegoTablero {  
    def comienzoJuego(juego: JuegoTablero): Unit 
    def juego(juego: JuegoTablero, tiradaDado: Int, jugador:Int, casilla:Int): Unit   
    def finalJuego(jugador:Int): Unit 
}

class PresentadorTexto extends PresentadorJuegoTablero {  
    var numeroTurnos = 0 
    def comienzoJuego(juego: JuegoTablero) {   
        numeroTurnos = 0     
        println("Ha empezado el juego")       
        println("El juego está usando un dado de " + juego.dado.caras + " caras")   
    }
    def juego(juego: JuegoTablero, tiradaDado: Int, jugador: Int, casilla:Int) {   
        numeroTurnos += 1       
        println("Nueva tirada del juegador "+ jugador + ": "+ tiradaDado) 
        println("Casilla del jugador "+ jugador +": " + casilla)   
    }
    def finalJuego(jugador:Int) {      
        println("Final del juego: Ha ganado el jugador: "+jugador) 
    } 
}

class Dado(val caras: Int) {   
    private val rnd = new scala.util.Random  
    def tirada(): Int = rnd.nextInt(caras) + 1
}
abstract class JuegoTablero {  
    val dado: Dado
    //var casillaActual: Int   
    def jugar(): Unit 
}

class SerpientesEscaleras(unDado: Dado, gamers:Int) extends JuegoTablero {  
    val casillaFinal = 25   
    val dado = unDado
    val tablero: Array[Int] = Array.fill(casillaFinal+1)(0)  
    tablero(3) = 8; 
    tablero(6) = 11;
    tablero(9) = 9;
    tablero(10) = 2; 
    tablero(14) = -10; 
    tablero(19) = -11;
    tablero(22) = -2; 
    tablero(24) = -8 
    var presentador: PresentadorJuegoTablero = null    
    var jugadores: Array[Int]=Array.fill(gamers+1)(0)   //Es un array en el que el 0 es el jugador actual
    var jugadorActual= 1;
    def jugar() {
        //casillaActual = 0 
        var finalJuego = false 
        // llamamos al objeto presentador 
        // para informarle que comienza el juego   
        if (presentador != null) presentador.comienzoJuego(this)    
        do {  
            val tirada = dado.tirada()  
            val nuevaCasilla = tirada + jugadores(jugadorActual) 
            if (nuevaCasilla == casillaFinal) {   
                jugadores(jugadorActual) = nuevaCasilla     
                finalJuego = true     
            }    
            if (nuevaCasilla < casillaFinal) {   
                jugadores(jugadorActual) = nuevaCasilla   
                jugadores(jugadorActual) += tablero(jugadores(jugadorActual))     
            }   
            // llamamos al objeto presentador    
            // para informarle que se ha hecho una tirada   
            if (presentador != null)
                presentador.juego(this, tirada, jugadorActual, jugadores(jugadorActual))    
            if(jugadorActual==gamers && !finalJuego){
                jugadorActual=1;
            }
            else 
                if(!finalJuego){
                    jugadorActual+=1
                }
            
        } while (!finalJuego)   
        // llamamos al objeto presentador    
        // para informarle que el juego ha terminado    
        if (presentador != null) 
            presentador.finalJuego(jugadorActual) 
    }
}

//
// Pruebas
//
val dado = new Dado(6) 
val juegoSerpientes = new SerpientesEscaleras(dado,5) 
val presentador = new PresentadorTexto 
juegoSerpientes.presentador = presentador 
juegoSerpientes.jugar

val cube = new Dado(20) 
val newgame = new SerpientesEscaleras(cube,7) 
val gentle = new PresentadorTexto 
newgame.presentador = gentle 
newgame.jugar
//

//
// Ejercicio 2
// Implementa una versión genérica (con un tipo genérico T) de la clase Tree del ejercicio 3 de la práctica 10. 
// Modifica las definiciones de los métodos para que funcionen correctamente, añadiendo algún parámetro adicional si lo consideras oportuno.
// Añade la definición de un método filtra que en el que se pasa un predicado y se devuelve la lista de los nodos que lo cumplan.
// Define pruebas para todos los métodos.


class Tree[T](dat:T, lista:List[Tree[T]]){
    val dato:T =dat;
    val listaHijos:List[Tree[T]] =lista;
    
    def this(dato:T)={
        this(dato, Nil)
    }
    
    def this(arbol:Tree[T])={
        this(arbol.dato,arbol.listaHijos)
    }
    
    def this(dat:T, arbol:Tree[T])={
        this(dat, List(arbol))
    }
    
    //Método auxiliar para transformar en lista
    def aplana(lista:List[Tree[T]]):List[T]={
        if(lista.tail.isEmpty)
            lista.head.toList
        else
            lista.head.toList:::aplana(lista.tail)
    }
    
    def toList():List[T]={
        if(listaHijos.isEmpty)
            List(dato)
        else
            dato::aplana(listaHijos)
    }
    
    //Método auxiliar para calcular el tamaño de una lista
    def tamaño(lista:List[T]):Int={
        if(lista.isEmpty)
            0
        else
            1+tamaño(lista.tail)
    }
    
    def numNodos:Int={
        tamaño(this.toList)
    }
    
    
    //Método auxiliar que suma los elementos de una lista
  /* def sumaLista(lista:List[T]):T={
        if(lista.tail.isEmpty)
            lista.head
        else
           lista.head + sumaLista(lista.tail)
    }
    
    def sumTotal:T={
        sumaLista(this.toList)
    }*/
    
    def filtraAux(lista:List[Tree[T]],pred:Any):List[T]={
        if(lista.isEmpty)
            Nil
        else{
            if(pred(lista.head)){
                lista.head :: filtraAux(lista.Tail)
            }
            else{
                filtraAux(lista.tail)
            }
        }
    }
    
    def filtra(lista:List[Tree[T]],pred:Any):List[T]={
        filtraAux(lista.toList, pred)  
    }
    
}

val a1= new Tree(1)
val a2= new Tree(2)
val a3= new Tree(3, new Tree(8))
val a4= new Tree(4)
val a5= new Tree(5)
val a6= new Tree(6,List(a1,a2,a3,a4,a5))
val a7= new Tree(7,a6)


val a8= new Tree("a")
val a9= new Tree("b")
val a10= new Tree("c", new Tree("h"))
val a11= new Tree("d")
val a12= new Tree("e")
val a13= new Tree("f",List(a8,a9,a10,a11,a12))
val a14= new Tree("g",a13)
//
// Pruebas
//
assert(a7.toList() == List(7,6,1,2,3,8,4,5))
//assert(a7.sumTotal==36)
assert(a7.numNodos == 8)

assert(a14.toList() == List("g","f","a","b","c","h","d","e"))
//assert(a14.sumTotal==("gfabchde"))
assert(a14.numNodos == 8)
//
