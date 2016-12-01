//
// 1/06/2015
//


////////////////////////////////////////////////////
// Ejercicio 1
////////////////////////////////////////////////////

class Dado(val caras: Int) {
    private val rnd = new scala.util.Random
    def tirada(): Int = rnd.nextInt(caras) + 1
}

abstract class JuegoTablero {
    val dado: Dado
    def jugar(): Unit
    // Nuevos atributos: 
    //      nJugadores
    //      array de casillas actuales de jugadores
    //      jugador actual
    val nJugadores : Int
    var casillaActualJ: Array[Int]
    var jugadorActual: Int 
}

trait PresentadorJuegoTablero {
    def comienzoJuego(juego: JuegoTablero): Unit
    def juego(juego: JuegoTablero, tiradaDado: Int): Unit
    def finalJuego(juego: JuegoTablero): Unit
}

class PresentadorTexto extends PresentadorJuegoTablero {
    var numeroTurnos = 0
    def comienzoJuego(juego: JuegoTablero) {
        numeroTurnos = 0
        println("Ha empezado el juego")
        println("El juego está usando un dado de " + juego.dado.caras + " caras")
    }

    def juego(juego: JuegoTablero, tiradaDado: Int) {
        numeroTurnos += 1
        println("Nueva tirada del jugador " + juego.jugadorActual +  ": " + tiradaDado)
        println("Casilla del jugador " + juego.jugadorActual +  ": " + juego.casillaActualJ(juego.jugadorActual))
    }

    def finalJuego(juego: JuegoTablero) {
        println("Final del juego")
        println("El juego ha durado " + numeroTurnos + " turnos")
        println("El !!!GANADOR!!! ha sido el jugador: " + juego.jugadorActual)
    }
}

class SerpientesEscaleras(val dado: Dado, val nJugadores:Int) extends JuegoTablero {
    val casillaFinal = 25
    val tablero: Array[Int] = Array.fill(casillaFinal+1)(0)
    tablero(3) = 8; tablero(6) = 11; tablero(9) = 9; tablero(10) = 2;
    tablero(14) = -10; tablero(19) = -11; tablero(22) = -2; tablero(24) = -8
    var presentador: PresentadorJuegoTablero = null
 
    var casillaActualJ = Array.fill(nJugadores+1)(0)
    var jugadorActual = 1

    def jugar() {
        var finalJuego = false
        // llamamos al objeto presentador
        // para informarle que comienza el juego
        if (presentador != null) presentador.comienzoJuego(this)
        do {
            val tirada = dado.tirada()
            val nuevaCasilla = tirada + casillaActualJ(jugadorActual)
            if (nuevaCasilla == casillaFinal) {
                casillaActualJ(jugadorActual) = nuevaCasilla
                finalJuego = true
            }
            if (nuevaCasilla < casillaFinal) {
                casillaActualJ(jugadorActual) = nuevaCasilla
                casillaActualJ(jugadorActual) += tablero(casillaActualJ(jugadorActual))
            }
            // llamamos al objeto presentador 
            // para informarle que se ha hecho una tirada
            if (presentador != null) presentador.juego(this, tirada)

            // pasamos el turno al siguiente jugador
            if (!finalJuego) {
                if (jugadorActual == nJugadores) jugadorActual = 1
                else jugadorActual +=1
            }

        } while (!finalJuego)
        // llamamos al objeto presentador
        // para informarle que el juego ha terminado
        if (presentador != null) presentador.finalJuego(this)
    }
}

val dado = new Dado(6)
val juegoSerpientes = new SerpientesEscaleras(dado, 3)
val presentador = new PresentadorTexto
juegoSerpientes.presentador = presentador
juegoSerpientes.jugar


////////////////////////////////////////////////////
// Ejercicio 2 
////////////////////////////////////////////////////

// Solución con funciones de orden superior y expresiones lambda.
// Todas las expresiones lambda están formuladas de forma expandida, sin abreviar.

class Tree[T](val dato: T, val hijos: List[Tree[T]]) {
   def this(dato: T) = this(dato, List())

   def toList(): List[T] = this.dato :: this.hijos.map((x:Tree[T]) => x.toList).fold(Nil)((x: List[T], y: List[T]) => x ::: y) // fold(Nil)(_ ::: _)

   def sumTotal(suma:(T,T)=>T, elemN:T): T = suma(this.dato,
                                                    this.hijos.map((x:Tree[T]) => x.sumTotal(suma, elemN)).fold(elemN)(suma))

   def numNodos(): Int = 1 + this.hijos.map((x:Tree[T]) => x.numNodos).fold(0)((x: Int, y: Int) => x + y) // fold(0)(_ + _)

   def filtra(pred:(T)=>Boolean): List[T] = cumplePred(pred, this.dato) :::
                                            this.hijos.map((x:Tree[T]) => x.filtra(pred)).fold(Nil)((x: List[T], y: List[T]) => x ::: y)

   private def cumplePred(pred:(T)=>Boolean, elem:T): List[T] = {
     if (pred(elem)) List(elem)
     else Nil
   }  
}



// Pruebas
//       10
//   2   7    4
//      5  6

val t2 = new Tree(2, Nil)
val t3 = new Tree(5, Nil)
val t4 = new Tree(6, Nil)
val t5 = new Tree(7, List(t3,t4))
val t6 = new Tree(4, Nil)
val t1 = new Tree(10, List(t2,t5,t6))

assert(t1.toList == List(10, 2, 7, 5, 6, 4))
assert(t1.numNodos == 6)
assert(t1.sumTotal(_+_, 0) == 34)

def impar(x:Int) : Boolean = x%2 == 1
assert(t1.filtra(impar) == List(7, 5))


//       a
//   b       c    
//   d     e  f  g
//            h
  
val tc2 = new Tree('d', Nil)
val tc3 = new Tree('e', Nil)
val tc4 = new Tree('h', Nil)
val tc1 = new Tree('g', Nil)
val tc5 = new Tree('f', List(tc4))
val tc6 = new Tree('c', List(tc3, tc5, tc1))
val tc7 = new Tree('b', List(tc2))
val tc0 = new Tree('a', List(tc7,tc6))

assert(tc0.toList == List('a', 'b', 'd', 'c', 'e', 'f', 'h', 'g'))
assert(tc0.numNodos == 8)
assert(tc0.filtra(Character.isUpperCase) == Nil)


//       "hola"
//   "que"      "tal"     
//   "mundo"

val ts1 = new Tree("mundo", Nil)
val ts2 = new Tree("tal", Nil)
val ts3 = new Tree("que", List(ts1))
val ts0 = new Tree("hola", List(ts3, ts2))

assert(ts0.toList == List("hola","que","mundo","tal"))
assert(ts0.numNodos == 4)
assert(ts0.sumTotal(_+_, "") == "holaquemundotal")

def longitud3(s:String) : Boolean = s.length == 3
assert(ts0.filtra(longitud3) == List("que","tal"))


////////////////////////////////////////////////////
// Ejercicio 3
////////////////////////////////////////////////////

def pi = 3.1416
def iguales(x: Double, y: Double): Boolean = (x-y).abs < 0.0000000000001

//
// Clase Intervalo (de la práctica pasada)
//

class Intervalo (val inicio:Double, val fin:Double) {
  require(inicio <= fin)

  def this(n: Double) = this(n, n) //constructor auxiliar

  def this(otro: Intervalo) = this(otro.inicio, otro.fin)  // constructor auxiliar

  override def toString = "[" + inicio + "," + fin + "]"
  
  def +(otro: Intervalo) : Intervalo = {
    new Intervalo(Math.min(inicio,otro.inicio), Math.max(fin,otro.fin))
  }
 
  def +(desp: Double) : Intervalo = {
    new Intervalo(inicio+desp, fin+desp)
  }

  def amplia(tamaño: Double) : Intervalo = {
    new Intervalo(inicio-tamaño, fin+tamaño)
  }

  def reduce(tamaño: Double) : Intervalo = {
    var resInicio = inicio+tamaño
    var resFin = fin-tamaño
    
    if (resInicio > resFin) {
      resInicio = (inicio+fin)/2
      resFin = resInicio
    }
    new Intervalo(resInicio, resFin)
  }

  def intersecta(otro: Intervalo) : Boolean = {
    (otro.inicio <= fin) && (inicio <= otro.fin)
  }

  def engloba(otro: Intervalo) : Boolean = {
    (inicio <= otro.inicio) && (fin >= otro.fin)
  }

  def interseccion(otro: Intervalo) : Intervalo = {
    if (otro == null) null
    else if (! this.intersecta(otro)) null
    else new Intervalo(math.max(inicio, otro.inicio), math.min(fin, otro.fin))
  }    

}

//
// Clase Punto2D
//

class Punto2D (val x:Double, val y:Double) {  
   def distancia2D(p: Punto2D) : Double = {
      Math.sqrt(Math.pow(this.x - p.x, 2) + Math.pow(this.y - p.y, 2))
   }

   override def equals(other: Any): Boolean =
      other match {
        case that: Punto2D => iguales(this.x, that.x) && iguales(this.y, that.y) 
        case _ => false
      }
}

//
// Clase Figura
// 

abstract class Figura {  
  def area() : Double
  def intersectaMismoTipo(otra: Figura) : Boolean
}

//
// Clase Rectangulo
// Versión en la que usamos la clase Intervalo definida anteriormente
// 

class Rectangulo (val esquinaInfIzquierda: Punto2D, val esquinaSupDerecha: Punto2D) extends Figura {
   private val alto: Double = esquinaSupDerecha.y - esquinaInfIzquierda.y
   private val ancho: Double = esquinaSupDerecha.x - esquinaInfIzquierda.x

   def area() = alto * ancho

   def intervaloX() = new Intervalo(esquinaInfIzquierda.x, esquinaInfIzquierda.x+ancho)
   def intervaloY() = new Intervalo(esquinaInfIzquierda.y, esquinaInfIzquierda.y+alto)
   
   def intersectaMismoTipo(otro: Figura) : Boolean = {
      otro match {
        case r: Rectangulo => this.intervaloX.intersecta(r.intervaloX) && this.intervaloY.intersecta(r.intervaloY)        
        case _ => false
      }
   }

   def interseccion(otro: Rectangulo): Option[Rectangulo] = {
      if (this.intersectaMismoTipo(otro)) {
        val interseccionX = this.intervaloX.interseccion(r.intervaloX)
        val interseccionY = this.intervaloY.interseccion(r.intervaloY)
        val esquinaInfIzquierda = new Punto2D(interseccionX.inicio, interseccionY.inicio)
        val esquinaSupDerecha = new Punto2D(interseccionX.fin, interseccionY.fin)
        Some(new Rectangulo(esquinaInfIzquierda, esquinaSupDerecha))         
      }
      else None
   } 

   override def equals(other: Any): Boolean =
      other match {
        case that: Rectangulo => this.esquinaInfIzquierda.equals(that.esquinaInfIzquierda) &&
                                 this.esquinaSupDerecha.equals(that.esquinaSupDerecha)
        case _ => false
      }

}

class Circulo (val centro: Punto2D, val radio: Double) extends Figura {
  def area() : Double = {
      pi * radio * radio
  }

  def intersectaMismoTipo(otro: Figura) : Boolean = {
    otro match {
      case c: Circulo =>  this.centro.distancia2D(c.centro) < this.radio + c.radio 
      case _ => false
    }    
  }
}

def sumaAreas(figuras: List[Figura]) : Double = figuras.map((x: Figura) => x.area).fold[Double](0)((x:Double,y:Double)=>x+y)
def cuentaTipos(figuras: List[Figura]) : (Int, Int) = figuras.map((x: Figura) => (x match {case _ : Circulo => (1,0)
                                                                                    case _ : Rectangulo => (0,1)
                                                                                    case _ => (0,0)}))
                                                      .fold((0,0))((x:(Int,Int),y:(Int,Int)) => (x._1+y._1, x._2+y._2))

def intersectanMismoTipo(figuras:List[Figura], otra: Figura) : List[Figura] = figuras.filter((x: Figura) => x.intersectaMismoTipo(otra))

/* 

Versiones recursivas:

def sumaAreas(figuras:List[Figura]): Double = {
    if (figuras.isEmpty) 0.0
    else figuras.head.area + sumaAreas(figuras.tail)
}

def cuentaTipos(figuras:List[Figura]): (Int, Int) = {
  if (figuras.isEmpty) (0,0)
  else {
      val contador = cuentaTipos(figuras.tail)
      figuras.head match {
        case _ : Circulo => (1+contador._1, contador._2)
        case _ : Rectangulo => (contador._1, 1+contador._2)
        case _ => (0,0)
      }
  }
}

def intersectanMismoTipo(figuras:List[Figura], otra: Figura): List[Figura] = {
  if (figuras.isEmpty) Nil
  else {
      val lista = intersectanMismoTipo(figuras.tail, otra)
      if (figuras.head.intersectaMismoTipo(otra)) figuras.head :: lista
      else lista
      }
}

def intersectanMismoTipo(figuras:List[Figura], otra: Figura): List[Figura] = {
  if (figuras.isEmpty) Nil
  else {
      val lista = intersectanMismoTipo(figuras.tail, otra)
      if (figuras.head.intersectaMismoTipo(otra)) figuras.head :: lista
      else lista
      }
}
*/


// Pruebas
// a)
val p1 = new Punto2D(3.0, 5.0)
val p2 = new Punto2D(-1.0, -1.0)
val c0 = new Circulo(p1, 2.0)
val r = new Rectangulo(p2, p1)

assert(p1.equals(new Punto2D(3.0, 5.0)))
assert(!p1.equals(null))
assert(iguales(r.area, 24.0))
assert(iguales(c0.area, 12.5664))
val figuras = List(c0,r)
assert(iguales(sumaAreas(figuras),36.5664))

// b)
val c1 = new Circulo(p2, 2.0)
assert(! c1.intersectaMismoTipo(c0))
val c2 = new Circulo(p2, 8.0)
assert(c2.intersectaMismoTipo(c0))

val p3 = new Punto2D(0.0,0.0)
val p4 = new Punto2D(4.0,6.0)
val r1 = new Rectangulo(p3,p4)
assert(r1.intersectaMismoTipo(r))

val p5 = new Punto2D(7.0, 10.0)
val r2 = new Rectangulo(p4,p5)
assert(! r2.intersectaMismoTipo(r))

// c)
val figuras1 = List(c0,r,c1,c2,r)
assert(cuentaTipos(figuras1) == (3,2))

assert(intersectanMismoTipo(figuras1, r2) == Nil)

val figuras2 = List(c0,r,c1,c2,r1)
// hay que comprobar la lista de objetos
//assert(intersectanMismoTipo(figuras1, r2).contains == List(r1))

// d)
assert(r1.interseccion(r).get.equals(new Rectangulo(p3,p1)))
assert(! r2.interseccion(r).isDefined)



////////////////////////////////////////////////////
// Ejercicio 4
////////////////////////////////////////////////////

class Clase1 {
   def g(x:Int, y:Int) = x+y
}

trait Trait1 {
   def h(x:Int, y:Int) = x*y
}

class Clase2 extends Clase1 with Trait1 {
   override def g(x:Int, y:Int) = x+y+100
   def f(x:Int) = x+30
}

class Clase3 {
   def g(x:Int, y:Int) = x+y+200
}

trait Trait2 extends Clase1 {
   abstract override def g(x:Int, y:Int) = super.g(x,y+10)
}

// a)

val a = new Clase2   // Correcta

//val b = new Clase3 with Trait2  // Incorrecta, ya que Clase3 no es subclase de Clase1.
                                // El Trait2 debe mezclarse con una clase de tipo Clase1,
                                // por tanto no es posible mezclar Clase3 con Trait2
val c = new Clase2 with Trait2  // Correcta
val d: Clase1 = new Clase2      // Correcta


// b)
println(a.h(2,4))  // ... 8     se aplica h definida en Trait1, ya que Clase2 se mezcla con Trait1
println(a.g(2,4))  // ... 106   se aplica g sobrescrita en Clase2 
//b.g(2,4)  // Error
//b.h(2,4)  // Error
println(c.g(2,4)) // ... 116   se aplica g definida en Trait2 y después 
                              // se invoca a g de Clase2, ya que super.g
                              // invoca al método g definido en la clase 
                              // que se mezcla con el trait
println(c.h(2,4))  // ... 8   // se aplica h definida en Trait1, ya que Clase 2 se mezcla con Trait1
//d.f(2)  // Error: f no es un método de Clase1, sino de su clase hija Clase2



////////////////////////////////////////////////////
// Ejercicio 5
////////////////////////////////////////////////////

import scala.actors.Actor

class Ping(count: Int, pong: Actor) extends Actor {
   def act() {
      for (i<-1 to count) {
         println("Ping envía Ping")
         pong ! "Ping"
      }
      pong ! "Stop"
   }
}

class Pong extends Actor {
   def act() {
      receive {
         case "Ping" => println ("Recibido mensaje")
         case "Stop" => println ("Pong se para")
                        exit()
      }
   }
}

val pong = new Pong
val ping = new Ping(10,pong)
ping.start
pong.start

// el actor Ping envía 10 mensajes "Ping" al actor Pong, mostrándose
// por pantalla 10 veces "Ping envía Ping", y después envía un
// mensaje "Stop". El actor Pong sólo recibe un mensaje "Ping" e imprime
// por pantalla "Recibido mensaje"
// El resto de mensajes enviados por Ping quedan en el buzón de Pong ya que
// éste no realiza más receive



