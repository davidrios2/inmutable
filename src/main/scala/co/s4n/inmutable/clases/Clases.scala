package co.s4n.inmutable.clases
//Objeto literales  son valores
//case object vs object --> case object generates more information, more methods, etc
//case object is use for symbolic data type, algebraic data type
//case object is a value, object is a reference for memory
//object is similar to static because it's created at the start of the program
//los valores pueden ser llamados como funciones object Io{ val color = "fawn"} Io.color
object Example {
  def method(a:Int, b:Int) ={
    a + b
  }

  def method2(a:Int, b:Int) ={
    a + b + c
  }
 //it only initialize at the start ONCE, it you call it again it's already assigned
  val c = 10

}

object comp {
  def square(num: Float) ={
    num * num
  }
  def cube(num: Float) ={
    num * square(num)
  }
}

object comp2 {

  def square(num: Long) ={
    num * num
  }
  def cube(num: Long) ={
    num * square(num)
  }
  //comp.cube(3)
}

object prueba {

  def x = {
    println("x")
    1
  }
  val y = {
    println("y")
    x + 2
  }
  def z = {
    println("z")
    x
    x + "c"
  }
 prueba.x + prueba.y + prueba.z//x + "c" warning string + int deprecated --> 41c
}

class Clases {}
