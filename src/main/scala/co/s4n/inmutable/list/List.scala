package co.s4n.inmutable.list.List

import scala.annotation.tailrec
sealed trait List[+A]
//it adds the methods of the case object, but you could add your own methods
case object Nil extends List[Nothing]
//metodos de acceso
case class Const[+A](head: A, tail: List[A]) extends List[A]
//instancia que me permite comparara y crear métodos
object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  def length[A](lst: List[A]): Int = lst match {
    case Nil => 0
    case Const(head, tail) => 1 + length(tail)
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Const(head, tail) => head + length(tail)
  }

  def product(productList: List[Double]): Double = productList match {
    case Nil => 1
    case Const(head, tail) => head * product(tail)
  }

  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(_, tail) => tail //remove first element
  }

  //search for option in the return type (manage exeptions in Scala])
  /*
  def head[A](lst: List[A]): Option [A] = lst match {
    case Nil => None
    case Const(head, tail) => Some (head) //return first element
  }*/
  def head[A](lst: List[A]): A = lst match {
    case Const(head, tail) => head //return first element
  }

  def and(lst: List[Boolean]): Boolean = {
    def andAux(lst: List[Boolean], acum: Boolean): Boolean = lst match {
      case Nil => acum
      case Const(h, t) => andAux(t, h && acum)
    }

    andAux(lst, true)
  }

  def or(lst: List[Boolean]): Boolean = lst match {
    case Nil => false
    case Const(false, tail) => or(tail)
    case _ => true
  }

  def max(lst: List[Int]): Int = {
    @tailrec
    def maxRec(lst: List[Int], max: Int): Int = lst match {
      case Nil => max
      case Const(head, tail) => if (head > max) maxRec(tail, head) else maxRec(tail, max)
    }

    maxRec(lst, 0)
  }

  def min(lst: List[Long]): Long = {
    @tailrec
    def minRec(lst: List[Long], min: Long): Long = lst match {
      case Nil => min
      case Const(head, tail) => if (head > min) minRec(tail, head) else minRec(tail, min)
    }

    minRec(lst, 0)
  }

  def minMax(lst: List[Double]): (Double, Double) = {
    @tailrec
    def minMaxP(lst: List[Double], acc: (Double, Double)): (Double, Double) = lst match {
      case Nil => acc
      case Const(h, t) => minMaxP(t, (if (h < acc._1) h else acc._1, if (h > acc._2) h else acc._2))
    }

    minMaxP(tail(lst), (head(lst), head(lst)))
  }

  //add an element at the begining of the list
  def const[A](head: A, tail: List[A]): List[A] = Const(head, tail)

  //create a copy and add an element to that copy
  def addEnd[A](lst: List[A], elem: A): List[A] = lst match {
    case Nil => Const(elem, Nil)
    case Const(head, tail) => Const(head, addEnd(tail, elem))
  }

  def append[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
    case (Nil, Nil) => Nil
    case (lst1, Nil) => lst1
    case (Nil, lst2) => lst2
    case (Const(head, tail), lst2) => Const(head, append(tail, lst2))
  }

  def drop[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (n, Nil) => Nil
    case (0, lst) => lst
    case (n, Const(head, tail)) => drop(n - 1, tail)
  }

  def take[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (n, Nil) => Nil
    case (1, Const(h, t)) => addEnd(Nil, h)
    case (n, Const(h, t)) => Const(h, take(n - 1, t))
  }

  def init[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(head, Nil) => Nil
    case Const(head, tail) => Const(head, init(tail))
  }

  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    def splitRec[A](c: Int, lst2: List[A], acum: List[A]): (List[A], List[A]) = (c, lst2) match {
      case (c, Nil) => (Nil, Nil)
      case (0, lst2) => (acum, lst2)
      case (c, Const(h, t)) => splitRec(c - 1, t, addEnd(acum, h))
    }

    splitRec(n, lst, Nil)
  }

  def zip[A, B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1, lst2) match {
    case (lst1, Nil) => Nil
    case (Nil, lst2) => Nil
    case (Const(head1, tail1), Const(head2, tail2)) => Const((head1, head2), zip(tail1, tail2))
  }

  def unzip[A, B](lst: List[(A, B)]): (List[A], List[B]) = {
    def unzipRec[A, B](lstO: List[(A, B)], lst1: List[A], lst2: List[B]): (List[A], List[B]) = lstO match {
      case Nil => (lst1, lst2)
      case Const(head, tail) => unzipRec(tail, addEnd(lst1, head._1), addEnd(lst2, head._2))
    }

    unzipRec(lst, Nil, Nil)
  }

  def reverse[A](l: List[A]): List[A] = {
    def reverseRec[A](acum: Int, lst: List[A], lstAux: List[A]): List[A] = (acum, lst) match {
      case (0, Nil) => lstAux
      case (acum, Const(head, tail)) => reverseRec(acum - 1, tail, Const(head, lstAux))
    }

    reverseRec(length(l), l, Nil)
  }

  def intersperse[A](elem: A, lst: List[A]): List[A] = {
    def intersperseRec[A](elem0: A, lst0: List[A], acum: List[A]): List[A] = lst0 match {
      case Nil => acum
      case Const(h, Nil) => addEnd(acum, h)
      case Const(h, t) => intersperseRec(elem0, t, addEnd(addEnd(acum, h), elem0))
    }

    intersperseRec(elem, lst, Nil)
  }

  def concat[A](lst: List[List[A]]): List[A] = {
    @tailrec
    def concatRec(lst: List[List[A]], acum: List[A]): List[A] = lst match {
      case Nil => acum
      case Const(head, tail) => concatRec(tail, append(acum, head))
    }

    concatRec(lst, Nil)
  }

  //funtion that has a function as parameter --> función de primer orden
  //currificación de parametros por () ()
  def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = lst match {
    case Const(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => lst
  }

  //only specify the type of the fuction not the "value" f:(Int) not f:(x:Int)
  def reduce[Int](lst: List[Int], z: Int)(f: (Int, Int) => Int): Int = lst match {
    case Nil => z
    case Const(head, tail) => f(head, reduce(tail, z)(f))
  }

  //z init value, it executes (fold) from right to left
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Const(head, tail) => f(head, foldRight(tail, z)(f))
  }

  //(x,y)=>x+y the whole expression could be write as _+_
  def sumR(lst: List[Int]) = reduce(lst, 0)((x, y) => x + y)

  def productR(lst: List[Int]) = reduce(lst, 1)((x, y) => x * y)

  //y acum val
  def lengthR[A](lst: List[A]): Int = foldRight(lst, 0)((x, y) => 1 + y)

  def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match {
    case Const(head, tail) => foldLeft(tail, f(z, head))(f)
    case Nil => z
  }

  def SumLeft(lst: List[Int]) = foldLeft(lst, 0)(_ + _)

  def profuctLeft(lst: List[Int]) = foldLeft(lst, 1)(_ * _)

  def sumOneFoldLeft(lst: List[Int]): List[Int] ={
    foldLeft(lst, Nil: List[Int])((lst, elem) => addEnd(lst, (1 + elem)))
  }

  def map[A, B](lst: List[A])(f: A => B): List[B] = foldRight(lst, Nil:List[B])((x, y) => Const(f(x), y))

  def andRight(lst: List[Boolean]): Boolean = foldRight(lst, true)((x, y) => x && y)

  def takeWhile[A](lst:List[A])(p:A => Boolean):List[A] = lst match {
    case Const(head,tail) if p(head) => takeWhile(tail)(p)
    case _=> lst
  }

  //todo def filter[A](lst:List[A])(p:A => Boolean): List[A] = foldRight(lst,)

  def unzipRight[A,B](lst:List[(A, B)]) : (List[A],List[B]) =
    foldRight(lst,(Nil:List[A],Nil:List[B]))((x,y) => (Const(x._1,y._1), Const(x._2,y._2)))

  def lengthLeft[A](lst:List[A]):Int = foldLeft(lst,0)((x,y) => 1+x)

  def andLeft(lst:List[Boolean]):Boolean = foldLeft(lst,true)((x,y) => x && y)

  def sumarUno(lst:List[Int]):List[Int] = lst match {
    case Nil => Nil
    case Const(head,tail) => Const(head + 1, sumarUno(tail))
  }

  def listIntToString(lst:List[Int]):List[String] = lst match {
    case Nil => Nil
    case Const(head,tail) => Const(head.toString,listIntToString(tail))
  }
 //mapeo gneralizado
 //It takes an A type and it transform it to B
  def mapGen [A, B](lst:List[Int])(f:Int => Int):List[Int] = lst match {
    case Nil => Nil
    case Const(head,tail) => Const (f(head),mapGen(tail)(f))
  }
  //apply map function
  //underscore (_) means every element of the list
  def sumOneMap (lst:List[Int]):List[Int] = mapGen(lst)(_+1) //(x) => x + 1

  // todo def lstIntToStringMap (lst:List[Int]): List[String] = mapGen(lst)(_.toString)



}