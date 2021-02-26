package co.s4n.inmutable.list

import scala.annotation.tailrec

sealed trait List [+A]
case object Nil extends List[Nothing]
case class Const[+A](head: A, tail: List[A]) extends List[A]

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

  def head[A](lst: List[A]): Any = lst match {
    case Nil => None
    case Const(head, tail) => head //return first element
  }

  def and(lst: List[Boolean]): Boolean = lst match {
    case Nil => true
    case Const(false, _) => false
    case Const(true, tail) => and(tail)
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

  //todo fix minMax
  def minMax(lst: List[Double]): (Double, Double) = {
    def minMaxRec(lst: List[Double], mm: (Double, Double)): (Double, Double) = lst match {
      case Nil => mm
      case Const(head, tail) => minMaxRec(tail, (if (head < mm._1) head else mm._1, if (head > mm._2) head else mm._2))
    }
    minMaxRec(tail, (head, head))
  }
  //add an element at the begining of the list
  def const[A](head:A, tail:List[A]): List[A] = Const(head, tail)
  //create a copy and add an element to that copy
  def addEnd [A] (lst:List[A], elem:A):List[A] = lst match {
    case Nil => Const (elem,Nil)
    case Const(head,tail) => Const(head, addEnd(tail,elem))
  }
   def append[A] (lst1:List[A], lst2:List[A]): List[A] = (lst1, lst2) match{
     case (Nil, Nil) => Nil
     case (lst1, Nil) => lst1
     case (Nil, lst2) => lst2
     case (Const(head,tail),lst2) => Const(head, append(tail, lst2))
   }

  def drop[A] (n:Int, lst:List[A]): List [A] = (n,lst) match {
    case (n,Nil) => Nil
    case (0,lst) => lst
    case (n,Const(head, tail)) => drop(n-1, tail)
  }

  /*def split[A](n:Int, lst:List[A]): (List[A],List[A]) = {
    def splitAux[A] (n:Int,lst:List[A],acum:(List[A])):(List[A], List[A])  = (n,lst) match {
      case (0, lst) => (lst, Nil)
      case (n, Nil) => (Nil, Nil)
      case (n, Const(head, tail)) => split(n - 1, lst,Nil)
    }
  }*/

}
