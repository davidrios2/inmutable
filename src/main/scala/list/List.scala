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

  def minMax(lst: List[Double]): (Double, Double) = {
    def minMaxp(lst: List[Double], mm: (Double, Double)): (Double, Double) = lst match {
      case Nil => mm
      case Const(head, tail) => minMaxp(tail, (if (head < mm._1) head else mm._1, if (head < mm._2) head else mm._2))
    }
    minMaxp(tail(lst), (head(lst), head(lst)))
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

  def take[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
    case (n, Nil) => Nil
    case (1, Const(head,tail)) => addEnd(head,Nil)
    case (n, Const(head,tail)) => Const(head,take(n-1,tail))
  }

  def init[A] (lst:List[A]): List[A] = lst match {
    case Nil => Nil
    case Const(head,Nil) => Nil
    case Const(head, tail) => Const(head, init(tail))
  }

  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    def splitRec[A](c: Int, lst2: List[A], acum: List[A]): (List[A], List[A]) = (c, lst2) match {
      case (c, Nil) => (Nil, Nil)
      case (0, Const(h, t)) => (acum, lst2)
      case (c, Const(h, t)) => splitRec(c - 1, t, addEnd(h,acum))
    }
    splitRec(n, lst, Nil)
  }

  def zip[A,B](lst1:List[A],lst2:List[B]):List[(A,B)] = (lst1,lst2) match {
    case (lst1,Nil) => Nil
    case (Nil,lst2) => Nil
    case (Const(head1,tail1),Const(head2,tail2)) => Const((head1,head2),zip(tail1,tail2))
  }

  def unzip[A,B](lst: List[(A,B)]):(List[A],List[B]) = {
    def unzipRec[A,B](lstO: List[(A,B)],lst1: List[A],lst2: List[B]): (List[A],List[B]) = lstO match {
      case Nil => (lst1,lst2)
      case Const(head,tail) => unzipRec(tail,addEnd(head._1,lst1),addEnd(head._2,lst2))
    }
    unzipRec(lst,Nil,Nil)
  }

  def reverse[A](l: List[A]): List[A] = {
    def reverseRec[A](acum: Int, lst:List[A],lstAux:List[A]): List[A] = (acum,lst) match {
      case (0,Nil) => lstAux
      case (acum,Const(head,tail)) => reverseRec(acum - 1,tail, Const(head,lstAux))
    }
    reverseRec(length(l),l,Nil)
  }

  def intersperse[A](elem: A,lst: List[A]): List[A] = {
    def intersperseRec[A](elem0: A,lst0: List[A],acum: List[A]):List[A] = lst0 match {
      case Nil => acum
      case Const(h,Nil) => addEnd(h,acum)
      case Const(h,t) => intersperseRec(elem0,t,addEnd(elem0,addEnd(h,acum)))
    }
    intersperseRec(elem,lst,Nil)
  }

  //todo add concat

}
