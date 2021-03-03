package co.s4n.inmutable.tree

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left: Tree[A],right: Tree[A]) extends Tree[A]

object Tree extends App {

  def size[A](tree: Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + size(left) + size(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + (if (depth(left) > depth(right)) depth(left) else depth(right))
  }

}

