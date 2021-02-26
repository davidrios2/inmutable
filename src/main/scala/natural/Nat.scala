package co.s4n.inmutable.natural

sealed trait Nat
case object Cero extends Nat
case class Suc(nat:Nat) extends Nat

object Nat {

  def fromNatToInt(nat: Nat): Int = nat match {
    case Cero => 0
    case Suc(nat) => 1 + fromNatToInt(nat)
  }

  def fromIntToNatural(integer: Int): Nat = integer match {
    case 0 => Cero
    case n => Suc(fromIntToNatural(n - 1))
  }

}