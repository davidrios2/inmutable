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

  def addNat (nat1:Nat, nat2:Nat): Nat = (nat1,nat2) match {
    case (Cero,Cero) => Cero
    case (Suc(nat1),Cero) => Suc(nat1)
    case (Cero,Suc(nat2)) => Suc(nat2)
    case (nat1,nat2) => fromIntToNatural(fromNatToInt(nat1)+fromNatToInt(nat2))
  }
  
  def prodNat (nat1:Nat, nat2:Nat): Nat = (nat1,nat2) match {
    case (Cero,Cero) => Cero
    case (Suc(nat1),Cero) => Cero
    case (Cero,Suc(nat2)) => Cero
    case (nat1,nat2) => fromIntToNatural(fromNatToInt(nat1)*fromNatToInt(nat2))
  }

}