package net.alasc.perms.sized

import spire.algebra.{Eq, Group}
import net.alasc.algebra.PermutationAction
import net.alasc.perms.Perm
import net.alasc.perms.internal.Prm
import net.alasc.util.NNOption

final class Perm16Algebra extends Group[Perm16] with Eq[Perm16] with PermutationAction[Perm16] {
  def inverse(a: Perm16) = Perm16Encoding.inverse(a).asInstanceOf[Perm16]
  def eqv(x: Perm16, y: Perm16) = x == y
  def isFaithful = true
  def findMovedPoint(g: Perm16) = smallestMovedPoint(g)
  override def isEmpty(a: Perm16)(implicit ev: Eq[Perm16]) = (a == Perm16Encoding.idEncoding)
  override def movesAnyPoint(g: Perm16) = (g != Perm16Encoding.idEncoding)
  override def nMovedPoints(g: Perm16) =  Perm16Encoding.nMovedPoints(g)
  override def movedPoints(g: Perm16) = Perm16Encoding.movedPoints(g)
  override def largestMovedPoint(g: Perm16) = NNOption(Perm16Encoding.largestMovedPoint(g))
  override def smallestMovedPoint(g: Perm16) = NNOption(Perm16Encoding.smallestMovedPoint(g))
  override def toPerm(g: Perm16) = new Perm(Perm16Encoding.toPrm(g))
  def movedPointsUpperBound(g: Perm16) = NNOption(15)
  def actl(g: Perm16, p: Int) = Perm16Encoding.invImage(g, p)
  def combine(x: Perm16, y: Perm16) = Perm16Encoding.op(x, y).asInstanceOf[Perm16]
  def empty = Perm16.id
  def actr(p: Int, g: Perm16) = Perm16Encoding.image(g, p)
}

object Perm16 {

  val id: Perm16 = Perm16Encoding.idEncoding.asInstanceOf[Perm16]

  def fromPerm(perm: Perm): Perm16 = Perm16Encoding.fromPrm(perm.p).asInstanceOf[Perm16]

  def fromPrm(prm: Prm): Perm16 = Perm16Encoding.fromPrm(prm).asInstanceOf[Perm16]

  def apply(cycle: Int*): Perm16 = fromPerm(Perm(cycle: _*))

}
