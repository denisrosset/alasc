package net.alasc.perms.sized

import spire.algebra.{Eq, Group}
import spire.syntax.cfor.cforRange
import net.alasc.algebra.PermutationAction
import net.alasc.perms.{Cycles, Perm}
import net.alasc.perms.internal.Prm
import net.alasc.util.NNOption

final class Perm32Algebra extends Group[Perm32] with Eq[Perm32] with PermutationAction[Perm32] {
  def inverse(a: Perm32) = a.inverse
  def eqv(x: Perm32, y: Perm32) = x == y
  def isFaithful = true
  override def movesAnyPoint(g: Perm32) = !g.isId
  override def nMovedPoints(g: Perm32) = g.nMovedPoints
  override def movedPoints(g: Perm32) = g.movedPoints
  override def largestMovedPoint(g: Perm32) = g.largestMovedPoint
  override def smallestMovedPoint(g: Perm32) = g.smallestMovedPoint
  override def toPerm(g: Perm32) = g.toPerm
  def findMovedPoint(g: Perm32) = g.smallestMovedPoint
  def movedPointsUpperBound(g: Perm32) = NNOption(31)
  def actr(p: Int, g: Perm32) = g.image(p)
  def actl(g: Perm32, p: Int) = g.invImage(p)
  def empty = Perm32.id
  def combine(x: Perm32, y: Perm32) = Perm32Encoding.combine(x, y)
  override def isEmpty(a: Perm32)(implicit ev: Eq[Perm32]) = a.isId
}

final class Perm32(var long2: Long = 0L, var long1: Long = 0L, var long0: Long = 0L) { lhs =>
  override def toString = if (isId) "Perm32.id" else "Perm32" + Cycles.fromPerm(toPerm).string
  override def equals(that: Any) = that match {
    case rhs: Perm32 => lhs.long2 == rhs.long2 && lhs.long1 == rhs.long1 && lhs.long0 == rhs.long0
    case _ => false
  }
  override def hashCode: Int = Perm32Encoding.hash(long2, long1, long0)
  def |+|(rhs: Perm32): Perm32 = Perm32Encoding.combine(lhs, rhs)
  def isId = long2 == 0L && long1 == 0L && long0 == 0L
  def image(preimage: Int) = Perm32Encoding.image(long2, long1, long0, preimage)
  def invImage(i: Int) = Perm32Encoding.invImage(long2, long1, long0, i)
  def inverse: Perm32 = Perm32Encoding.inverse(this)
  def nMovedPoints = Perm32Encoding.nMovedPoints(long2, long1, long0)
  def movedPoints = Perm32Encoding.movedPoints(long2, long1, long0)
  def largestMovedPoint = NNOption(Perm32Encoding.largestMovedPoint(long2, long1, long0))
  def smallestMovedPoint = NNOption(Perm32Encoding.smallestMovedPoint(long2, long1, long0))
  def isValidPerm16: Boolean = Perm32Encoding.isValidPerm16(long2, long1, long0)
  def toPerm16: Perm16 = Perm32Encoding.toPerm16(long2, long1, long0)
  def toPrm: Prm = {
    val n = Perm32Encoding.largestMovedPoint(long2, long1, long0) + 1
    val images = new Array[Int](n)
    cforRange(0 until n) { i => images(i) = image(i) }
    images.asInstanceOf[Prm]
  }
  def toPerm: Perm = new Perm(toPrm)
  def apply(cycle: Int*): Perm32 = lhs |+| Perm32(cycle: _*)
}

object Perm32 {

  val id = new Perm32(0L, 0L, 0L)

  implicit val algebra: Perm32Algebra = new Perm32Algebra

  def fromPerm(perm: Perm): Perm32 = Perm32Encoding.fromPrm(perm.p)

  def fromPrm(prm: Prm): Perm32 = Perm32Encoding.fromPrm(prm)

  def apply(cycle: Int*): Perm32 = fromPerm(Perm(cycle: _*))

}
