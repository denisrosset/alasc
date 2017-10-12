package net.alasc.perms.sized

import net.alasc.perms.Perm
import net.alasc.perms.internal.Prm
import net.alasc.util.NNOption

/** Operations on permutations of domains of size 16. */
final class Perm16Ops(val encoding: Long) extends AnyVal {
  def invImage(i: Int): Int = Perm16Encoding.invImage(encoding, i)
  def image(i: Int): Int = Perm16Encoding.image(encoding, i)
  def isId: Boolean = encoding == 0L
  def movedPoints: Set[Int] = Perm16Encoding.movedPoints(encoding)
  def nMovedPoints: Int = Perm16Encoding.nMovedPoints(encoding)
  def smallestMovedPoint: NNOption = NNOption(Perm16Encoding.smallestMovedPoint(encoding))
  def largestMovedPoint: NNOption = NNOption(Perm16Encoding.largestMovedPoint(encoding))
  def inverse: Perm16 = Perm16Encoding.inverse(encoding).asInstanceOf[Perm16]
  def |+|(rhs: Perm16): Perm16 = Perm16Encoding.op(encoding, rhs).asInstanceOf[Perm16]
  def toPrm: Prm = Perm16Encoding.toPrm(encoding)
  def toPerm: Perm = new Perm(toPrm)
  def toPerm32: Perm32 = Perm16Encoding.toPerm32(encoding)
  def apply(cycle: Int*): Perm16 = Perm16Encoding.op(encoding, Perm16.fromPerm(Perm(cycle: _*))).asInstanceOf[Perm16]
}
