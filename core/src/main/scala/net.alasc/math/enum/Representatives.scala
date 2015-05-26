package net.alasc
package math
package enum

import scala.annotation.tailrec

import spire.algebra._
import spire.algebra.partial.RightPartialAction
import spire.syntax.group._
import net.alasc.algebra._
import net.alasc.math.guide._

trait Representatives[T, G] extends Iterable[Representative[T, G]] {
  override def toString = s"Representatives($head, ... total of $length elements)"
  implicit def T: Enumerable[T]
  implicit def TG: Permutable[T, G]
  implicit def actionTG: RightPartialAction[T, G] = TG.action
  def t: T
  def grp: Grp[G]
  def partition: Domain#Partition = T.partition(t)
  def representation: Representation[G] = TG.representation(t)
  def symGrp: Grp[G] = TG.symmetryGroup(t, grp)
  def length: BigInt = grp.order / symGrp.order
  override def size = if (length.isValidInt) length.toInt else throw new IllegalArgumentException("Method size cannot be called when the size is bigger than Int.MaxValue. Use .length instead.")
  def iterator = (symGrp \ grp).iterator.map { coset => Representative(t, coset.g) }
}

final class RepresentativesImpl[T, G](val t: T, val grp: Grp[G])(implicit val T: Enumerable[T], val TG: Permutable[T, G]) extends Representatives[T, G] {
  override lazy val partition = super.partition
  override lazy val symGrp = super.symGrp
}

object Representatives {
  def apply[T: Enumerable, G](t: T, grp: Grp[G])(implicit TG: Permutable[T, G]): Representatives[T, G] = new RepresentativesImpl[T, G](t, grp)
  def searchable[T: EnumerableSearchable, G](t: T, grp: Grp[G])(implicit TG: Permutable[T, G]): RepresentativesSearchable[T, G] = new RepresentativesSearchableImpl[T, G](t, grp)
  def ordered[T, G, A](t: T, grp: Grp[G])(implicit TG: Permutable[T, G], T: EnumerableOrdered[T, A]): RepresentativesOrdered[T, G, A] = new RepresentativesOrderedImpl[T, G, A](t, grp)
}
