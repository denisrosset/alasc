package net.alasc.enum

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra._
import spire.algebra.partial.RightPartialAction
import spire.syntax.group._

import net.alasc.algebra._
import net.alasc.domains._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.prep.bsgs._
import net.alasc.prep.chain._

abstract class Representatives[T, G] extends Iterable[Representative[T, G]] {

  override def toString = s"Representatives($head, ... total of $length elements)"

  implicit def enumerable: Enumerable[T]

  implicit def permutable: Permutable[T, G]

  implicit def action: RightPartialAction[T, G] = permutable.action

  implicit def builder: PGrpBuilder[G]

  def t: T

  val grp: Grp[G]

  def partition: Partition = enumerable.partition(t)

  def pRep: FaithfulPRep[G] = permutable.pRep(t)

  def symGrp: Grp.SubgroupOf[grp.type, G] = permutable.symmetryGroup(t, grp).asSubgroupOf(grp).get

  def length: BigInt = grp.order / symGrp.order

  override def size = if (length.isValidInt) length.toInt else throw new IllegalArgumentException("Method size cannot be called when the size is bigger than Int.MaxValue. Use .length instead.")

  def iterator = grp.rightCosetsBy(symGrp).iterator.map { coset => Representative(t, coset.g) }

}

final class RepresentativesImpl[T, G](
    val t: T,
    val grp: Grp[G]
  )(implicit
    val builder: PGrpBuilder[G],
    val enumerable: Enumerable[T],
    val permutable: Permutable[T, G]
  ) extends Representatives[T, G] {

  override lazy val partition = super.partition

  override lazy val symGrp = super.symGrp

}

object Representatives {

  def apply[T:Enumerable, G:PGrpBuilder](t: T, grp: Grp[G])(implicit permutable: Permutable[T, G]): Representatives[T, G] =
    new RepresentativesImpl[T, G](t, grp)

  def searchable[T:EnumerableSearchable, G:PGrpChainBuilder](t: T, grp: Grp[G])(implicit permutable: Permutable[T, G]): RepresentativesSearchable[T, G] = new RepresentativesSearchableImpl[T, G](t, grp)

  def ordered[T, G:ClassTag:PGrpChainBuilder, A](t: T, grp: Grp[G])(implicit permutable: Permutable[T, G], enumerable: EnumerableOrdered[T, A]): RepresentativesOrdered[T, G, A] = new RepresentativesOrderedImpl[T, G, A](t, grp)

}
