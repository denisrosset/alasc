package net.alasc.wreath

import spire.algebra._
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.perms.FaithfulPermRepBuilder
import net.alasc.syntax.permutationAction._

/** Describes the wreath product of two objects. */
case class Wr[A, H](aSeq: Seq[A], h: H) {

  override def toString = s"Wr($aSeq, $h)"

  override def equals(that: Any) = sys.error("Not implemented")

  override def hashCode = sys.error("Not implemented")

}

/** Default wreath product object and type classes for wreath products. */
object Wr {

  implicit def wrFaithfulPermRepBuilder[A:Group:FaithfulPermRepBuilder, H:Permutation]: FaithfulPermRepBuilder[Wr[A, H]] =
    new WrFaithfulPermRepBuilder[A, H]

  class WrEqGroup[A:Eq:Group, H:Eq:Permutation] extends Eq[Wr[A, H]] with Group[Wr[A, H]] {

    val aSeqEqFiniteGroup = new SeqEqGroup[Seq[A], A]
    def eqv(x: Wr[A, H], y: Wr[A, H]): Boolean = (x.h === y.h) && aSeqEqFiniteGroup.eqv(x.aSeq, y.aSeq)
    def id = Wr(Seq.empty[A], Group[H].id)
    def inverse(w: Wr[A, H]): Wr[A, H] = {
      val hInv = w.h.inverse
      val n = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
      Wr(Seq.tabulate(n)( i => w.aSeq.applyOrElse(i <|+| hInv, (k: Int) => Group[A].id).inverse), hInv)
    }
    def op(x: Wr[A, H], y: Wr[A, H]): Wr[A, H] = {
      val newH = x.h |+| y.h
      val n = x.aSeq.size.max(y.aSeq.size).max(x.h.largestMovedPoint.getOrElseFast(-1) + 1)
      Wr(Seq.tabulate(n)( i => x.aSeq.applyOrElse(i, (k: Int) => Group[A].id) |+| y.aSeq.applyOrElse(i <|+| x.h, (k: Int) => Group[A].id) ), newH)
    }

  }

  implicit def wrEqGroup[A:Eq:Group, H:Eq:Permutation]: Eq[Wr[A, H]] with Group[Wr[A, H]] = new WrEqGroup[A, H]

  def grpDef[A:Eq:Group:FaithfulPermRepBuilder, H:Eq:Permutation](n: Int, ga: Grp[A], gh: Grp[H]): GrpDef[Wr[A, H]] = {
    val aGenerators = for {
      k <- 0 until n
      a <- ga.generators
    } yield Wr(Seq.tabulate(k + 1)( i => if (i == k) a else Group[A].id ), Group[H].id)
    val hGenerators = for {
      h <- gh.generators
    } yield Wr(Seq.empty[A], h)
    val order = ga.order.pow(n) * gh.order
    GrpDef(aGenerators ++ hGenerators, Opt(order))
  }

}
