package net.alasc.wreath

import spire.algebra._
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.perms.{FaithfulPermRepBuilder, Perm}
import net.alasc.syntax.permutationAction._

/** Describes the wreath product of two objects. */
case class Wr[A](aSeq: Seq[A], h: Perm) {

  override def toString = s"Wr($aSeq, $h)"

  override def equals(that: Any) = sys.error("Not implemented")

  override def hashCode = sys.error("Not implemented")

}

/** Default wreath product object and type classes for wreath products. */
object Wr {

  implicit def wrFaithfulPermRepBuilder[A:Eq:Group:FaithfulPermRepBuilder]: FaithfulPermRepBuilder[Wr[A]] =
    new WrFaithfulPermRepBuilder[A]

  class WrEqGroup[A:Eq:Group] extends Eq[Wr[A]] with Group[Wr[A]] {

    val aSeqEqFiniteGroup = new SeqEqGroup[Seq[A], A]
    def eqv(x: Wr[A], y: Wr[A]): Boolean = (x.h === y.h) && aSeqEqFiniteGroup.eqv(x.aSeq, y.aSeq)
    def id = Wr(Seq.empty[A], Perm.id)
    def inverse(w: Wr[A]): Wr[A] = {
      val hInv = w.h.inverse
      val n = w.aSeq.size.max(w.h.largestMovedPoint.getOrElseFast(-1) + 1)
      Wr(Seq.tabulate(n)( i => w.aSeq.applyOrElse(i <|+| hInv, (k: Int) => Group[A].id).inverse), hInv)
    }
    def op(x: Wr[A], y: Wr[A]): Wr[A] = {
      val newH = x.h |+| y.h
      val n = x.aSeq.size.max(y.aSeq.size).max(x.h.largestMovedPoint.getOrElseFast(-1) + 1)
      Wr(Seq.tabulate(n)( i => x.aSeq.applyOrElse(i, (k: Int) => Group[A].id) |+| y.aSeq.applyOrElse(i <|+| x.h, (k: Int) => Group[A].id) ), newH)
    }

  }

  implicit def wrEqGroup[A:Eq:Group]: Eq[Wr[A]] with Group[Wr[A]] = new WrEqGroup[A]

  def grpDef[A:Eq:Group:FaithfulPermRepBuilder](n: Int, ga: Grp[A], gh: Grp[Perm]): GrpDef[Wr[A]] = {
    val aGenerators = for {
      k <- 0 until n
      a <- ga.generators
    } yield Wr(Seq.tabulate(k + 1)( i => if (i == k) a else Group[A].id ), Perm.id)
    val hGenerators = for {
      h <- gh.generators
    } yield Wr(Seq.empty[A], h)
    val order = ga.order.pow(n) * gh.order
    GrpDef(aGenerators ++ hGenerators, Opt(order))
  }

}
