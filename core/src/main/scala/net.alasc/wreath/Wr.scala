package net.alasc.wreath

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.syntax.permutationAction._

/** Describes the wreath product of two objects. */
trait Wr[A, H] {

  override def toString = s"Wr($aSeq, $h)"

  def aSeq: Seq[A]

  def h: H

}

/** Default wreath product object and type classes for wreath products. */
object Wr {

  def apply[A, H](aSeq0: Seq[A], h0: H): Wr[A, H] = new Wr[A, H] {
    val aSeq = aSeq0
    val h = h0
  }

  implicit def wrImprimitivePRepBuilder[A:Group:PRepBuilder, H:PermutationBuilder]: PRepBuilder[Wr[A, H]] = new WrImprimitivePRepBuilder[A, H]

  implicit def wrEqGroup[A:Eq:Group, H:Eq:PermutationBuilder]: Eq[Wr[A, H]] with Group[Wr[A, H]] = new WrEqGroup[A, H]

  def grpDef[A:Eq:Group:PRepBuilder, H:Eq:PermutationBuilder](n: Int, ga: Grp[A], gh: Grp[H]): GrpDef[Wr[A, H]] = {
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

class WrEqGroup[A:Eq:Group, H:Eq:PermutationBuilder] extends Eq[Wr[A, H]] with Group[Wr[A, H]] {

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
