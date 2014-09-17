package net.alasc.math
package wreath

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.mutable
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.util._

/** Describes the wreath product of two objects. */
case class Wr[A, H](aSeq: Seq[A], h: H)

/** Type classes for wreath products. */
object Wr {
  implicit def wrImprimitiveRepresentations[A: FiniteGroup: Representations, H: Permutation]: Representations[Wr[A, H]] = new WrImprimitiveRepresentations[A, H]
  implicit def wrFiniteGroup[A: FiniteGroup, H: Permutation]: FiniteGroup[Wr[A, H]] = new WrFiniteGroup[A, H]
  def grp[SA, SH, A: Representations, H](n: Int, sa: SA, sh: SH)(
    implicit afg: FiniteGroup[A], hp: Permutation[H], sba: Subgroup[SA, A], sbh: Subgroup[SH, H]): Grp[Wr[A, H]] = {
    val aGenerators = for {
      k <- 0 until n
      a <- sa.generators
    } yield Wr(Seq.tabulate(k + 1)( i => if (i == k) a else afg.id ), hp.id)
    val hGenerators = for {
      h <- sh.generators
    } yield Wr(Seq.empty[A], h)
    val order = sa.order.pow(n) * sh.order
    Grp.fromGeneratorsAndOrder(aGenerators ++ hGenerators, order)
  }
}

class WrFiniteGroup[A, H](implicit aAlgebra: FiniteGroup[A], hAlgebra: Permutation[H]) extends FiniteGroup[Wr[A, H]] {
  def eqv(x: Wr[A, H], y: Wr[A, H]): Boolean = (x.h === y.h) && (x.aSeq === y.aSeq)
  def id = Wr(Seq.empty[A], hAlgebra.id)
  def inverse(w: Wr[A, H]): Wr[A, H] = {
    val hInv = w.h.inverse
    val n = w.aSeq.size.max(w.h.supportMax.getOrElse(-1) + 1)
    Wr(Seq.tabulate(n)( i => w.aSeq.applyOrElse(i <|+| hInv, (k: Int) => aAlgebra.id).inverse), hInv)
  }
  def op(x: Wr[A, H], y: Wr[A, H]): Wr[A, H] = {
    val newH = x.h |+| y.h
    val n = x.aSeq.size.max(y.aSeq.size).max(x.h.supportMax.getOrElse(-1) + 1)
    Wr(Seq.tabulate(n)( i => x.aSeq.applyOrElse(i, (k: Int) => aAlgebra.id) |+| y.aSeq.applyOrElse(i <|+| x.h, (k: Int) => aAlgebra.id) ), newH)
  }
}
