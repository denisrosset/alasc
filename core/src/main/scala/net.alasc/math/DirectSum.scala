package net.alasc.math

import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.eq._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._

case class DirectSum[S](seq: Seq[S])

object DirectSum {
  implicit def DirectSumSubgroup[S, G](implicit sg: Subgroup[S, G]): Subgroup[DirectSum[S], G] =
    new DirectSumSubgroup[S, G]
}

class DirectSumSubgroup[S, G](implicit val sg: Subgroup[S, G]) extends Subgroup[DirectSum[S], G] {

  implicit def equ: Eq[G] = sg.equ
  implicit def group: Group[G] = sg.group

  type DS = DirectSum[S]
  def iterator(ds: DS) = {
    def rec(i: Int): Iterator[G] =
      if (i == ds.seq.size) Iterator(Group[G].id) else for {
        el1 <- rec(i + 1)
        el2 <- ds.seq(i).iterator
      } yield el1 |+| el2
    rec(0)
  }
  def generators(ds: DS) = ds.seq.flatMap(_.generators)
  def order(ds: DS) = (BigInt(1) /: ds.seq) { case (o, s) => o * s.order }
  def randomElement(ds: DS, gen: Random) = (Group[G].id /: ds.seq) { case (g, s) => g |+| s.randomElement(gen) }
  def contains(ds: DS, el: G) = iterator(ds).exists( g => (el === g))

}

object FixingSeq {
  def apply[P](seq: Seq[Any])(implicit algebra: Permutation[P]): DirectSum[Conjugate[P, Sym[P]]] = {
    val degree = seq.size
    val groups: Seq[Conjugate[P, Sym[P]]] = seq.zipWithIndex.groupBy(_._1).flatMap {
      case (_, Seq(one)) => None
      case (_, pairSeq) =>
        val indices: Set[Int] = pairSeq.map(_._2).toSet
        val remDomain: Set[Int] = (0 until degree).toSet -- indices
        val images = indices.toSeq.sorted ++ remDomain.toSeq.sorted
        val perm = algebra.fromImages(images)
        val permInverse = perm.inverse
        Some(Conjugate(Sym[P](indices.size), perm, permInverse))
    }.toSeq
    DirectSum(groups)
  }
}
