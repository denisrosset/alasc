package net.alasc.math

import net.alasc.algebra._
import net.alasc.syntax.subgroup._
import spire.syntax.group._
import scala.util.Random

case class DirectSum[S](seq: Seq[S])

object DirectSum {
  implicit def DirectSumSubgroup[S, G](implicit sg: Subgroup[S, G], algebra: FiniteGroup[G]): Subgroup[DirectSum[S], G] =
    new DirectSumSubgroup[S, G]
}

class DirectSumSubgroup[S, G](implicit val sg: Subgroup[S, G], val algebra: FiniteGroup[G]) extends Subgroup[DirectSum[S], G] {
  type DS = DirectSum[S]
  def iterator(ds: DS) = {
    def rec(i: Int): Iterator[G] =
      if (i == ds.seq.size) Iterator(algebra.id) else for {
        el1 <- rec(i + 1)
        el2 <- ds.seq(i).iterator
      } yield el1 |+| el2
    rec(0)
  }
  def generators(ds: DS) = ds.seq.flatMap(_.generators)
  def order(ds: DS) = (BigInt(1) /: ds.seq) { case (o, s) => o * s.order }
  def randomElement(ds: DS, gen: Random) = (algebra.id /: ds.seq) { case (g, s) => g |+| s.randomElement(gen) }
  def contains(ds: DS, el: G) = iterator(ds).exists( g => algebra.eqv(el, g))
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
        Some(Conjugate(permInverse, Sym[P](indices.size), perm))
    }.toSeq
    DirectSum(groups)
  }
}
