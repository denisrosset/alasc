package net.alasc.math

import net.alasc.algebra._
import net.alasc.syntax.subgroup._
import spire.syntax.group._
import scala.util.Random

case class DirectSum[S](seq: Seq[S])

object DirectSum {
  implicit def DirectSumSubgroup[S, G](implicit sg: Subgroup[S, G], scalar: FiniteGroup[G]): Subgroup[DirectSum[S], G] =
    new DirectSumSubgroup[S, G]
  implicit def DirectSumPermutationSubgroup[S, P](implicit sg: PermutationSubgroup[S, P], scalar: Permutation[P]): PermutationSubgroup[DirectSum[S], P] =
    new DirectSumPermutationSubgroup[S, P]
}
class DirectSumSubgroup[S, G](implicit val sg: Subgroup[S, G], val scalar: FiniteGroup[G]) extends Subgroup[DirectSum[S], G] {
  type DS = DirectSum[S]
  def elementsIterator(ds: DS, i: Int): Iterator[G] =
    if (i == ds.seq.size) Iterator(scalar.id) else for {
      el1 <- elementsIterator(ds, i + 1)
      el2 <- ds.seq(i).elements
    } yield el1 |+| el2
  def elements(ds: DS) = new Iterable[G] {
      override def stringPrefix = "Elements"
      def iterator = elementsIterator(ds, 0)
  }
  def generators(ds: DS) = ds.seq.flatMap(_.generators)
  def order(ds: DS) = (BigInt(1) /: ds.seq) { case (o, s) => o * s.order }
  def random(ds: DS, gen: Random) = (scalar.id /: ds.seq) { case (g, s) => g |+| s.random(gen) }
}

class DirectSumPermutationSubgroup[S, P](implicit val sg: PermutationSubgroup[S, P], val scalar: Permutation[P])
    extends PermutationSubgroup[DirectSum[S], P] {
  type DS = DirectSum[S]
  def elementsIterator(ds: DS, i: Int): Iterator[P] =
    if (i == ds.seq.size) Iterator(scalar.id) else for {
      el1 <- elementsIterator(ds, i + 1)
      el2 <- ds.seq(i).elements
    } yield el1 |+| el2
  def elements(ds: DS) = new Iterable[P] {
      override def stringPrefix = "Elements"
      def iterator = elementsIterator(ds, 0)
  }
  def generators(ds: DS) = ds.seq.flatMap(_.generators)
  def order(ds: DS) = (BigInt(1) /: ds.seq) { case (o, s) => o * s.order }
  def random(ds: DS, gen: Random) = (scalar.id /: ds.seq) { case (p, s) => p |+| s.random(gen) }
}

object FixingSeq {
  def apply[P](seq: Seq[Any])(implicit permutation: Permutation[P], builder: PermutationBuilder[P]): DirectSum[Conjugate[P, Sym[P]]] = {
    val degree = seq.size
    val groups: Seq[Conjugate[P, Sym[P]]] = seq.zipWithIndex.groupBy(_._1).flatMap {
      case (_, Seq(one)) => None
      case (_, pairSeq) =>
        val indices: Set[Int] = pairSeq.map(_._2).toSet
        val remDomain: Set[Int] = (0 until degree).toSet -- indices
        val images = indices.toSeq.sorted ++ remDomain.toSeq.sorted
        val permInverse = builder.fromImages(images)
        val perm = permInverse.inverse
        Some(Conjugate(perm, Sym[P](indices.size), permInverse))
    }.toSeq
    DirectSum(groups)
  }
}
