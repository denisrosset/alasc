package net.alasc.perms

import scala.annotation.tailrec

import spire.algebra.Ring

import net.alasc.algebra.Permutation

trait FaithfulPermRepBuilder[G] {

  def build[K:Ring](generators: Iterable[G]): FaithfulPermRep[G, K]

}

object FaithfulPermRepBuilder {

  final class FromPermutation[G](implicit val permutation: Permutation[G])
    extends FaithfulPermRepBuilder[G] {
    self =>

    final class MyRep[K](val dimension: Int)(implicit val scalar: Ring[K]) extends FaithfulPermRep[G, K] {

      type F = permutation.type
      val permutationAction: F = permutation

      def represents(g: G) = permutationAction.largestMovedPoint(g).getOrElseFast(-1) < dimension

    }

    @tailrec protected def adequateSize(size: Int, iterator: Iterator[G]): Int =
      if (iterator.hasNext)
        adequateSize(size.max(permutation.largestMovedPoint(iterator.next).getOrElseFast(-1) + 1), iterator)
      else size

    def build[K:Ring](generators: Iterable[G]): MyRep[K] = new MyRep[K](adequateSize(1, generators.iterator))

  }

  implicit def fromPermutation[G](implicit permutation: Permutation[G]): FaithfulPermRepBuilder[G] =
    new FromPermutation[G]

  def apply[G](implicit G: FaithfulPermRepBuilder[G]): FaithfulPermRepBuilder[G] = G

}
