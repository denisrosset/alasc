package net.alasc.perms

import scala.annotation.tailrec

import net.alasc.algebra.Permutation

trait FaithfulPermRepBuilder[G] {

  def build(generators: Iterable[G]): FaithfulPermRep[G]

}

object FaithfulPermRepBuilder {

  final class FromPermutation[G](implicit val permutation: Permutation[G])
    extends FaithfulPermRepBuilder[G] {
    self =>

    final class MyRep(val dimension: Int) extends FaithfulPermRep[G] {

      type F = permutation.type
      val permutationAction: F = permutation

      def represents(g: G) = permutationAction.largestMovedPoint(g).getOrElseFast(-1) < dimension

    }

    @tailrec protected def adequateSize(size: Int, iterator: Iterator[G]): Int =
      if (iterator.hasNext)
        adequateSize(size.max(permutation.largestMovedPoint(iterator.next).getOrElseFast(-1) + 1), iterator)
      else size

    def build(generators: Iterable[G]): MyRep = new MyRep(adequateSize(1, generators.iterator))

  }

  implicit def fromPermutation[G](implicit permutation: Permutation[G]): FaithfulPermRepBuilder[G] =
    new FromPermutation[G]

}
