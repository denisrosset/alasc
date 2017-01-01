package net.alasc.rep

import scala.annotation.tailrec

import spire.algebra.Ring

import net.alasc.perms.{Perm, PermAlgebra}

trait FaithfulPermRepBuilder[G] {

  def build[K:Ring](generators: Iterable[G]): FaithfulPermRep[G, K]

}

object FaithfulPermRepBuilder {

  implicit object perm extends FaithfulPermRepBuilder[Perm] {

    final class MyRep[K](val dimension: Int)(implicit val scalar: Ring[K]) extends FaithfulPermRep[Perm, K] {

      type F = PermAlgebra.type
      val permutationAction: F = Perm.algebra

      def represents(g: Perm) = permutationAction.largestMovedPoint(g).getOrElseFast(-1) < dimension

    }

    @tailrec protected def adequateSize(size: Int, iterator: Iterator[Perm]): Int =
      if (iterator.hasNext)
        adequateSize(size.max(iterator.next.largestMovedPoint.getOrElseFast(-1) + 1), iterator)
      else size

    def build[K:Ring](generators: Iterable[Perm]): MyRep[K] = new MyRep[K](adequateSize(1, generators.iterator))

  }

  def apply[G](implicit G: FaithfulPermRepBuilder[G]): FaithfulPermRepBuilder[G] = G

}
