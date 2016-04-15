package net.alasc.perms

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.PartialOrder
import spire.algebra.lattice.{BoundedJoinSemilattice, Lattice}

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._

final class PermutationBuiltRep[B0 <: PermutationRepBuilder[G] with Singleton, G](val size: Int)(implicit val builder: B0) extends BuiltRep[G] with FaithfulPRep[G] {

  def permutationAction = builder.permutation

  type B = B0

  def represents(g: G) = permutationAction.supportMax(g).getOrElseFast(-1) < size

}

final class PermutationRepBuilder[G](implicit val permutation: PermutationBuilder[G]) extends PRepBuilder[G] { self =>

  type R = PermutationBuiltRep[this.type, G]

  def classTagR: ClassTag[R] = implicitly[ClassTag[PermutationBuiltRep[this.type, G]]]

  @tailrec protected def adequateSize(size: Int, iterator: Iterator[G]): Int =
    if (iterator.hasNext)
      adequateSize(size.max(permutation.supportMax(iterator.next).getOrElseFast(-1) + 1), iterator)
    else size

  def forSize(size: Int): R = new PermutationBuiltRep[this.type, G](size)(this)

  def build(generators: Iterable[G]): R = forSize(adequateSize(1, generators.iterator))

  implicit object partialOrder extends PartialOrder[R] {

    def partialCompare(x: R, y: R) = (x.size - y.size).signum.toDouble
  }

  implicit object lattice extends Lattice[R] with BoundedJoinSemilattice[R] {

    def zero: R = new PermutationBuiltRep[PermutationRepBuilder.this.type, G](1)(PermutationRepBuilder.this)

    def join(x: R, y: R) = if (x.size >= y.size) x else y

    def meet(x: R, y: R) = if (x.size <= y.size) x else y

  }

}
