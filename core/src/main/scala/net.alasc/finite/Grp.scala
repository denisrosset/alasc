package net.alasc.finite

import scala.util.Random

import spire.algebra.{Eq, Group, PartialOrder}
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}
import spire.syntax.group._
import spire.util.Opt

/** Finite group base class. */
abstract class Grp[G] {

  override def toString = generators.mkString("Grp(", ", ", ")") +  s" of order $order"

  override def hashCode = sys.error("HashCode not defined for Grp")

  override def equals(any: Any) = any match {
    case that: Grp[G] =>
      order == that.order &&
      group.id == that.group.id &&
      that.generators.forall(contains(_))
    case _ => false
  }

  type Parent <: Grp[G] with Singleton

  implicit def builder: GrpBuilder[G]

  /** Group operations on type `G`. */
  implicit def group: Group[G] = builder.group

  /** Equality for type `G`. */
  implicit def equ: Eq[G] = builder.equ

  /** Iterator through all the group elements. */
  def iterator: Iterator[G]

  /** Returns whether `g` is contained in this group. */
  def contains(g: G): Boolean

  /** Generators of the group, does not contain the identity. */
  def generators: Iterable[G]

  /** Group order. */
  def order: BigInt

  /** Returns whether this is the trivial group with a single identity element. */
  def isTrivial: Boolean = generators.isEmpty

  /** Returns the group H = hInv G h, where G is this group. */
  def conjugatedBy(h: G, hInvOpt: Opt[G] = Opt.empty): Grp[G] = {
    val hInv = hInvOpt match {
      case Opt(e) => e
      case _ => h.inverse
    }
    def conjugatedRandomElement(random: Random): G = hInv |+| randomElement(random) |+| h
    builder.fromGeneratorsRandomElementsAndOrder(generators.map(g => hInv |+| g |+| h),
      conjugatedRandomElement(_), order)
  }

  /** Generates a random element. */
  def randomElement(random: Random): G

  /** Tests whether `rhs` is a subgroup of this group. */
  def hasSubgroup(rhs: Grp[G]): Boolean = rhs.generators.forall(contains(_))

  /** Tests whether this group is a subgroup of `rhs`. */
  def isSubgroupOf(rhs: Grp[G]): Boolean = generators.forall(rhs.contains(_))

  /** Union (closure) of groups. */
  def union(rhs: Grp[G]): Grp[G]

  /** Intersection of groups. */
  def intersect(rhs: Grp[G]): Grp[G]

  /** Left cosets. */
  def leftCosetsBy(rhs: Grp.WithParent[this.type, G]): LeftCosets[G]

  /** Right cosets. */
  def rightCosetsBy(rhs: Grp.WithParent[this.type, G]): RightCosets[G]

  /** Simplifies the description current group.*/
  def simplified: Grp[G]

}

object Grp {

  implicit def partialOrder[G]: PartialOrder[Grp[G]] = new GrpPartialOrder[G]

  implicit def lattice[G](implicit builder: GrpBuilder[G]): Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G]

  type WithParent[P <: Grp[G] with Singleton, G] = Grp[G] { type Parent = P }

  implicit def WithParent[P <: Grp[G] with Singleton, G](subgrp: Grp[G])(implicit witness: shapeless.Witness.Aux[P]): Grp.WithParent[P, G] = {
    val parentGrp: Grp[G] = witness.value
    if (!subgrp.generators.forall(parentGrp.contains(_)))
      throw new IllegalArgumentException(s"Group $subgrp is not a subgrp of $parentGrp")
    subgrp.asInstanceOf[Grp.WithParent[P, G]]
  }

  def apply[G](generators: G*)(implicit builder: GrpBuilder[G]): Grp[G] = {
    import builder.{equ, group}
    builder.fromGenerators(generators.filterNot(_.isId))
  }

  def trivial[G](implicit builder: GrpBuilder[G]): Grp[G] = builder.trivial

  def fromGenerators[G](generators: Iterable[G])(implicit builder: GrpBuilder[G]): Grp[G] =
    builder.fromGenerators(generators)

  def fromGeneratorsAndOrder[G](generators: Iterable[G], order: BigInt)(implicit builder: GrpBuilder[G]): Grp[G] =
    builder.fromGeneratorsAndOrder(generators, order)

  def fromGeneratorsRandomElementsAndOrder[G](generators: Iterable[G], randomElement: Random => G, order: BigInt)(implicit builder: GrpBuilder[G]): Grp[G] =
    builder.fromGeneratorsRandomElementsAndOrder(generators, randomElement, order)

}

final class GrpLattice[G](implicit builder: GrpBuilder[G]) extends Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] {

  def zero = Grp.trivial[G]

  def join(x: Grp[G], y: Grp[G]) = x union y

  def meet(x: Grp[G], y: Grp[G]) = x intersect y

}

final class GrpPartialOrder[G] extends PartialOrder[Grp[G]] {

  override def eqv(x: Grp[G], y: Grp[G]): Boolean = (x.order == y.order) && lteqv(x, y)
  override def lteqv(x: Grp[G], y: Grp[G]): Boolean = x.generators.forall(y.contains)
  override def gteqv(x: Grp[G], y: Grp[G]): Boolean = y.generators.forall(x.contains)
  override def lt(x: Grp[G], y: Grp[G]): Boolean = (x.order < y.order) && lteqv(x, y)
  override def gt(x: Grp[G], y: Grp[G]): Boolean = (x.order > y.order) && gteqv(x, y)

  def partialCompare(x: Grp[G], y: Grp[G]): Double = {
    val c = x.order.compare(y.order)
    if (c < 0) {
      if (lteqv(x, y)) -1.0 else Double.NaN
    } else if (c > 0) {
      if (gteqv(x, y)) 1.0 else Double.NaN
    } else { // c == 0
      if (lteqv(x, y)) 0.0 else Double.NaN
    }
  }

}
