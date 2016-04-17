package net.alasc.finite

import scala.util.Random

import spire.algebra.{Eq, Group, PartialOrder}
import spire.algebra.lattice.{Lattice, BoundedJoinSemilattice}
import spire.syntax.group._

/** Finite group base class. */
abstract class Grp[G] { lhs =>

  override def toString = generators.mkString("Grp(", ", ", ")") +  s" of order $order"

  override def hashCode = sys.error("Object.hashCode not defined for Grp")

  override def equals(any: Any) = sys.error("Object.equals not defined for Grp")

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
  def order: BigInt // TODO: replace by SafeLong

  /** Returns whether this is the trivial group with a single identity element. */
  def isTrivial: Boolean = generators.isEmpty

  /** Returns the group H = hInv G h, where G is this group. */
  def conjugatedBy(h: G): Grp[G] = builder.conjugatedBy(lhs, h)

  /** Generates a random element. */
  def randomElement(random: Random): G

  /** Tests whether `rhs` is a subgroup of this group. */
  def hasSubgroup(rhs: Grp[G]): Boolean = rhs.generators.forall(contains)

  /** Tests whether this group is a subgroup of `rhs`. */
  def isSubgroupOf(rhs: Grp[G]): Boolean = generators.forall(rhs.contains)

  /** This group `lhs` normalizes the group `rhs` if for every g in lhs and u in rhs, the element g^-1 u g is
    * a member of rhs. Note that `rhs` needs not be a subgroup of `lhs`.
    * */
  def normalizes(rhs: Grp[G]): Boolean = generators.forall { g =>
    val gInv = g.inverse
    rhs.generators.forall(u => rhs.contains(gInv |+| u |+| g))
  }

  /** Union (closure) of groups. */
  def union(rhs: Grp[G]): Grp[G] = builder.union(lhs, rhs)

  /** Intersection of groups. */
  def intersect(rhs: Grp[G]): Grp[G] = builder.intersect(lhs, rhs)

  /** Left cosets. */
  def leftCosetsBy(subgrp: Grp[G]): LeftCosets[G] = builder.leftCosetsBy(lhs, subgrp)

  /** Right cosets. */
  def rightCosetsBy(subgrp: Grp[G]): RightCosets[G] = builder.rightCosetsBy(lhs, subgrp)

  /** Simplifies the description current group.*/
  def smallGeneratingSet: Iterable[G] = builder.smallGeneratingSet(lhs)

}

object Grp {

  implicit def partialOrder[G]: PartialOrder[Grp[G]] = new GrpPartialOrder[G]

  implicit def lattice[G](implicit builder: GrpBuilder[G]): Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G]

  def apply[G](generators: G*)(implicit builder: GrpBuilder[G]): Grp[G] = {
    import builder.{equ, group}
    builder.fromGenerators(generators.filterNot(_.isId))
  }

  def trivial[G](implicit builder: GrpBuilder[G]): Grp[G] = builder.trivial

  def fromGenerators[G](generators: Iterable[G])(implicit builder: GrpBuilder[G]): Grp[G] =
    builder.fromGenerators(generators)

  def fromGeneratorsAndOrder[G](generators: Iterable[G], order: BigInt)(implicit builder: GrpBuilder[G]): Grp[G] =
    builder.fromGeneratorsAndOrder(generators, order)

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
