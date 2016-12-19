package net.alasc.finite

import scala.util.Random
import spire.algebra.{Eq, Group, PartialOrder}
import spire.algebra.lattice.{BoundedJoinSemilattice, Lattice}
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt
import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.domains.Partition
import net.alasc.perms.Perm
import net.alasc.syntax.all._
import net.alasc.util.{NNOption, _}
import metal.syntax._

/** Finite group base class. */
abstract class Grp[G] { lhs =>

  def ===(rhs: Grp[G])(implicit equ1: Eq[Grp[G]]): Boolean = equ1.eqv(lhs, rhs)

  override def toString = generators.mkString("Grp(", ", ", ")")

  override def hashCode = sys.error("Object.hashCode not defined for Grp")

  /** Group operations on type `G`. */
  implicit def group: Group[G]

  /** Equality for type `G`. */
  implicit def equ: Eq[G]

  /** Iterator through all the group elements. */
  def iterator: Iterator[G]

  /** Returns whether `g` is contained in this group. */
  def contains(g: G): Boolean

  /** Generators of the group, does not contain the identity. */
  def generators: IndexedSeq[G]

  /** Group order. */
  def order: SafeLong

  /** Returns whether this is the trivial group with a single identity element. */
  def isTrivial: Boolean = generators.isEmpty

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

}

case class GrpTrivial[G]()(implicit val equ: Eq[G], val group: Group[G]) extends Grp[G] {

  def iterator: Iterator[G] = Iterator(group.id)

  def contains(g: G): Boolean = g.isId

  def generators: IndexedSeq[G] = IndexedSeq.empty

  def order: SafeLong = SafeLong.one

  def randomElement(random: Random): G = group.id

}

abstract class Grp0 {

  implicit def permutationActionGrpSyntax[G](grp: Grp[G]): GrpPermutationActionSyntax[G] = new GrpPermutationActionSyntax[G](grp)

}

object Grp {

  implicit def partialOrder[G]: PartialOrder[Grp[G]] = new GrpPartialOrder[G]

  implicit def lattice[G](implicit ev: GrpGroup[G]): Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G]

  def apply[G:Eq:Group](generators: G*)(implicit ev: GrpGroup[G]): Grp[G] =
    ev.fromGenerators(generators.filterNot(_.isId).toIndexedSeq)

  def trivial[G:Eq:Group]: Grp[G] = GrpTrivial[G]()

  def fromGenerators[G](generators: IndexedSeq[G])(implicit ev: GrpGroup[G]): Grp[G] =
    ev.fromGenerators(generators)

  def fromGeneratorsAndOrder[G](generators: IndexedSeq[G], order: SafeLong)(implicit ev: GrpGroup[G]): Grp[G] =
    ev.fromGeneratorsAndOrder(generators, order)

  implicit def permGrpSyntax(pg: Grp[Perm]): GrpPermSyntax = new GrpPermSyntax(pg)

  implicit def grpGroupSyntax[G](grp: Grp[G]): GrpGroupSyntax[G] = new GrpGroupSyntax[G](grp)

}

final class GrpLattice[G](implicit val builder: GrpGroup[G]) extends Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] {

  def zero = builder.trivial

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
