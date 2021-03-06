package net.alasc.finite

import scala.util.Random

import spire.algebra.{Action, Eq, Group, PartialOrder}
import spire.algebra.lattice.{BoundedJoinSemilattice, Lattice}
import spire.math.SafeLong
import spire.syntax.group._

import net.alasc.perms.Perm
import net.alasc.syntax.all._

import net.alasc.attributes.Attributes

/** Finite group base class. */
abstract class Grp[G0] extends FinitelyGeneratedGrp { lhs =>

  type G = G0

  def ===(rhs: Grp[G0])(implicit equ1: Eq[Grp[G0]]): Boolean = equ1.eqv(lhs, rhs)

  override def toString = generators.mkString("Grp(", ", ", ")")

  override def hashCode: Int = sys.error("Object.hashCode not defined for Grp")

  override def equals(that: Any): Boolean = sys.error("Object.equals not defined for Grp, use typesafe operator === instead")

  /** Iterator through all the group elements. */
  def iterator: Iterator[G0]

  /** Returns whether `g` is contained in this group. */
  def contains(g: G0): Boolean

  /** Group order. */
  def order: SafeLong

  /** Generates a random element. */
  def randomElement(random: Random): G0

  /** Tests whether `rhsGenerators` is a subgroup of this group. */
  def hasSubgroup(rhs: Grp[G0]): Boolean = rhs.generators.forall(contains)

  /** Tests whether this group is a subgroup of `rhsGenerators`. */
  def isSubgroupOf(rhs: Grp[G0]): Boolean = generators.forall(rhs.contains)

  /** This group `lhs` normalizes the group `rhsGenerators` if for every g in lhs and u in rhsGenerators, the element g^-1 u g is
    * a member of rhsGenerators. Note that `rhsGenerators` needs not be a subgroup of `lhs`.
    * */
  def normalizes(rhs: Grp[G0]): Boolean = generators.forall { g =>
    val gInv = g.inverse
    rhs.generators.forall(u => rhs.contains(gInv |+| u |+| g))
  }

}

object Grp {

  object Attributes extends Attributes("Grp") {
    object ConjugacyClasses extends Attribute("ConjugacyClasses") {
      implicit def forGrp[G]: For[Grp[G], ConjugacyClasses[G]] = For
    }
    object DerivedSubgroup extends Attribute("DerivedSubgroup") {
      implicit def forGrp[G]: For[Grp[G], Grp[G]] = For
    }
    object SmallGeneratingSet extends Attribute("SmallGeneratingSet") {
      implicit def forGrp[G]: For[Grp[G], Seq[G]] = For
    }
    object IsAbelian extends Attribute.Property("IsAbelian")
    object IsCyclic extends Attribute.Property("IsCyclic")
  }

  implicit def partialOrder[G]: PartialOrder[Grp[G]] = new GrpPartialOrder[G]

  implicit def lattice[G](implicit ev: GrpGroup[G]): Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G]

  def apply[G:Eq:Group](generators: G*)(implicit ev: GrpGroup[G]): Grp[G] =
    ev.fromGenerators(generators.filterNot(_.isId).toIndexedSeq)

  def trivial[G:Eq:Group]: Grp[G] = GrpTrivial[G]()

  def fromGenerators[G](generators: Seq[G])(implicit ev: GrpGroup[G]): Grp[G] =
    ev.fromGenerators(generators)

  def fromGeneratorsAndOrder[G](generators: Seq[G], order: SafeLong)(implicit ev: GrpGroup[G]): Grp[G] =
    ev.fromGeneratorsAndOrder(generators, order)

  implicit def permGrpSyntax(pg: Grp[Perm]): GrpPermSyntax = new GrpPermSyntax(pg)

  implicit def grpGroupSyntax[G](grp: Grp[G]): GrpGroupSyntax[G] = new GrpGroupSyntax[G](grp)

  implicit def grpPermutationActionSyntax[G](grp: Grp[G]): GrpPermutationActionSyntax[G] = new GrpPermutationActionSyntax[G](grp)

  implicit def grpStructureSyntax[G](grp: Grp[G]): GrpStructureSyntax[G] = new GrpStructureSyntax[G](grp)

  def conjugationAction[G:Group:GrpGroup]: Action[Grp[G], G] = new Action[Grp[G], G] {
    def actr(grp: Grp[G], g: G): Grp[G] = GrpGroup[G].conjugatedBy(grp, g)
    def actl(g: G, grp: Grp[G]): Grp[G] = GrpGroup[G].conjugatedBy(grp, g.inverse)
  }

  def commutator[G](grp1: Grp[G], grp2: Grp[G])(implicit ev: GrpStructure[G]): Grp[G] = ev.commutator(grp1, grp2)

}

case class GrpTrivial[G]()(implicit val equ: Eq[G], val group: Group[G]) extends Grp[G] {

  def iterator: Iterator[G] = Iterator(group.id)

  def contains(g: G): Boolean = g.isId

  def generators: Seq[G] = Seq.empty

  def nGenerators = 0

  def generator(i: Int) = throw new ArrayIndexOutOfBoundsException("The group has no generators")

  def order: SafeLong = SafeLong.one

  def randomElement(random: Random): G = group.id

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
