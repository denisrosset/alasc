package net.alasc.finite

import scala.util.Random

import spire.algebra.{Eq, Group, PartialOrder}
import spire.algebra.lattice.{BoundedJoinSemilattice, Lattice}
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, Permutation, PermutationBuilder}
import net.alasc.domains.Partition
import net.alasc.perms.{PermGrp, PermGrpBuilder}
import net.alasc.syntax.all._
import net.alasc.util.{NNOption, _}
import metal.syntax._

import net.alasc.bsgs

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
  def generators: Iterable[G]

  /** Group order. */
  def order: BigInt // TODO: replace by SafeLong

  /** Returns whether this is the trivial group with a single identity element. */
  def isTrivial: Boolean = generators.isEmpty

  /** Returns the group H = hInv G h, where G is this group. */
  def conjugatedBy[GG <: Grp[G]](h: G)(implicit builder: GrpBuilder.Aux[G, GG]): GG = builder.conjugatedBy(lhs, h)

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
  def union[GG <: Grp[G]](rhs: Grp[G])(implicit builder: GrpBuilder.Aux[G, GG]): GG = builder.union(lhs, rhs)

  /** Intersection of groups. */
  def intersect[GG <: Grp[G]](rhs: Grp[G])(implicit builder: GrpBuilder.Aux[G, GG]): GG = builder.intersect(lhs, rhs)

  /** Left cosets. */
  def leftCosetsBy(subgrp: Grp[G])(implicit builder: GrpBuilder[G]): LeftCosets[G] =
    builder.leftCosetsBy(lhs, subgrp)

  /** Right cosets. */
  def rightCosetsBy(subgrp: Grp[G])(implicit builder: GrpBuilder[G]): RightCosets[G] =
    builder.rightCosetsBy(lhs, subgrp)

  /** Simplifies the description current group.*/
  def smallGeneratingSet(implicit builder: GrpBuilder[G]): Iterable[G] = builder.smallGeneratingSet(lhs)

  /** Sequence of the group elements, ordered lexicographically by their images. */
  def lexElements(implicit builder: PermGrpBuilder[G]): BigIndexedSeq[G] = builder.lexElements(lhs)

  /** Returns the subgroup that fixes the given partition. */
  def fixingPartition[GG <: PermGrp[G]](partition: Partition)(implicit builder: PermGrpBuilder.Aux[G, GG]): GG =
    builder.fixingPartition(lhs, partition)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point. */
  def someStabilizer[GG <: PermGrp[G]](implicit builder: PermGrpBuilder.Aux[G, GG]): Opt[GG] =
    builder.someStabilizer(lhs)

  /** Returns the subgroup that stabilizes `b`. */
  def stabilizer[GG <: PermGrp[G]](b: Int)(implicit builder: PermGrpBuilder.Aux[G, GG]): GG =
    builder.stabilizer(lhs, b)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point,
    * and the associated transversal.
    */ // TODO: Transversal
  def someStabilizerTransversal[GG <: PermGrp[G]](implicit builder: PermGrpBuilder.Aux[G, GG]): Opt[(GG, bsgs.Transversal[G])] =
    builder.someStabilizerTransversal(lhs)

  /** Returns the subgroup that stabilizes `b` and the associated transversal. */
  def stabilizerTransversal[GG <: PermGrp[G]](b: Int)(implicit builder: PermGrpBuilder.Aux[G, GG]): (GG, bsgs.Transversal[G]) =
    builder.stabilizerTransversal(lhs, b)

  def pointwiseStabilizer[GG <: PermGrp[G]](set: Set[Int])(implicit builder: PermGrpBuilder.Aux[G, GG]): GG =
    builder.pointwiseStabilizer(lhs, set)

  def pointwiseStabilizer[GG <: PermGrp[G]](points: Int*)(implicit builder: PermGrpBuilder.Aux[G, GG]): GG =
    pointwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  def setwiseStabilizer[GG <: PermGrp[G]](set: Set[Int])(implicit builder: PermGrpBuilder.Aux[G, GG]): GG =
    builder.setwiseStabilizer(lhs, set)

  def setwiseStabilizer[GG <: PermGrp[G]](points: Int *)(implicit builder: PermGrpBuilder.Aux[G, GG]): GG =
    setwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  /** Finds an element of this group with the image as `q`, if it exists. */
  def find[Q:Permutation](q: Q)(implicit builder: PermGrpBuilder[G]): Opt[G] = builder.find(lhs, q)

  /** Returns the subgroup for which `predicate` is satisfied; the test `backtrackTest` is used to
    * prune the search tree.
    *
    * @param backtrackTest Tests if a pair (preimage, image) is valid for an element of the subgroup. False
    *                      positives are allowed, but a false negative would incorrectly prune the tree.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup satisfying `predicate`
    */
  def subgroupFor[GG <: PermGrp[G]](backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean)
                                   (implicit builder: PermGrpBuilder.Aux[G, GG]): GG =
    builder.subgroupFor(lhs, backtrackTest, predicate)

  /** Returns a sequence of domain elements such that no element of this group apart from
    * the identity fixes all the points in the sequence.
    */
  def base(implicit builder: PermGrpBuilder[G]): Seq[Int] = builder.base(lhs)

  /** Return the smallest element of the domain moved by this group, or [[NNNone]]. */
  def smallestMovedPoint(implicit permutation: Permutation[G]): NNOption = {
    var mn = Int.MaxValue
    generators.foreach { g =>
      mn = spire.math.min(g.smallestMovedPoint.get, mn)
    }
    NNOption(mn)
  }

  /** Return the largest element of the domain moved by this group, or [[NNNone]]. */
  def largestMovedPoint(implicit permutation: Permutation[G]): NNOption = {
    var mx = 0
    generators.foreach { g =>
      mx = spire.math.max(g.largestMovedPoint.get, mx)
    }
    NNOption(mx)
  }

  /** Returns an arbitrary element moved by this group, or [[NNNone]]. */
  def findMovedPoint(implicit permutation: Permutation[G]): NNOption =
    if (isTrivial) NNNone else generators.head.findMovedPoint // non-empty because generator cannot be the identity

  /** Tests if the point `i` is in the support of `g`. */
  def movesPoint(i: Int)(implicit permutation: Permutation[G]): Boolean = generators.exists(g => i <|+| g != g)

  /** Number of non-negative integers moved by the permutations in this group. */
  def nMovedPoints(implicit permutation: Permutation[G]): Int = movedPoints.size

  /** Returns a bit set of all non-negative integers k that are moved by the action of this group,
    * i.e. `S = { k | exists g in this group s.t. k <|+| g != k }`.
    */
  def movedPoints(implicit permutation: Permutation[G]): Set[Int] =
    if (isTrivial) Set.empty[Int] else {
      val b = metal.mutable.BitSet.empty
      generators.foreach { g =>
        cforRange(g.smallestMovedPoint.get until g.largestMovedPoint.get + 1) { i =>
          if (i <|+| g != i)
            b += i
        }
      }
      b.toScala
    }

  def toPermutation[Q:PermutationBuilder:GrpBuilder](implicit permutation: Permutation[G]): Grp[Q] =
    Grp.fromGeneratorsAndOrder(generators.map(_.toPermutation[Q]), order)

}

object Grp {

  implicit def partialOrder[G]: PartialOrder[Grp[G]] = new GrpPartialOrder[G]

  implicit def lattice[G](implicit builder: GrpBuilder[G]): Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G]

  def apply[G](generators: G*)(implicit builder: GrpBuilder[G]): builder.GG = {
    import builder.{equ, group}
    builder.fromGenerators(generators.filterNot(_.isId))
  }

  def trivial[G](implicit builder: GrpBuilder[G]): builder.GG = builder.trivial

  def fromGenerators[G](generators: Iterable[G])(implicit builder: GrpBuilder[G]): builder.GG =
    builder.fromGenerators(generators)

  def fromGeneratorsAndOrder[G](generators: Iterable[G], order: BigInt)(implicit builder: GrpBuilder[G]): builder.GG =
    builder.fromGeneratorsAndOrder(generators, order)

}

final class GrpLattice[G](implicit val builder: GrpBuilder[G]) extends Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] {

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
