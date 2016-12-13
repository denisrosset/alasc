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
import net.alasc.perms.{Perm, PermGrpBuilder, PermutationActionGrpBuilder}
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
  def generators: IndexedSeq[G]

  /** Group order. */
  def order: SafeLong

  /** Returns whether this is the trivial group with a single identity element. */
  def isTrivial: Boolean = generators.isEmpty

  /** Returns the group H = hInv G h, where G is this group. */
  def conjugatedBy(h: G)(implicit builder: GrpBuilder[G]): Grp[G] = builder.conjugatedBy(lhs, h)

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
  def union(rhs: Grp[G])(implicit builder: GrpBuilder[G]): Grp[G] = builder.union(lhs, rhs)

  /** Intersection of groups. */
  def intersect(rhs: Grp[G])(implicit builder: GrpBuilder[G]): Grp[G] = builder.intersect(lhs, rhs)

  /** Left cosets. */
  def leftCosetsBy(subgrp: Grp[G])(implicit builder: GrpBuilder[G]): LeftCosets[G, subgrp.type] =
    builder.leftCosetsBy(lhs, subgrp)

  /** Right cosets. */
  def rightCosetsBy(subgrp: Grp[G])(implicit builder: GrpBuilder[G]): RightCosets[G, subgrp.type] =
    builder.rightCosetsBy(lhs, subgrp)

  /** Simplifies the description current group.*/
  def smallGeneratingSet(implicit builder: GrpBuilder[G]): IndexedSeq[G] = builder.smallGeneratingSet(lhs)

  /** Sequence of the group elements, ordered lexicographically by their images.
    * Returns a value only if the action is faithful. */
  def lexElements(action: PermutationAction[G])(implicit builder: PermutationActionGrpBuilder[G]): Opt[BigIndexedSeq[G]]

  /** Returns the subgroup that fixes the given partition under the given action. */
  def fixingPartition(action: PermutationAction[G], partition: Partition)(implicit builder: PermutationActionGrpBuilder[G]): Grp[G]

  /** Returns the subgroup that stabilizes `b` by the given action. */
  def stabilizer(action: PermutationAction[G], b: Int)(implicit builder: PermutationActionGrpBuilder[G]): Grp[G]

  /** If the action of this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point,
    * and the associated transversal.
    */
  def someStabilizerTransversal(action: PermutationAction[G])(implicit builder: PermutationActionGrpBuilder[G]): Opt[(Grp[G], bsgs.Transversal[G, action.type])]

  /** Returns the subgroup that stabilizes `b` under the given action and the associated transversal. */
  def stabilizerTransversal(action: PermutationAction[G], b: Int)(implicit builder: PermutationActionGrpBuilder[G]): (Grp[G], bsgs.Transversal[G, action.type])

  def pointwiseStabilizer(action: PermutationAction[G], set: Set[Int])(implicit builder: PermutationActionGrpBuilder[G]): Grp[G]

  def pointwiseStabilizer(action: PermutationAction[G], points: Int*)(implicit builder: PermutationActionGrpBuilder[G]): Grp[G] =
    pointwiseStabilizer(action, scala.collection.immutable.BitSet(points: _*))

  def setwiseStabilizer(action: PermutationAction[G], set: Set[Int])(implicit builder: PermutationActionGrpBuilder[G]): Grp[G]

  def setwiseStabilizer(action: PermutationAction[G], points: Int *)(implicit builder: PermutationActionGrpBuilder[G]): Grp[G] =
    setwiseStabilizer(action, scala.collection.immutable.BitSet(points: _*))

  /** Finds an element of this group under `actionG` with the same image as `q` under `actionQ`, if it exists. */
  def find[Q:Eq:Group](actionG: PermutationAction[G], actionQ: PermutationAction[Q], q: Q)(implicit builder: PermutationActionGrpBuilder[G]): Opt[G]

  /** Finds an element of this group under `actionG` with the same image as the given permutation, if it exists. */
  def find(actionG: PermutationAction[G], p: Perm)(implicit builder: PermutationActionGrpBuilder[G]): Opt[G]

  /** Returns the subgroup for which `predicate` is satisfied; the test `backtrackTest` is used to
    * prune the search tree.
    *
    * @param action        Action used in the subgroup test
    * @param backtrackTest Tests if a pair (preimage, image) is valid for an element of the subgroup. False
    *                      positives are allowed, but a false negative would incorrectly prune the tree.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup satisfying `predicate`
    */
  def subgroupFor(action: PermutationAction[G], backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean)
                 (implicit builder: PermutationActionGrpBuilder[G]): Grp[G]

  /** Returns a sequence of domain elements such that no element of this group apart from
    * the identity fixes all the points in the sequence.
    * Returns a value if and only if the action is faithful.
    */
  def base(action: PermutationAction[G])(implicit builder: PermutationActionGrpBuilder[G]): Opt[Seq[Int]]

  /** Return the smallest element of the domain moved by this group under the given action, or [[NNNone]]. */
  def smallestMovedPoint(action: PermutationAction[G]): NNOption =
    if (lhs.isTrivial) NNNone else {
      var mn = Int.MinValue
      lhs.generators.foreach { g =>
        action.smallestMovedPoint(g) match {
          case NNOption(i) =>
            if (mn == Int.MinValue)
              mn = i
            else
              mn = spire.math.min(i, mn)
          case _ =>
        }
      }
      if (mn == Int.MinValue) NNNone else NNOption(mn)
    }

  /** Return the largest element of the domain moved by this group under the given action, or [[NNNone]]. */
  def largestMovedPoint(action: PermutationAction[G]): NNOption =
    if (lhs.isTrivial) NNNone else {
      var mx = Int.MinValue
      lhs.generators.foreach { g =>
        mx = spire.math.max(action.largestMovedPoint(g).getOrElse(Int.MinValue), mx)
      }
      if (mx == Int.MinValue) NNNone else NNOption(mx)
    }

  /** Returns an arbitrary element moved by this group under the given action, or [[NNNone]]. */
  def findMovedPoint(action: PermutationAction[G]): NNOption =
    if (lhs.isTrivial) NNNone else {
      lhs.generators.foreach { g =>
        action.findMovedPoint(g) match {
          case NNOption(i) => return NNOption(i)
          case _ =>
        }
      }
      NNNone
    }

  /** Tests if the point `i` is in the support of `g` under the given action. */
  def movesPoint(action: PermutationAction[G], i: Int): Boolean = lhs.generators.exists(g => action.actr(i, g) != i)

  /** Number of non-negative integers moved by the permutations in this group. */
  def nMovedPoints(action: PermutationAction[G]): Int = movedPoints(action).size

  /** Returns a bit set of all non-negative integers k that are moved by the action of this group,
    * i.e. `S = { k | exists g in this group s.t. k <|+| g != k }`.
    */
  def movedPoints(action: PermutationAction[G]): Set[Int] =
    if (lhs.isTrivial) Set.empty[Int] else {
      val b = metal.mutable.ResizableBitSet.empty
      lhs.generators.foreach { g =>
        action.smallestMovedPoint(g) match {
          case NNOption(lb) =>
            cforRange(lb until action.largestMovedPoint(g).get + 1) { i =>
              if (action.actr(i, g) != i)
                b += i
            }
          case _ =>
        }
      }
      b.toScala
    }

  def toPerm(action: PermutationAction[G])(implicit builder: PermGrpBuilder): Grp[Perm]
// TODO if action is not faithful    Grp.fromGeneratorsAndOrder(generators.map(action.toPerm(_)), order)

}

class PermGrpSyntax(val lhs: Grp[Perm]) extends AnyVal {

  /** Sequence of the group elements, ordered lexicographically by their images. */
  def lexElements(implicit builder: PermGrpBuilder): BigIndexedSeq[Perm] = builder.lexElements(lhs)

  /** Returns the subgroup that fixes the given partition. */
  def fixingPartition(partition: Partition)(implicit builder: PermGrpBuilder): Grp[Perm] =
    builder.fixingPartition(lhs, partition)

  /** Returns the subgroup that stabilizes `b`. */
  def stabilizer(b: Int)(implicit builder: PermGrpBuilder): Grp[Perm] =
    builder.stabilizer(lhs, b)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point,
    * and the associated transversal.
    */
  def someStabilizerTransversal(implicit builder: PermGrpBuilder): Opt[(Grp[Perm], bsgs.Transversal[Perm, Perm.algebra.type])] =
    builder.someStabilizerTransversal(lhs)

  /** Returns the subgroup that stabilizes `b` and the associated transversal. */
  def stabilizerTransversal(b: Int)(implicit builder: PermGrpBuilder): (Grp[Perm], bsgs.Transversal[Perm, Perm.algebra.type]) =
    builder.stabilizerTransversal(lhs, b)

  def pointwiseStabilizer(set: Set[Int])(implicit builder: PermGrpBuilder): Grp[Perm] =
    builder.pointwiseStabilizer(lhs, set)

  def pointwiseStabilizer(points: Int*)(implicit builder: PermGrpBuilder): Grp[Perm] =
    pointwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  def setwiseStabilizer(set: Set[Int])(implicit builder: PermGrpBuilder): Grp[Perm] =
    builder.setwiseStabilizer(lhs, set)

  def setwiseStabilizer(points: Int *)(implicit builder: PermGrpBuilder): Grp[Perm] =
    setwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  /** Finds an element of this group with the image as `q`, if it exists. */
  def find[Q:Eq:Group:PermutationAction](q: Q)(implicit builder: PermGrpBuilder): Opt[Perm] = builder.find(lhs, q)

  /** Returns the subgroup for which `predicate` is satisfied; the test `backtrackTest` is used to
    * prune the search tree.
    *
    * @param backtrackTest Tests if a pair (preimage, image) is valid for an element of the subgroup. False
    *                      positives are allowed, but a false negative would incorrectly prune the tree.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup satisfying `predicate`
    */
  def subgroupFor(backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean)
                 (implicit builder: PermGrpBuilder): Grp[Perm] =
    builder.subgroupFor(lhs, backtrackTest, predicate)

  /** Returns a sequence of domain elements such that no element of this group apart from
    * the identity fixes all the points in the sequence.
    */
  def base(implicit builder: PermGrpBuilder): Seq[Int] = builder.base(lhs)

  /** Return the smallest element of the domain moved by this group, or [[NNNone]]. */
  def smallestMovedPoint: NNOption =
    if (lhs.isTrivial) NNNone else {
      var mn = Int.MaxValue
      lhs.generators.foreach { g =>
        mn = spire.math.min(g.smallestMovedPoint.get, mn)
      }
      NNOption(mn)
    }

  /** Return the largest element of the domain moved by this group, or [[NNNone]]. */
  def largestMovedPoint: NNOption =
    if (lhs.isTrivial) NNNone else {
      var mx = 0
      lhs.generators.foreach { g =>
        mx = spire.math.max(g.largestMovedPoint.get, mx)
      }
      NNOption(mx)
    }

  /** Returns an arbitrary element moved by this group, or [[NNNone]]. */
  def findMovedPoint: NNOption =
    if (lhs.isTrivial) NNNone else lhs.generators.head.findMovedPoint // non-empty because generator cannot be the identity

  /** Tests if the point `i` is in the support of `g`. */
  def movesPoint(i: Int): Boolean = lhs.generators.exists(g => (i <|+| g) != i)

  /** Number of non-negative integers moved by the permutations in this group. */
  def nMovedPoints: Int = movedPoints.size

  /** Returns a bit set of all non-negative integers k that are moved by the action of this group,
    * i.e. `S = { k | exists g in this group s.t. k <|+| g != k }`.
    */
  def movedPoints: Set[Int] =
    if (lhs.isTrivial) Set.empty[Int] else {
      val b = metal.mutable.ResizableBitSet.empty
      lhs.generators.foreach { g =>
        cforRange(g.smallestMovedPoint.get until g.largestMovedPoint.get + 1) { i =>
          if (i <|+| g != i)
            b += i
        }
      }
      b.toScala
    }

}

object Grp {

  implicit def partialOrder[G]: PartialOrder[Grp[G]] = new GrpPartialOrder[G]

  implicit def lattice[G](implicit builder: GrpBuilder[G]): Lattice[Grp[G]] with BoundedJoinSemilattice[Grp[G]] = new GrpLattice[G]

  def apply[G](generators: G*)(implicit builder: GrpBuilder[G]): Grp[G] = {
    import builder.{equ, group}
    builder.fromGenerators(generators.filterNot(_.isId).toIndexedSeq)
  }

  def trivial[G](implicit builder: GrpBuilder[G]): Grp[G] = builder.trivial

  def fromGenerators[G](generators: IndexedSeq[G])(implicit builder: GrpBuilder[G]): Grp[G] =
    builder.fromGenerators(generators)

  def fromGeneratorsAndOrder[G](generators: IndexedSeq[G], order: SafeLong)(implicit builder: GrpBuilder[G]): Grp[G] =
    builder.fromGeneratorsAndOrder(generators, order)

  implicit def permGrpSyntax(pg: Grp[Perm]): PermGrpSyntax = new PermGrpSyntax(pg)

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
