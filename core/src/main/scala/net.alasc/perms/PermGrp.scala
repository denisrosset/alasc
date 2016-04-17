package net.alasc.perms

import spire.syntax.action._
import spire.syntax.cfor._
import spire.util.Opt

import metal.syntax._

import net.alasc.algebra.{BigIndexedSeq, Permutation}
import net.alasc.domains.Partition
import net.alasc.finite.Grp
import net.alasc.prep.bsgs
import net.alasc.syntax.all._
import net.alasc.util._

abstract class PermGrp[G] extends Grp[G] { lhs =>

  implicit def builder: PermGrpBuilder[G]

  implicit def permutation: Permutation[G]

  /** Return the smallest element of the domain moved by this group, or [[NNNone]]. */
  def smallestMovedPoint: NNOption = {
    var mn = Int.MaxValue
    generators.foreach { g =>
      mn = spire.math.min(g.smallestMovedPoint.get, mn)
    }
    NNOption(mn)
  }

  /** Return the largest element of the domain moved by this group, or [[NNNone]]. */
  def largestMovedPoint: NNOption = {
    var mx = 0
    generators.foreach { g =>
      mx = spire.math.max(g.largestMovedPoint.get, mx)
    }
    NNOption(mx)
  }

  /** Returns an arbitrary element moved by this group, or [[NNNone]]. */
  def findMovedPoint: NNOption =
    if (isTrivial) NNNone else generators.head.findMovedPoint // non-empty because generator cannot be the identity

  /** Tests if the point `i` is in the support of `g`. */
  def movesPoint(i: Int): Boolean = generators.exists(g => i <|+| g != g)

  /** Number of non-negative integers moved by the permutations in this group. */
  def nMovedPoints(g: G): Int = movedPoints(g).size

  /** Returns a bit set of all non-negative integers k that are moved by the action of this group,
    * i.e. `S = { k | exists g in this group s.t. k <|+| g != k }`.
    */
  def movedPoints(g: G): Set[Int] =
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

  /** Returns a sequence of domain elements such that no element of this group apart from
    * the identity fixes all the points in the sequence.
    */
  def base: Seq[Int]

  /** Sequence of the group elements, ordered lexicographically by their images. */
  def lexElements: BigIndexedSeq[G]

  /** Returns the subgroup that fixes the given partition. */
  def fixingPartition(partition: Partition): Grp[G] = builder.fixingPartition(lhs, partition)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point. */
  def someStabilizer: Opt[Grp[G]] = builder.someStabilizer(lhs)

  /** Returns the subgroup that stabilizes `b`. */
  def stabilizer(b: Int): Grp[G] = builder.stabilizer(lhs, b)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point,
    * and the associated transversal.
    */
  def someStabilizerTransversal: Opt[(Grp[G], bsgs.Transversal[G])] = builder.someStabilizerTransversal(lhs)

  /** Returns the subgroup that stabilizes `b` and the associated transversal. */
  def stabilizerTransversal(b: Int): (Grp[G], bsgs.Transversal[G]) = builder.stabilizerTransversal(lhs, b)

  def pointwiseStabilizer(set: Set[Int]): Grp[G] = builder.pointwiseStabilizer(lhs, set)

  def pointwiseStabilizer(points: Int*): Grp[G] = pointwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  def setwiseStabilizer(set: Set[Int]): Grp[G] = builder.setwiseStabilizer(lhs, set)

  def setwiseStabilizer(points: Int *): Grp[G] = setwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  /** Finds an element of this group with the image as `q`, if it exists. */
  def find[Q:Permutation](q: Q): Opt[G]

  /** Returns the subgroup for which `predicate` is satisfied; the test `backtrackTest` is used to
    * prune the search tree.
    *
    * @param backtrackTest Tests if a pair (preimage, image) is valid for an element of the subgroup. False
    *                      positives are allowed, but a false negative would incorrectly prune the tree.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup satisfying `predicate`
    */
  def subgroupFor(backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): Grp[G] =
    builder.subgroupFor(lhs, backtrackTest, predicate)

}
