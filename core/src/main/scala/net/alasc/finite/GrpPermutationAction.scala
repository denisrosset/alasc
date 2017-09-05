package net.alasc.finite

import spire.syntax.cfor._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.partitions.Partition
import net.alasc.perms.Perm
import net.alasc.syntax.permutationAction._
import net.alasc.util.{NNNone, NNOption}

import metal.syntax._

/** Group methods that depend on a PermutationAction. */
trait GrpPermutationAction[G] extends GrpAction[G, Int, PermutationAction[G]] {

  def findSameAction[Q:PermutationAction](grp: Grp[G], action: PermutationAction[G], q: Q): Opt[G]

  def lexElements(grp: Grp[G], action: PermutationAction[G]): Opt[BigIndexedSeq[G]]

  def fixingPartition(grp: Grp[G], action: PermutationAction[G], partition: Partition): Grp[G]

  def base(grp: Grp[G], action: PermutationAction[G]): Opt[Seq[Int]]

  def subgroupFor(grp: Grp[G], action: PermutationAction[G], backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean): Grp[G]

  def toPerm(grp: Grp[G], action: PermutationAction[G])(implicit builder: GrpGroup[Perm]): Grp[Perm]

}

object GrpPermutationAction {

  /** Return the smallest element of the domain moved by this group under the given action, or [[NNNone]]. */
  @inline final def smallestMovedPoint[G:PermutationAction](grp: Grp[G]): NNOption =
    if (grp.isTrivial) NNNone else {
      var mn = Int.MinValue
      grp.generators.foreach { g =>
        g.smallestMovedPoint match {
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
  @inline final def largestMovedPoint[G:PermutationAction](grp: Grp[G]): NNOption =
    if (grp.isTrivial) NNNone else {
      var mx = Int.MinValue
      grp.generators.foreach { g =>
        mx = spire.math.max(g.largestMovedPoint.getOrElse(Int.MinValue), mx)
      }
      if (mx == Int.MinValue) NNNone else NNOption(mx)
    }

  /** Returns an arbitrary element moved by this group under the given action, or [[NNNone]]. */
  @inline final def findMovedPoint[G:PermutationAction](grp: Grp[G]): NNOption =
    if (grp.isTrivial) NNNone else {
      grp.generators.foreach { g =>
        g.findMovedPoint match {
          case NNOption(i) => return NNOption(i)
          case _ =>
        }
      }
      NNNone
    }

  /** Tests if the point `i` is in the support of `g` under the given action. */
  @inline final def movesPoint[G:PermutationAction](grp: Grp[G], i: Int): Boolean =
    if (grp.isTrivial) false else grp.generators.exists(g => (i <|+| g) != i)

  /** Tests if the point `i` is in the support of `g` under the given action. */
  @inline final def movesAnyPoint[G:PermutationAction](grp: Grp[G]): Boolean =
    if (grp.isTrivial) false else grp.generators.exists(PermutationAction[G].movesAnyPoint)

  /** Number of non-negative integers moved by the permutations in this group. */
  @inline final def nMovedPoints[G:PermutationAction](grp: Grp[G]): Int =
    if (grp.isTrivial) 0 else movedPoints(grp).size

  /** Returns a bit set of all non-negative integers k that are moved by the action of this group,
    * i.e. `S = { k | exists g in this group s.t. k <|+| g != k }`.
    */
  @inline final def movedPoints[G:PermutationAction](grp: Grp[G]): Set[Int] =
    if (grp.isTrivial) Set.empty[Int] else {
      val b = metal.mutable.ResizableBitSet.empty
      grp.generators.foreach { g =>
        g.smallestMovedPoint match {
          case NNOption(lb) =>
            cforRange(lb until g.largestMovedPoint.get + 1) { i =>
              if ((i <|+| g) != i)
                b += i
            }
          case _ =>
        }
      }
      b.toScala
    }

}

class GrpPermutationActionSyntax[G](val lhs: Grp[G]) extends AnyVal {

  /** Find a group element whose permutation action is the same as the given element, if it exists, or returns Opt.empty. */
  def findSameAction[Q:PermutationAction](action: PermutationAction[G], q: Q)(implicit algos: GrpPermutationAction[G]): Opt[G]
  = algos.findSameAction(lhs, action, q)

  /** Find the kb of the given action, as a normal subgroup of this group. */
  def kernel(action: PermutationAction[G])(implicit algos: GrpPermutationAction[G]): Grp[G]
  = algos.kernel(lhs, action)

  /** Sequence of the group elements, ordered lexicographically by their images.
    * Returns a value only if the action is faithful. */
  def lexElements(action: PermutationAction[G])(implicit algos: GrpPermutationAction[G]): BigIndexedSeq[G]
  = algos.lexElements(lhs, action).get

  /** Returns the subgroup that fixes the given partition under the given action. */
  def fixingPartition(action: PermutationAction[G], partition: Partition)(implicit algos: GrpPermutationAction[G]): Grp[G]
  = algos.fixingPartition(lhs, action, partition)

  /** Returns the subgroup that stabilizes `b` by the given action. */
  def stabilizer(action: PermutationAction[G], b: Int)(implicit algos: GrpPermutationAction[G]): Grp[G]
  = algos.stabilizer(lhs, action, b)

  /** If the action of this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point,
    * and the associated transversal.
    */
  def someStabilizerTransversal(action: PermutationAction[G])(implicit algos: GrpPermutationAction[G]): Opt[(Grp[G], Transversal[G])]
  = algos.someStabilizerTransversal(lhs, action)

  /** Returns the subgroup that stabilizes `b` under the given action and the associated transversal. */
  def stabilizerTransversal(action: PermutationAction[G], b: Int)(implicit algos: GrpPermutationAction[G]): (Grp[G], Transversal[G])
  = algos.stabilizerTransversal(lhs, action, b)

  def pointwiseStabilizer(action: PermutationAction[G], set: Set[Int])(implicit algos: GrpPermutationAction[G]): Grp[G]
  = algos.pointwiseStabilizer(lhs, action, set)

  def pointwiseStabilizer(action: PermutationAction[G], points: Int*)(implicit algos: GrpPermutationAction[G]): Grp[G] =
    pointwiseStabilizer(action, scala.collection.immutable.BitSet(points: _*))

  def setwiseStabilizer(action: PermutationAction[G], set: Set[Int])(implicit algos: GrpPermutationAction[G]): Grp[G]
  = algos.setwiseStabilizer(lhs, action, set)

  def setwiseStabilizer(action: PermutationAction[G], points: Int *)(implicit algos: GrpPermutationAction[G]): Grp[G] =
    setwiseStabilizer(action, scala.collection.immutable.BitSet(points: _*))

  /*
  /** Finds an element of this group under `actionG` with the same image as `q` under `actionQ`, if it exists. */
  def find[Q:Eq:Group](actionG: PermutationAction[G], actionQ: PermutationAction[Q], q: Q)(implicit builder: PermutationActionGrpAlgos[G]): Opt[G]
  = builder.find(lhs, actionG, actionQ, q)

  /** Finds an element of this group under `actionG` with the same image as the given permutation, if it exists. */
  def find(actionG: PermutationAction[G], p: Perm)(implicit builder: PermutationActionGrpAlgos[G]): Opt[G]
  = builder.find(lhs, actionG, p)
*/

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
                                            (implicit algos: GrpPermutationAction[G]): Grp[G]
  = algos.subgroupFor(lhs, action, backtrackTest, predicate)

  /** Returns a sequence of domain elements such that no element of this group apart from
    * the identity fixes all the points in the sequence.
    * Returns a value if and only if the action is faithful.
    */
  def base(action: PermutationAction[G])(implicit algos: GrpPermutationAction[G]): Opt[Seq[Int]]
  = algos.base(lhs, action)

  def toPerm(action: PermutationAction[G])(implicit algosG: GrpPermutationAction[G], builder: GrpGroup[Perm]): Grp[Perm]
  = algosG.toPerm(lhs, action)

  /** Return the smallest element of the domain moved by this group under the given action, or [[NNNone]]. */
  def smallestMovedPoint(action: PermutationAction[G]): NNOption = GrpPermutationAction.smallestMovedPoint(lhs)(action)

  /** Return the largest element of the domain moved by this group under the given action, or [[NNNone]]. */
  def largestMovedPoint(action: PermutationAction[G]): NNOption = GrpPermutationAction.largestMovedPoint(lhs)(action)

  /** Returns an arbitrary element moved by this group under the given action, or [[NNNone]]. */
  def findMovedPoint(action: PermutationAction[G]): NNOption = GrpPermutationAction.findMovedPoint(lhs)(action)

  /** Tests if the point `i` is in the support of `g` under the given action. */
  def movesPoint(action: PermutationAction[G], i: Int): Boolean = GrpPermutationAction.movesPoint(lhs, i)(action)

  /** Tests if the group moves any point under the given action. */
  def movesAnyPoint(action: PermutationAction[G]): Boolean = GrpPermutationAction.movesAnyPoint(lhs)(action)

  /** Number of non-negative integers moved by the permutations in this group. */
  def nMovedPoints(action: PermutationAction[G]): Int = GrpPermutationAction.nMovedPoints(lhs)(action)

  /** Returns a bit set of all non-negative integers k that are moved by the action of this group,
    * i.e. `S = { k | exists g in this group s.t. k <|+| g != k }`.
    */
  def movedPoints(action: PermutationAction[G]): Set[Int] = GrpPermutationAction.movedPoints(lhs)(action)

}

class GrpPermSyntax(val lhs: Grp[Perm]) extends AnyVal {

  /** Sequence of the group elements, ordered lexicographically by their images. */
  def lexElements(implicit ev: GrpPermutationAction[Perm]): BigIndexedSeq[Perm] = ev.lexElements(lhs, Perm.algebra).get

  /** Returns the subgroup that fixes the given partition. */
  def fixingPartition(partition: Partition)(implicit ev: GrpPermutationAction[Perm]): Grp[Perm] =
    ev.fixingPartition(lhs, Perm.algebra, partition)

  /** Returns the subgroup that stabilizes `b`. */
  def stabilizer(b: Int)(implicit ev: GrpPermutationAction[Perm]): Grp[Perm] =
    ev.stabilizer(lhs, Perm.algebra, b)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point,
    * and the associated transversal.
    */
  def someStabilizerTransversal(implicit ev: GrpPermutationAction[Perm]): Opt[(Grp[Perm], Transversal[Perm])] =
    ev.someStabilizerTransversal(lhs, Perm.algebra)

  /** Returns the subgroup that stabilizes `b` and the associated transversal. */
  def stabilizerTransversal(b: Int)(implicit ev: GrpPermutationAction[Perm]): (Grp[Perm], Transversal[Perm]) =
    ev.stabilizerTransversal(lhs, Perm.algebra, b)

  def pointwiseStabilizer(set: Set[Int])(implicit ev: GrpPermutationAction[Perm]): Grp[Perm] =
    ev.pointwiseStabilizer(lhs, Perm.algebra, set)

  def pointwiseStabilizer(points: Int*)(implicit ev: GrpPermutationAction[Perm]): Grp[Perm] =
    pointwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  def setwiseStabilizer(set: Set[Int])(implicit ev: GrpPermutationAction[Perm]): Grp[Perm] =
    ev.setwiseStabilizer(lhs, Perm.algebra, set)

  def setwiseStabilizer(points: Int *)(implicit ev: GrpPermutationAction[Perm]): Grp[Perm] =
    setwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  /** Returns the subgroup for which `predicate` is satisfied; the test `backtrackTest` is used to
    * prune the search tree.
    *
    * @param backtrackTest Tests if a pair (preimage, image) is valid for an element of the subgroup. False
    *                      positives are allowed, but a false negative would incorrectly prune the tree.
    * @param predicate Tests if an element is member of the subgroup
    * @return the subgroup satisfying `predicate`
    */
  def subgroupFor(backtrackTest: (Int, Int) => Boolean, predicate: Perm => Boolean)
                 (implicit ev: GrpPermutationAction[Perm]): Grp[Perm] =
    ev.subgroupFor(lhs, Perm.algebra, backtrackTest, predicate)

  /** Returns a sequence of domain elements such that no element of this group apart from
    * the identity fixes all the points in the sequence.
    */
  def base(implicit ev: GrpPermutationAction[Perm]): Seq[Int] = ev.base(lhs, Perm.algebra).get

  /** Return the smallest element of the domain moved by this group, or [[NNNone]]. */
  def smallestMovedPoint: NNOption = GrpPermutationAction.smallestMovedPoint(lhs)

  /** Return the largest element of the domain moved by this group, or [[NNNone]]. */
  def largestMovedPoint: NNOption = GrpPermutationAction.largestMovedPoint(lhs)

  /** Returns an arbitrary element moved by this group, or [[NNNone]]. */
  def findMovedPoint: NNOption = GrpPermutationAction.findMovedPoint(lhs)

  /** Tests if the point `i` is in the support of `g`. */
  def movesPoint(i: Int): Boolean = GrpPermutationAction.movesPoint(lhs, i)

  /** Number of non-negative integers moved by the permutations in this group. */
  def nMovedPoints: Int = GrpPermutationAction.nMovedPoints(lhs)

  /** Returns a bit set of all non-negative integers k that are moved by the action of this group,
    * i.e. `S = { k | exists g in this group s.t. k <|+| g != k }`.
    */
  def movedPoints: Set[Int] = GrpPermutationAction.movedPoints(lhs)

}
