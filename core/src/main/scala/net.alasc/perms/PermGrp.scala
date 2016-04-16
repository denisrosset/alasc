package net.alasc.perms

import net.alasc.algebra.{BigIndexedSeq, Permutation}
import net.alasc.domains.Partition
import net.alasc.finite.Grp
import net.alasc.prep.bsgs
import spire.util.Opt

abstract class PermGrp[G] extends Grp[G] { lhs =>

  implicit def builder: PermGrpBuilder[G]

  implicit def permutation: Permutation[G]

  def group = permutation

  def equ = permutation

  /** Sequence of the group elements, ordered lexicographically by their images. */
  def lexElements: BigIndexedSeq[G]

  /** Returns the subgroup that fixes the given partition. */
  def fixingPartition(partition: Partition): Grp[G] = builder.fixingPartition(lhs, partition)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point. */
  def someStabilizer: Opt[Grp[G]] = builder.stabilizer(lhs)

  /** Returns the subgroup that stabilizes `b`. */
  def stabilizer(b: Int): Grp[G] = builder.stabilizer(lhs, b)

  /** If this group is trivial, returns Opt.empty, otherwise, returns a subgroup that stabilizes some point,
    * and the associated transversal.
    */
  def someStabilizerTransversal: Opt[(Grp[G], bsgs.Transversal[G])] = builder.someStabilizerTransversal(lhs)

  /** Returns the subgroup that stabilizes `b` and the associated transversal. */
  def stabilizerTransversal(b: Int): (Grp[G], bsgs.Transversal[G]) = builder.stabilizerTransversal(lhs, b)

  def pointwiseStabilizer(set: Set[Int]): Grp[G] = builder.pointwiseStabilizer(set)

  def pointwiseStabilizer(points: Int*): Grp[G] = pointwiseStabilizer(scala.collection.immutable.BitSet(points: _*))

  def setwiseStabilizer(set: Set[Int]): Grp[G] = builder.setwiseStabilizer(set)

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
  def subgroupFor(backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): Grp[G]

}
