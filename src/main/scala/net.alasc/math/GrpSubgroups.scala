package net.alasc
package math

import scala.language.implicitConversions
import scala.reflect.ClassTag

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.math.bsgs._
import net.alasc.math.bsgs.algorithms._
import net.alasc.math.guide.BaseGuideSeq
import net.alasc.syntax.all._
import net.alasc.util._

class GrpSubgroups[G](val lhs: Grp[G]) {
  import lhs.representation
  implicit def gClassTag: ClassTag[G] = lhs.algorithms.gClassTag
  implicit def algorithms: BasicAlgorithms[G] = lhs.algorithms
  implicit def representations: Representations[G] = lhs.representations
  implicit def algebra: FiniteGroup[G] = lhs.algebra
  def union(rhs: Grp[G]): Grp[G] = lhs.lattice.join(lhs, rhs)
  def intersect(rhs: Grp[G]): Grp[G] = lhs.lattice.meet(lhs, rhs)
  def hasSubgroup(rhs: Grp[G]): Boolean = lhs.lattice.gteqv(lhs, rhs)
  def hasProperSubgroup(rhs: Grp[G]): Boolean = lhs.lattice.gt(lhs, rhs)
  def isSubgroupOf(rhs: Grp[G]): Boolean = lhs.lattice.lteqv(lhs, rhs)
  def isProperSubgroupOf(rhs: Grp[G]): Boolean = lhs.lattice.lt(lhs, rhs)

  def &(rhs: Grp[G]) = intersect(rhs)
  def |(rhs: Grp[G]) = union(rhs)

  def /(rhs: Grp[G]): LeftCosets[G] = {
    require(rhs.generators.forall(lhs.contains(_)))
    new LeftCosets(lhs, rhs)
  }
  def \(rhs: Grp[G]): RightCosets[G] = {
    require(lhs.generators.forall(rhs.contains(_)))
    new RightCosets(lhs, rhs)
  }
  def fixingPartition(partition: Domain#Partition, rp: Representation[G]): Grp[G] =
    Grp.fromChain(FixingPartition.fixingPartition(lhs.chain(rp, FixingPartition.baseGuide(partition)), partition), RefSome(rp))

  def fixingPartition(partition: Domain#Partition)(implicit prp: PermutationRepresentations[G]): Grp[G] =
    fixingPartition(partition, prp.forSize(partition.size))

  def stabilizer(rp: Representation[G]): RefOption[(Grp[G], Transversal[G])] = {
    lhs match {
      case grp: GrpConjugated[G] => grp.originalChain match {
        case node: Node[G] if node.action == rp.action =>
          return RefSome(stabilizer(node.action.actr(node.beta, grp.conjugatedBy.g), rp))
        case node: Node[G] =>
        case _: Term[G] => return RefNone
      }
      case _ =>
    }
    lhs.chainIfComputed match {
      case RefOption(node: Node[G]) if node.action == rp.action => RefSome(stabilizer(node.beta, rp))
      case RefOption(_: Term[G]) => RefNone
      case _ if lhs.isTrivial => RefNone
      case _ => lhs.withComputedChain(rp).stabilizer(rp)
    }
  }

  def stabilizer(b: Int, rp: Representation[G]): (Grp[G], Transversal[G]) = {
    implicit val algebra = lhs.algorithms.algebra
    lhs match {
      case grp: GrpConjugated[G] =>
        grp.originalChain match {
          case node: Node[G] if node.action == rp.action =>
            import grp.conjugatedBy.gInv
            implicit def action = node.action
            val a = b <|+| gInv
            if (node.inOrbit(a)) {
              val ip = node.uPair(a)
              val newConjugatedBy = ip |+| grp.conjugatedBy
              return (GrpConjugated(grp.algorithms, node.next.generators, grp.representation, node.next, newConjugatedBy), ConjugatedTransversal(node, newConjugatedBy))
            } else if (node.isFixed(a))
              return (grp, Transversal.empty(b))
          case term: Term[G] => return (grp, Transversal.empty[G](b))
          case _ =>
        }
      case grp =>
        grp.chainIfComputed match {
          case RefOption(node: Node[G]) if node.action == rp.action =>
            implicit def action = node.action
            if (node.inOrbit(b)) {
              val ip = node.uPair(b)
              return (GrpConjugated(grp.algorithms, node.next.generators, grp.representation, node.next, ip), ConjugatedTransversal(node, ip))
            } else if (node.isFixed(b))
              return (grp, Transversal.empty(b))
          case RefOption(term: Term[G]) => return (grp, Transversal.empty[G](b))
          case _ =>
        }
    }
    val newChain = lhs.chain(rp, BaseGuideSeq(Seq(b)))
    val (nextChain, transversal) = newChain.detach(b)
    (Grp.fromChain(nextChain, RefSome(rp)), transversal)
  }

  def stabilizer(b: Int)(implicit prp: PermutationRepresentations[G]): (Grp[G], Transversal[G]) = {
    val rp = if (b < representation.size) representation else prp.forSize(b + 1)
    stabilizer(b, rp)
  }

  def pointwiseStabilizer(set: Set[Int], rp: Representation[G]): Grp[G] =
    Grp.fromChain(Stabilizer.pointwiseStabilizer(lhs.chain(rp, Stabilizer.baseGuide(set)), set))

  def pointwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return lhs
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    pointwiseStabilizer(set, rp)
  }

  def setwiseStabilizer(set: Set[Int], rp: Representation[G]): Grp[G] =
    Grp.fromChain(Stabilizer.setwiseStabilizer(lhs.chain(rp, Stabilizer.baseGuide(set)), set))
  def setwiseStabilizer(points: Int*)(implicit prp: PermutationRepresentations[G]): Grp[G] = {
    if (points.size == 0) return lhs
    val set = Set(points:_*)
    val maxSet = set.max
    val rp = if (maxSet < representation.size) representation else prp.forSize(maxSet + 1)
    setwiseStabilizer(set, rp)
  }

  /** Returns the subgroup for which `predicate` is satisfied; the test `backtrackTest` is used to
    * prune the search tree.
    *
  * @param backtrackTest Tests if a pair (preimage, image) is valid for an element of the subgroup. False
  *                      positives are allowed, but a false negative would incorrectly prune the tree.
  * @param rp Representation to use for `backtrackTest`
  * @param predicate Tests if an element is member of the subgroup
  * @return the subgroup satisfying `predicate`
  */
  def subgroupFor(backtrackTest: (Int, Int) => Boolean, rp: Representation[G], predicate: G => Boolean): Grp[G] = {
    object ThisTest extends SubgroupTest {
      def test(b: Int, orbitImage: Int, currentG: G, node: Node[G])(
        implicit action: FaithfulPermutationAction[G]): RefOption[SubgroupTest[G]] =
      if (backtrackTest(node.beta, orbitImage)) RefSome(this) else RefNone
    }
     Grp.fromChain(algorithms.subgroupSearch(lhs.chain(rp), predicate, ThisTest).toChain)
  }
}
