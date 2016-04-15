package net.alasc.prep

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationBuilder}
import net.alasc.domains.Partition
import net.alasc.finite._
import net.alasc.prep.chain.PGrpChainBuilder

abstract class PGrp[G] extends Grp[G] { lhs =>

  implicit def builder: PGrpBuilder[G]

  /** Singleton type of the representation used. */
  type R <: FaithfulPRep[G] with Singleton

  /** Permutation representation used to construct the group. */
  val pRep: R

  def lexElements: BigIndexedSeq[G]

  def fixingPartition(partition: Partition): Grp[G]

  def stabilizer: Opt[Grp[G]] = stabilizerTransversal match {
    case Opt((g, t)) => Opt(g)
    case _ => Opt.empty[Grp[G]]
  }

  def stabilizer(b: Int): Grp[G] = stabilizerTransversal(b)._1

  def stabilizerTransversal: Opt[(Grp[G], bsgs.Transversal[G])]

  def stabilizerTransversal(b: Int): (Grp[G], bsgs.Transversal[G])

  def pointwiseStabilizer(set: Set[Int]): Grp[G]

  def pointwiseStabilizer(points: Int*): Grp[G] =
    pointwiseStabilizer(Set(points: _*))

  def setwiseStabilizer(set: Set[Int]): Grp[G]

  def setwiseStabilizer(points: Int*): Grp[G] =
    setwiseStabilizer(Set(points: _*))

  def find[Q:Eq:PermutationBuilder](q: Q): Opt[G]

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

object PGrp {

  type In[R0 <: FaithfulPRep[G] with Singleton, G] = PGrp[G] { type R = R0 }

  class Algorithms(
    val randomOpt: Opt[Random] = Opt(Random),
    val baseChangeRecomputes: Boolean = false,
    val baseChangeConjugates: Boolean = true) {

    import bsgs._

    implicit val schreierSims: SchreierSims = randomOpt match {
      case Opt(random) => SchreierSims.randomized(random)
      case _ => SchreierSims.deterministic
    }

    implicit val baseSwap: BaseSwap = randomOpt match {
      case Opt(random) => BaseSwap.randomized(random)
      case _ => BaseSwap.deterministic
    }

    implicit val baseChange: BaseChange =
      if (baseChangeRecomputes)
        BaseChange.fromScratch
      else if (baseChangeConjugates)
        BaseChange.swapConjugation
      else
        BaseChange.swap

    implicit def pGrpChainBuilder[G:ClassTag:Eq:Group:PRepBuilder]: PGrpChainBuilder[G] =
      new chain.PGrpChainBuilder[G]

  }

  object deterministicNoSwap extends Algorithms(Opt.empty[Random], true, false)

  object deterministicNoConjugate extends Algorithms(Opt.empty[Random], false, false)

  object deterministic extends Algorithms(Opt.empty[Random], false, true)

  object default extends Algorithms(Opt(Random), false, true)

}
