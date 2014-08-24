package net.alasc.math

import scala.util.Random

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._
import bsgs._
import algorithms._

/** User-friendly representation of a group internally using a base and strong generating set data structure.
  *
  * Can be constructed from any finite group with a faithful permutation action.
  */
class Grp[P](
  val defaultAction: PermutationAction[P],
  val algorithms: BasicAlgorithms[P],
  val givenGenerators: RefOption[Iterable[P]] = RefNone,
  var knownOrder: RefOption[BigInt] = RefNone,
  var knownChain: RefOption[Chain[P]] = RefNone,
  val givenRandomElement: Option[Random => P] = None)(implicit algebra: FiniteGroup[P]) { lhs =>
  // TODO conjugatedBy
  override def toString = generators.mkString("Grp(", ", ", ")") + (if (knownOrder.nonEmpty || knownChain.nonEmpty) s" of order ${order}" else "")
  def chain: Chain[P] = knownChain.getOrElse {
    implicit def action = defaultAction
    if (givenGenerators.nonEmpty && knownOrder.nonEmpty)
      algorithms.completeChainFromGeneratorsAndOrder(givenGenerators.get, knownOrder.get).toChain
    else if (givenRandomElement.nonEmpty && knownOrder.nonEmpty && algorithms.isInstanceOf[SchreierSimsRandomized[P]])
      algorithms.asInstanceOf[SchreierSimsRandomized[P]].randomizedSchreierSims(givenRandomElement.get, knownOrder.get).toChain
    else if (givenGenerators.nonEmpty)
      algorithms.completeChainFromGenerators(givenGenerators.get).toChain
    else
      sys.error("Group cannot be represented from the given information.")
  }
  def order: BigInt = if (knownOrder.nonEmpty) knownOrder.get else {
    knownOrder = RefSome(chain.order)
    knownOrder.get
  }
  def generators = givenGenerators.getOrElse(chain.generators)
  def randomElement(random: Random): P = givenRandomElement.fold(chain.randomElement(random))(_(random))
  def &(rhs: Grp[P]) = intersect(rhs)
  def |(rhs: Grp[P]) = union(rhs)
  def fixing(seq: Seq[Any])(implicit action: PermutationAction[P]): Grp[P] =
    Grp.fromChain(algorithms.fixing(chain, seq)(action).toChain)
  def intersect(rhs: Grp[P]): Grp[P] = Grp.fromChain(algorithms.intersection(chain, rhs.chain)(defaultAction).toChain)(algebra, defaultAction)
  def union(rhs: Grp[P]): Grp[P] = if (knownChain.nonEmpty || givenGenerators.isEmpty) { // TODO: something more clever if the two groups commute
    // TODO: when rhs has been computed but not lhs
    val mutableChain = algorithms.mutableCopyWithAction(chain, defaultAction)
    algorithms.insertGenerators(mutableChain, rhs.generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain)(algebra, defaultAction)
  } else {
    val mutableChain = algorithms.incompleteChainWithGenerators(lhs.generators ++ rhs.generators)(defaultAction)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain)(algebra, defaultAction)
  }
}

object Grp {
  def defaultAlg[P](implicit algebra: FiniteGroup[P]) = BasicAlgorithms.randomized(Random)
  def fromChain[P](chain: Chain[P])(implicit algebra: FiniteGroup[P], action: PermutationAction[P]) =
    new Grp[P](defaultAction = action, algorithms = defaultAlg[P], knownChain = RefSome(chain))
  def apply[P](generators: P*)(implicit algebra: FiniteGroup[P], action: PermutationAction[P]) = {
    new Grp[P](defaultAction = action, algorithms = defaultAlg[P], givenGenerators = RefSome(generators))
  }
  def fromSubgroup[S, P](subgroup: S)(implicit algebra: FiniteGroup[P], sg: Subgroup[S, P], action: PermutationAction[P]) = {
    val alg = defaultAlg[P]
    fromChain(alg.completeChainFromGeneratorsAndOrder(subgroup.generators, subgroup.order).toChain)
  }
  implicit def GrpSubgroup[P](implicit algebra: FiniteGroup[P]): Subgroup[Grp[P], P] = new GrpSubgroup[P]
}

class GrpSubgroup[P](implicit val algebra: FiniteGroup[P]) extends Subgroup[Grp[P], P] {
  def elements(grp: Grp[P]) = grp.chain.elements
  def generators(grp: Grp[P]) = grp.generators
  def order(grp: Grp[P]) = grp.order
  def randomElement(grp: Grp[P], random: Random) = grp.randomElement(random)
  override def contains(grp: Grp[P], g: P) = grp.chain.contains(g)
}
