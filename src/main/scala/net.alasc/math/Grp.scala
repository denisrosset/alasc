package net.alasc.math

import scala.util.Random

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._
import bsgs._
import algorithms._

class Grp[P](
  val defaultAction: PermutationAction[P],
  val algorithms: BasicAlgorithms[P],
  val givenGenerators: RefOption[Iterable[P]] = RefNone,
  var knownOrder: RefOption[BigInt] = RefNone,
  var knownChain: RefOption[Chain[P]] = RefNone,
  val givenRandomElement: Option[Random => P] = None)(implicit algebra: FiniteGroup[P]) { lhs =>
  implicit def action = defaultAction
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
  def intersect(rhs: Grp[P]): Grp[P] = Grp(algorithms.intersection(chain, rhs.chain).toChain)
  def union(rhs: Grp[P]): Grp[P] = if (knownChain.nonEmpty || givenGenerators.isEmpty) { // TODO: something more clever if the two groups commute
    // TODO: when rhs has been computed but not lhs
    val mutableChain = algorithms.mutableCopyWithAction(chain, action)
    algorithms.insertGenerators(mutableChain, rhs.generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp(mutableChain.toChain)
  } else {
    val mutableChain = algorithms.incompleteChainWithGenerators(lhs.generators ++ rhs.generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp(mutableChain.toChain)
  }
}

object Grp {
  def defaultAlg[P](implicit algebra: FiniteGroup[P]) = BasicAlgorithms.randomized(Random)
  def apply[P](chain: Chain[P])(implicit algebra: FiniteGroup[P], action: PermutationAction[P]) =
    new Grp[P](defaultAction = action, algorithms = defaultAlg[P], knownChain = RefSome(chain))
  def apply[P](generators: P*)(implicit algebra: FiniteGroup[P], action: PermutationAction[P]) = {
    new Grp[P](defaultAction = action, algorithms = defaultAlg[P], givenGenerators = RefSome(generators))
  }
  implicit def GrpSubgroup[P](implicit algebra: FiniteGroup[P]): Subgroup[Grp[P], P] = new GrpSubgroup[P]
}

class GrpSubgroup[P](implicit val algebra: FiniteGroup[P]) extends Subgroup[Grp[P], P] {
  def elements(grp: Grp[P]) = grp.chain.elements
  def generators(grp: Grp[P]) = grp.generators
  def order(grp: Grp[P]) = grp.order
  def randomElement(grp: Grp[P], random: Random) = grp.randomElement(random)
}
