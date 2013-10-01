package net.alasc

import scala.util.Random

/** All-around group class. Constructs a BSGS behind the scenes as needed.
  * 
  * The group can be constructed by using the following algorithms (listed
  * below by decreasing priority):
  * 
  * - the random Schreier-Sims algorithm if all the following elements are provided:
  *   - the order of the group,
  *   - a random number generator (by default, [[scala.util.Random]] will be used),
  *   - a way to generate random group elements, using either:
  *     - a random element function,
  *     - creating a [[RandomBag] from the list of generators.
  * - the deterministic Schreier-Sims algorihm if the group generators are known.
  */
abstract class Group[F <: FiniteElement[F]](
  val identity: F,
  val action: Action[F]
) extends FiniteGroup[F] with GroupBSGSData[F] with GroupBSGSElements[F] with GroupBSGSSifting[F] with GroupBSGSMutable[F] {
  containingGroup =>

  def random = scala.util.Random // FIXME: add as parameter
  def transversalBuilder: TransversalBuilder = TransversalExplicit // FIXME: add as parameter
  def orbitBuilder: OrbitBuilder = OrbitSet

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil) =
    transversalBuilder.empty(newBeta, identity, action).updated(genSet, genSet)
  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil) =
    orbitBuilder.empty(newBeta, action).updated(genSet, genSet)

  require_(action.faithful)

  def bsgs: BSGSChain

  private[alasc] object BSGSChain extends BSGSMutableCompanion {
  }

  private[alasc] sealed abstract class BSGSChain extends BSGSData with BSGSElements with BSGSSifting with BSGSMutable {
    def isTerminal: Boolean
    def tail: BSGSChain

    def isImmutable: Boolean

    def transversal: Transversal[F]
    def strongGeneratingSet: List[F]
  }

  private[alasc] final class BSGSNode(
    var transversal: Transversal[F],
    var strongGeneratingSet: List[F],
    var tail: BSGSChain,
    var isImmutable: Boolean = true) extends BSGSChain {
    def isTerminal = false
  }

  private[alasc] final class BSGSTerminal extends BSGSChain {
    def isTerminal = true
    def isImmutable = true
    def strongGeneratingSet = List.empty[F]
    def transversal = throw new IllegalArgumentException("Cannot get transversal of BSGS chain terminal.")
    def tail = throw new IllegalArgumentException("Cannot get tail of BSGS chain terminal.")
  }

  def order = bsgs.order
  def randomElement(gen: Random) = bsgs.randomElement(gen)
  def elements = bsgs.elements
  def generators = bsgs.strongGeneratingSet
  def compatible(f: F) = identity.compatible(f)
  def contains(f: F) = bsgs.contains(f)

  class Subgroup(val subBSGS: BSGSChain) extends FiniteGroup[F] {
    def order = subBSGS.order
    def randomElement(gen: Random) = subBSGS.randomElement(gen)
    def generators = subBSGS.strongGeneratingSet
    def elements = subBSGS.elements
    def identity = containingGroup.identity
    def compatible(f: F) = identity.compatible(f)
    def contains(f: F) = subBSGS.contains(f)
  }
}
