package net.alasc

import scala.util.Random

sealed abstract class BaseChangeStrategy { }

case object BaseSwapOnly extends BaseChangeStrategy { }

case object BaseSwapAndConjugation extends BaseChangeStrategy { }

case object BaseFromScratch extends BaseChangeStrategy { }

case class GroupOptions(
  val useRandomizedAlgorithms: Boolean,
  val randomGenerator: scala.util.Random,
  val transversalBuilder: TransversalBuilder,
  val orbitBuilder: OrbitBuilder,
  val baseChangeStrategy: BaseChangeStrategy) { }

object GroupOptions {
  def default = GroupOptions(
    useRandomizedAlgorithms = true,
    randomGenerator = scala.util.Random,
    transversalBuilder = TransversalExplicit,
    orbitBuilder = OrbitSet,
    baseChangeStrategy = BaseSwapAndConjugation)
}

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
  val action: Action[F],
  val options: GroupOptions = GroupOptions.default
) extends FiniteGroup[F] with GroupBSGSData[F] with GroupBSGSElements[F] with GroupBSGSSifting[F] with GroupBSGSMutable[F] with GroupBSGSSearch[F] with GroupBSGSBase[F] with GroupBSGSCheck[F] {
  containingGroup =>

  def withOptions(newOptions: GroupOptions) =
    new GroupFromBSGS(identity, action, bsgs.base, bsgs.strongGeneratingSet, newOptions)

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil) =
    options.transversalBuilder.empty(newBeta, identity, action).updated(genSet, genSet)

  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil) =
    options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)

  require_(action.faithful)

  def bsgs: BSGSChain

  object BSGSChain extends BSGSMutableCompanion {
  }

  sealed abstract class BSGSChain extends BSGSData with BSGSElements with BSGSSifting with BSGSMutable with BSGSSearch with BSGSBase with BSGSCheck {
    def isTerminal: Boolean
    def tail: BSGSChain

    def isImmutable: Boolean

    def transversal: Transversal[F]
    def strongGeneratingSet: List[F]
  }

  final class BSGSNode(
    var transversal: Transversal[F],
    var strongGeneratingSet: List[F],
    var tail: BSGSChain,
    var isImmutable: Boolean = true) extends BSGSChain {
    def isTerminal = false
  }

 final class BSGSTerminal extends BSGSChain {
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
    // def intersection(that: Subgroup): Subgroup
    // def &(that: Subgroup) = intersection(that)
    // def conjugatedBy(f: F): Subgroup
    // def isSubgroup(potentialSubgroup: Subgroup) = this.intersection(potentialSubgroup).order == potentialSubgroup.order
    //    def setStabilizer(set: Set[Dom]): BSGSChain
  }

  /** Returns subgroup fixing a given sequence.
    * 
    * @param s   Sequence to be fixed by the subgroup.
    * 
    * @return The subgroup fixing s.
    */
  /*
   def fixing[O](s: Seq[O]): Subgroup = {

   def leaveInvariant(a: ActionElement[F, Perm]) =
   base.list.map(d => s(a.image(d)._0)).sameElements(s)

   object Test extends BaseImageTest {
   def apply(baseImage: Dom) = {
   val takeIt = s(base.list.head._0) == s(baseImage._0)
   (takeIt, Test(Base(base.list.tail)))
   }
   }
   val subgroupBSGS = bsgs.subgroupSearch( leaveInvariant, Test(groupBase) )
   new Group(faithfulAction, identity, knownBSGS = Some(subgroupBSGS))
   }
   */
}

import scala.language.higherKinds

object Group { // : ClassTag
  def apply[E <: PermElement[E]](elements: E*) = {
    assert(!elements.isEmpty)
    val identity = elements.find(_.isIdentity) match {
      case Some(id) => id
      case None => elements.head * elements.head.inverse
    }
    val generators = elements.filterNot(_.isIdentity).toList
    new GroupFromGenerators(identity, TrivialAction(identity), generators, Nil)
  }
}
