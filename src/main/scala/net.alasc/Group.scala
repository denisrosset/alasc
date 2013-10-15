package net.alasc

import scala.util.Random
import scala.math.max

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
  var default = GroupOptions(
    useRandomizedAlgorithms = true,
    randomGenerator = scala.util.Random,
    transversalBuilder = TransversalExplicit,
    orbitBuilder = OrbitSet,
    baseChangeStrategy = BaseSwapAndConjugation)
}

/** All-around group class. Constructs a BSGS behind the scene. */
abstract class Group[F <: FiniteElement[F]](
  val identity: F,
  val action: Action[F],
  val options: GroupOptions = GroupOptions.default
) extends FiniteGroup[F] with GroupBSGSData[F] with GroupBSGSElements[F] with GroupBSGSSifting[F] with GroupBSGSMutable[F] with GroupBSGSSearch[F] with GroupBSGSBase[F] with GroupBSGSCheck[F] with GroupCoset[F] {
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

  sealed abstract class BSGSChain extends BSGSData with BSGSElements with BSGSSifting with BSGSMutable with BSGSSearch with BSGSBase with BSGSCheck with BSGSCoset {
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
  def subgroup = new Subgroup(bsgs)

  object Subgroup {
    def apply(myGenerators: F*) = {
      require_(myGenerators.forall(f => containingGroup.contains(f)))
      require_(myGenerators.forall(f => !f.isIdentity))
      val subBSGS = BSGSChain.deterministicSchreierSims(bsgs.base, myGenerators.toList)
      new Subgroup(subBSGS)
    }

    def fromGeneratorsAndOrder(myGenerators: List[F], myOrder: BigInt) = {
      if (options.useRandomizedAlgorithms) {
        val bag = RandomBag(myGenerators, identity, max(10, myGenerators.length), 50, options.randomGenerator)
        val subBSGS = BSGSChain.randomSchreierSims(bsgs.base, bag.randomElement, myOrder)
        new Subgroup(subBSGS)
      } else apply(myGenerators:_*)
    }

    def fromRandomAndOrder(myRandomElement: Random => F, myOrder: BigInt) = {
      require_(options.useRandomizedAlgorithms == true)
      val subBSGS = BSGSChain.randomSchreierSims(bsgs.base, myRandomElement, myOrder)
      new Subgroup(subBSGS)
    }
  }

  class Subgroup(val subBSGS: BSGSChain) extends FiniteGroup[F] with SubgroupCoset {
    def order = subBSGS.order
    def randomElement(gen: Random) = subBSGS.randomElement(gen)
    def generators = subBSGS.strongGeneratingSet
    def elements = subBSGS.elements
    def identity = containingGroup.identity
    def compatible(f: F) = identity.compatible(f)
    def contains(f: F) = subBSGS.contains(f)

    def intersection(that: Subgroup): Subgroup = {
      val newBSGS = subBSGS.intersection(that.subBSGS)
      new Subgroup(newBSGS)
    }
    def &(that: Subgroup) = intersection(that)
    def conjugatedBy(f: F): Subgroup = {
      val newBSGS = subBSGS.conjugatedBy(f)
      new Subgroup(newBSGS)
    }
    def isSubgroup(potentialSubgroup: Subgroup) = potentialSubgroup.subBSGS.strongGeneratingSet.forall(g => Subgroup.this.contains(g))
    def stabilizer(k: Dom) = subBSGS.isTerminal match {
      case true => this
      case false => new Subgroup(subBSGS.withHeadBasePoint(k).tail)
    }

    /** Returns subgroup fixing a given sequence. */
    def fixing[O](s: Seq[O]) = {

      require_(s.size == action.dimension)
      def leaveInvariant(f: F) =
        (0 until s.size).forall( i => s(action(f, Dom._0(i))._0) == s(i) )

      val groupBase = bsgs.base

      case class Test(remainingBase: List[Dom]) extends BaseImageTest {
        def apply(baseImage: Dom) = {
          val takeIt = s(remainingBase.head._0) == s(baseImage._0)
          (takeIt, Test(remainingBase.tail))
        }
      }
      val newBSGS = subBSGS.subgroupSearch( leaveInvariant, Test(subBSGS.base) )
      new Subgroup(newBSGS)
    }
  }
}

object Group {
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
