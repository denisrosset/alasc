/*
# Implementation of algorithms for finite and permutation groups #

This part defines a `Group` base class which implements computational group theory
algorithms for finite and permutation groups.

Finite groups are handled by using a faithful permutation action.
*/

package net.alasc

import scala.util.Random
import scala.math.max

/*
## Options for computational group theory algorithms 

Several variants of CGT algorithms are implemented in `alasc`, and are
selected using the options below, that can be provided when constructing a
`Group` instance. The options are as follows: 

- `useRandomizedAlgorithms`: selects between randomized and deterministic algorithms
- `randomGenerator`: the `scala.util.Random` generator used by the algorithms,
  can be used to run tests using a fixed seed as to have pseudo-random results
  that can be reproduced
- `transversalBuilder`: builder for the kind of `Transversal` to
  be employed. For the first version of `alasc`, only `TransversalExplicit` is
  implemented.
- `orbitBuilder`: builder for the kind of `Orbit` to be employed. For the first
  version of `alasc`, only `OrbitSet` is implemented.
- `baseChangeStrategy`: algorithm used to change between bases (see below).
*/

case class GroupOptions(
  val useRandomizedAlgorithms: Boolean,
  val randomGenerator: scala.util.Random,
  val transversalBuilder: TransversalBuilder,
  val orbitBuilder: OrbitBuilder,
  val baseChangeStrategy: BaseChangeStrategy) { }

/*

### Base change strategies

- `BaseSwapOnly` only uses swapping
- `BaseSwapAndConjugation` uses both swapping and conjugation
- `BaseFromScratch` always rebuild the base using a Schreier-Sims algorithm
  variant
*/

sealed abstract class BaseChangeStrategy { }
case object BaseSwapOnly extends BaseChangeStrategy { }
case object BaseSwapAndConjugation extends BaseChangeStrategy { }
case object BaseFromScratch extends BaseChangeStrategy { }

/*
### Default options

The default options use the randomized variants as they are the fastest. Note that
randomized variants are never used when the probability of failure is non-zero, so
our software always returns exact results.
*/

object GroupOptions {
  var default = GroupOptions(
    useRandomizedAlgorithms = true,
    randomGenerator = scala.util.Random,
    transversalBuilder = TransversalExplicit,
    orbitBuilder = OrbitSet,
    baseChangeStrategy = BaseSwapAndConjugation)
}

/*
## `Group`: a base class for groups

This base class implements the main CGT algorithms used in `alasc`. 
Two subclasses of `Group` are provided below:

- the `PGroup` subclass represents a permutation group using a BSGS chain,
- the `FGroup` subclass represents a finite group using a BSGS chain by internally
  using a faithful action to perform the CGT algorithms.
*/

trait BaseImageTest {
  def apply(baseImage: Dom): (Boolean, BaseImageTest)
}

object TrivialBaseImageTest extends BaseImageTest {
  def apply(baseImage: Dom) = (true, this)
}

abstract class Group[F <: Finite[F]] extends FiniteGroup[F] with FiniteGroupLike[F] {
  containingGroup =>

  val identity: F
  implicit val options: GroupOptions
  val action: Action[F]

  def act(f: F, k: Dom): Dom
  def actionDomain: Iterable[Dom]
  def actionDimension: Int

  def withOptions(newOptions: GroupOptions): Group[F]

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil): Transversal[F]
  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil): Orbit[F]

  def bsgs: BSGSChain[F]
  def order = bsgs.order
  def random(implicit gen: Random) = bsgs.random(gen)
  def elements = new Iterable[F] {
    override def size = (if (order.isValidInt) order.toInt else
      throw new IllegalArgumentException("Order of the group is " + order.toString + " which is not a valid iterator size of type Int.")
    )
    def iterator = bsgs.elements
  }
  def generators = bsgs.strongGeneratingSet
  def contains(f: F) = bsgs.contains(f)
  def subgroup = new Subgroup(bsgs)

  def canEqual(any: Any): Boolean = any match {
    case that: Group[_] => action == that.action
    case _ => false
  }

  override def equals(any: Any): Boolean = any match {
    case that: Group[F] => (that canEqual this) && generators.forall(that.contains) && that.generators.forall(contains)
    case _ => false
  }

  object Subgroup {
    def apply(myGenerators: F*) = {
      require_(myGenerators.forall(f => containingGroup.contains(f)))
      require_(myGenerators.forall(f => !f.isIdentity))
      val subBSGS = BSGSChain.deterministicSchreierSims(action, Nil, myGenerators.toList)
      new Subgroup(subBSGS)
    }

    def fromGeneratorsAndOrder(myGenerators: List[F], myOrder: BigInt) = {
      if (options.useRandomizedAlgorithms) {
        val bag = RandomBag(myGenerators, identity, max(10, myGenerators.length), 50, options.randomGenerator)
        val subBSGS = BSGSChain.randomSchreierSims(action, Nil, bag.randomElement, myOrder, myGenerators)
        new Subgroup(subBSGS)
      } else apply(myGenerators:_*)
    }

    def fromRandomAndOrder(myRandomElement: Random => F, myOrder: BigInt) = {
      require_(options.useRandomizedAlgorithms == true)
      val subBSGS = BSGSChain.randomSchreierSims(action, Nil, myRandomElement, myOrder)
      new Subgroup(subBSGS)
    }
  }

  class Subgroup(val subBSGS: BSGSChain[F]) extends FiniteGroup[F] {
    override def toString = "Subgroup of order " + order

    def canEqual(other: Any): Boolean =
      other.isInstanceOf[Subgroup] && (other.asInstanceOf[Subgroup].group eq group)
    override def equals(other: Any) = other match {
      case that: Subgroup => (this eq that) || ((that canEqual this) && ((this intersection that).order == order))
      case _ => false
    }
    def group = containingGroup
    def order = subBSGS.order
    def random(implicit gen: Random) = subBSGS.random(gen)
/*
The method `generators` returns the strong generating set of the underlying
stabilizer chain, while the method `generatorsAccordingTo` returns a strong
generating set according to the `subgroups` sequence and pre-existing `givenGenerators`.

In this last case, `subgroups` is a list of `n` `Subgroup`s.
The returned list will be composed of:
`addGenerators ++ newGenerators(n-1) ++ ... ++ newGenerators(0) ++ givenGenerators`
so that `newGenerators(j)` are (new) generators of `this intersection subgroups(j)`,
`addGenerators` are possible additional generators so that the returned sequence
is a strong generating set for `this` subgroup.

This function is used to provide a readable list of generators for this subgroup of 
a group, by considering first the intersection of this subgroup with 
remarkable subgroups of the underlying group.
*/
    def generators = subBSGS.strongGeneratingSet
    def generatorsAccordingTo(subgroups: List[Subgroup], givenGenerators: List[F] = Nil): List[F] =
      subgroups match {
        case Nil =>
          require(givenGenerators.forall(this.contains(_)))
          val newGenerators = subBSGS.strongGeneratingSetGiven(givenGenerators)
          val newGivenGenerators = (newGenerators.toSet diff givenGenerators.toSet).toList
          newGivenGenerators ++ givenGenerators
        case hd :: tl =>
          val interSubgroup = this intersection hd
          val newGenerators = interSubgroup.subBSGS.withBase(subBSGS.base).
            strongGeneratingSetGiven(givenGenerators.filter(interSubgroup.contains))
          val newGivenGenerators = (newGenerators.toSet diff givenGenerators.toSet).toList
          generatorsAccordingTo(tl, newGivenGenerators ++ givenGenerators)
      }
    def elements = new Iterable[F] {
      override def size = (if (subBSGS.order.isValidInt) subBSGS.order.toInt else
        throw new IllegalArgumentException("Order of the group is " + subBSGS.order.toString + " which is not a valid iterator size of type Int.")
      )
      def iterator = subBSGS.elements
    }
    def identity = containingGroup.identity
    def contains(f: F) = subBSGS.contains(f)
    def supunion(that: Subgroup): Subgroup = 
      Subgroup(generators ++ that.generators:_*)
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
      require_(s.size == actionDimension)
      def leaveInvariant(f: F) =
        (0 until s.size).forall( i => s(act(f, Dom._0(i))._0) == s(i) )

      case class Test(remainingBase: List[Dom]) extends BaseImageTest {
        def apply(baseImage: Dom) = {
          val takeIt = s(remainingBase.head._0) == s(baseImage._0)
          (takeIt, Test(remainingBase.tail))
        }
      }
      val fullBase = subBSGS.fullLexicographicBase
      val newBSGS = subBSGS.withBase(fullBase).subgroupSearch( leaveInvariant, Test(fullBase) ).removingRedundantBasePoints
      new Subgroup(newBSGS)
    }
/*
#### Coset algorithms

The method `rightCosetMinimalRepresentative` finds the minimal lexicographic
representative of the right coset \\( S f \\), where \\( S \\) is the current
subgroup of the main group \\( G \\) and \\( f \in G \\).

It retrieves the element \\( s \in S \\) such that \\( f' = s f \\) is
lexicographically minimal according to `domOrdering`, and returns \\( f' = s f \\).

Lexicographic order is a total order according to the following rule:

\\[ d < e \Leftrightarrow (b_1^{d'} < b_1^{e'}) \text{ or } 
(b_1^{d'} = b_1^{e'} \text{ and } b_2^{d'} < b_2^{e'}) \text{ or ...} \\]

- where \\( d' = \phi(d), e' = \phi(e) \\) and \\( \phi \\) is the permutation action
  of the group,
- where the order \\( < \\) of domain elements is given by `domOrdering` and
  the indices used in the lexicographic ordering are given by the base of
  the subgroup. The subgroup should have a complete base (b1, b2 ...) = (1, 2, 3 ...)
  except if you know what you are doing.

(see also the comment above for `rightCosetMinimalRepresentativeUsingBSGSBase`).
*/
    def rightCosetMinimalRepresentative(f: F): F = subBSGS.withFullLexicographicBase.
      rightCosetMinimalRepresentativeUsingBSGSBase(f)(DomOrdering)

/*
The method `cosetIterator` enumerate coset representatives for the Subgroup `bySubgroup`.
The algorithm can probably be improved a lot.
*/ 
    def cosetIterator(bySubgroup: Subgroup): Iterator[F] = {
      // TODO: prove that you do not need a particular basis
      val bySubgroupLexicographicBase = new Subgroup(bySubgroup.subBSGS.withFullLexicographicBase)
      assert(bySubgroup.generators.forall(Subgroup.this.contains(_)))
      for (e <- elements.iterator if bySubgroupLexicographicBase.rightCosetMinimalRepresentative(e) == e)
      yield e
    }
  }
}

abstract class PGroup[P <: Permuting[P]](val identity: P, val options: GroupOptions = GroupOptions.default) extends Group[P] with PermutingGroup[P] with PermutingGroupLike[P] {

  val action = TrivialAction(identity)

  def act(p: P, k: Dom) = p.image(k)
  def actionDomain = identity.domain
  def actionDimension = degree

  def withOptions(newOptions: GroupOptions) =
    PGroup.fromBSGS(identity, bsgs.base, bsgs.strongGeneratingSet)(newOptions)

  def makeTransversal(newBeta: Dom, genSet: List[P] = Nil) =
    options.transversalBuilder.empty(newBeta, action).updated(genSet, genSet)

  def makeOrbit(newBeta: Dom, genSet: List[P] = Nil) =
    options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)

  def degree = identity.size
  def explicitSift(product: P, remaining: Perm, chain: BSGSChain[P]): (P, Perm) = chain.isTerminal match {
    case true => (product, remaining)
    case false => {
      val b = remaining.image(chain.beta)
      if (!chain.transversal.isDefinedAt(b))
        (product, remaining)
      else {
        val nextRemaining = remaining * chain.transversal(b).uinv.toPerm
        val nextProduct = chain.transversal(b).u * product
        explicitSift(nextProduct, nextRemaining, chain.tail)
      }
    }
  }
  def fromExplicit(perm: Perm): Option[P] = explicitSift(identity, perm, bsgs) match {
    case (p, remaining) if remaining.isIdentity => Some(p)
    case _ => None
  }
}

object PGroup {
  def apply[P <: Permuting[P]](elements: P*)(implicit myOptions: GroupOptions = GroupOptions.default) = {
    assert(!elements.isEmpty)
    val identity = elements.find(_.isIdentity) match {
      case Some(id) => id
      case None => elements.head * elements.head.inverse
    }
    val generators = elements.filterNot(_.isIdentity).toList
    fromGenerators(identity, generators, Nil)
  }

  def fromPermutingGroup[P <: Permuting[P]](
    permGroup: PermutingGroup[P],
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = myOptions.useRandomizedAlgorithms match {
    case true =>
      fromRandomElementsAndOrder(permGroup.identity, permGroup.random(_), permGroup.order, myBase)
    case false =>
      fromGenerators(permGroup.identity, permGroup.generators.toList, myBase)
  }

  def fromGenerators[P <: Permuting[P]](
    myIdentity: P,
    myGenerators: List[P],
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity) {
    lazy val bsgs = BSGSChain.deterministicSchreierSims(TrivialAction(myIdentity), myBase, myGenerators)
  }

  def fromGeneratorsAndOrder[P <: Permuting[P]](
    myIdentity: P,
    myGenerators: List[P],
    myOrder: BigInt,
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity, myOptions) {
    lazy val bsgs = myOptions.useRandomizedAlgorithms match {
      case true => {
        val bag = RandomBag(myGenerators, myIdentity, max(10, myGenerators.length), 50, myOptions.randomGenerator)
        BSGSChain.randomSchreierSims(action, myBase, bag.randomElement, myOrder)
      }
      case false => BSGSChain.deterministicSchreierSims(TrivialAction(myIdentity), myBase, myGenerators)
    }
  }

  def fromRandomElementsAndOrder[P <: Permuting[P]](
    myIdentity: P,
    myRandomElement: Random => P,
    myOrder: BigInt,
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity, myOptions) {
    require_(myOptions.useRandomizedAlgorithms == true)
    lazy val bsgs = BSGSChain.randomSchreierSims(TrivialAction(myIdentity), myBase, myRandomElement, myOrder)
  }

  def fromBSGS[P <: Permuting[P]](
    myIdentity: P,
    myBase: List[Dom],
    myStrongGeneratingSet: List[P])(implicit myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity, myOptions) {
    val bsgs = {
      val chain = BSGSChain.mutableFromBaseAndGeneratingSet(TrivialAction(myIdentity), myBase, myStrongGeneratingSet)
      chain.makeImmutable
      chain
    }
  }
}

abstract class FGroup[F <: Finite[F]](
  val identity: F,
  val action: Action[F],
  val options: GroupOptions = GroupOptions.default) extends Group[F] {
  require_(action.faithful)

  def act(f: F, k: Dom) = action(f, k)
  def actionDomain = action.domain
  def actionDimension = action.dimension

  def withOptions(newOptions: GroupOptions) =
    FGroup.fromBSGS(identity, action, bsgs.base, bsgs.strongGeneratingSet)(newOptions)

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil) =
    options.transversalBuilder.empty(newBeta, action).updated(genSet, genSet)

  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil) =
    options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)
}

object FGroup {
  def fromFiniteGroup[F <: Finite[F]](
    finiteGroup: FiniteGroup[F],
    myAction: Action[F],
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = myOptions.useRandomizedAlgorithms match {
    case true =>
      fromRandomElementsAndOrder(finiteGroup.identity, myAction, finiteGroup.random(_), finiteGroup.order, myBase)
    case false =>
      fromGenerators(finiteGroup.identity, myAction, finiteGroup.generators.toList, myBase)
  }

  def fromGenerators[F <: Finite[F]](
    myIdentity: F,
    myAction: Action[F],
    myGenerators: List[F],
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    lazy val bsgs = BSGSChain.deterministicSchreierSims(myAction, myBase, myGenerators)
  }

  def fromGeneratorsAndOrder[F <: Finite[F]](
    myIdentity: F,
    myAction: Action[F],
    myGenerators: List[F],
    myOrder: BigInt,
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    lazy val bsgs = myOptions.useRandomizedAlgorithms match {
      case true => {
        val bag = RandomBag(myGenerators, myIdentity, max(10, myGenerators.length), 50, myOptions.randomGenerator)
        BSGSChain.randomSchreierSims(myAction, myBase, bag.random(_), myOrder)
      }
      case false => BSGSChain.deterministicSchreierSims(myAction, myBase, myGenerators)
    }
  }

  def fromRandomElementsAndOrder[F <: Finite[F]](
    myIdentity: F,
    myAction: Action[F],
    myRandomElement: Random => F,
    myOrder: BigInt,
    myBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    require_(myOptions.useRandomizedAlgorithms == true)
    lazy val bsgs = BSGSChain.randomSchreierSims(myAction, myBase, myRandomElement, myOrder)
  }

  def fromBSGS[F <: Finite[F]](
    myIdentity: F,
    myAction: Action[F],
    myBase: List[Dom],
    myStrongGeneratingSet: List[F])(implicit myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    val bsgs = {
      val chain = BSGSChain.mutableFromBaseAndGeneratingSet(myAction, myBase, myStrongGeneratingSet)
      chain.makeImmutable
      chain
    }
  }
}
