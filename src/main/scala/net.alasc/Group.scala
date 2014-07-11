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
  implicit var default = GroupOptions(
    useRandomizedAlgorithms = true,
    randomGenerator = scala.util.Random,
    transversalBuilder = TransversalExplicit,
    orbitBuilder = OrbitSet,
    baseChangeStrategy = BaseSwapAndConjugation)
}

/*
## `Group`: a base class for groups

This base class implements the main CGT algorithms used in `alasc`.
*/

abstract class Group[F <: Finite[F]](implicit val options: GroupOptions = GroupOptions.default) extends FiniteGroup[F] with FiniteGroupImpl[F] {
  containingGroup =>
  def action: PRepr[F]
  def identity = action.identity

  def act(f: F, k: Dom) = action(f, k)
  def actionDomain = action.domain
  def actionDimension = action.dimension

  def withOptions(newOptions: GroupOptions) =
    Group.fromBaseAndStrongGeneratingSet(action, bsgs.base, bsgs.strongGeneratingSet)(newOptions)

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil): Transversal[F] =
    options.transversalBuilder.empty(newBeta, action).updated(genSet, genSet)

  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil): Orbit[F] =
    options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)

  def bsgs: BSGSChain[F]
  def order = bsgs.order
  def random(implicit gen: Random) = bsgs.random(gen)
  def elements = new Iterable[F] {
    override def size = (if (order.isValidInt) order.toInt else
      throw new IllegalArgumentException("Order of the group is " + order.toString + " which is not a valid iterator size of type Int.")
    )
    def iterator = bsgs.elements
  }
  def generators: Seq[F]
  def contains(f: F) = bsgs.contains(f)

  def canEqual(any: Any): Boolean = any match {
    case that: Group[_] => action == that.action
    case _ => false
  }

  override def equals(any: Any): Boolean = any match {
    case that: Group[F] => (that canEqual this) && generators.forall(that.contains) && that.generators.forall(contains)
    case _ => false
  }

  override def toString = "Group of order " + order

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
  def generatorsAccordingTo(subgroups: List[Group[F]], givenGenerators: List[F] = Nil): List[F] =
    subgroups match {
      case Nil =>
        require(givenGenerators.forall(this.contains(_)))
        val newGenerators = bsgs.strongGeneratingSetGiven(givenGenerators)
        val newGivenGenerators = (newGenerators.toSet diff givenGenerators.toSet).toList
        newGivenGenerators ++ givenGenerators
      case hd :: tl =>
        val interSubgroup = this intersection hd
        val newGenerators = interSubgroup.bsgs.withBase(bsgs.base).
          strongGeneratingSetGiven(givenGenerators.filter(interSubgroup.contains))
        val newGivenGenerators = (newGenerators.toSet diff givenGenerators.toSet).toList
        generatorsAccordingTo(tl, newGivenGenerators ++ givenGenerators)
    }

  def isSubgroup(potentialSubgroup: Group[F]) = potentialSubgroup.generators.forall(contains)

  def supunion(that: Group[F]): Group[F] = Group.fromGenerators(action, (generators ++ that.generators).toList)

  def intersection(that: Group[F]): Group[F] = {
    val newBSGS = bsgs.intersection(that.bsgs)
    Group(newBSGS)
  }

  def &(that: Group[F]): Group[F] = intersection(that)

  def conjugatedBy(f: F): Group[F] = {
    val newBSGS = bsgs.conjugatedBy(f)
    Group(newBSGS)
  }

  def stabilizer(k: Dom) = bsgs match {
    case terminal: BSGSTerminal[F] => this
    case node: BSGSNode[F] => Group(node.withHeadBasePoint(k).tail)
  }

  case class FixingTest(pointSetsToTest: List[Array[Int]], indices: Array[Int]) extends SubgroupSearchTest[F] {
    import Dom.ZeroBased._
    def test(baseImage: Dom, deltaP: Dom, action: PRepr[F], uPrev: F, transversal: Transversal[F]): SubgroupSearchTest[F] = {
      val pointSet = pointSetsToTest.head
      if (indices(pointSet(0)) != indices(baseImage))
        return null
      if (pointSet.length > 1) {
        val uThis = transversal(deltaP).u * uPrev
        var i = 1
        while (i < pointSet.length) {
          val k = pointSet(i)
          if (indices(k) != indices(act(uThis, k)))
            return null
          i += 1
        }
      }
      FixingTest(pointSetsToTest.tail, indices)
    }
  }

  def fixing[O](s: Seq[O], usePointSets: Boolean = true, reorderBase: Boolean = true): Group[F] = {
    val seqValueToInt = s.distinct.zipWithIndex.toMap
    val seqInteger = s.map(seqValueToInt).toArray
    import Dom.ZeroBased._
    val n = s.size
    require_(n == actionDimension)
    def leaveInvariant(f: F): Boolean = {
      var i = 0
      while (i < n) {
        if (seqInteger(act(f, i)) != seqInteger(i))
          return false
        i += 1
      }
      return true
    }
    val bsgsToUse = reorderBase match {
      case true =>
        val newBase: List[Dom] = // reorder the base such that rare elements of s are tested first
          s.zipWithIndex.groupBy(_._1).toSeq // pair each element with its index, and group by value
            .sortBy(_._2.length).flatMap(_._2) // sort by frequence of value
            .map(pair => Dom._0(pair._2)).toList // get index and transform as domain
        val newOptions = options.copy(baseChangeStrategy = BaseFromScratch)
        bsgs.withBase(newBase)(newOptions).removingRedundantBasePoints
      case false =>
        bsgs
    }
    val pointSets: List[Array[Int]] = usePointSets match {
      case false =>
        bsgsToUse.base.map(b => Array(b._0)).toList
      case true =>
        bsgsToUse.basePointGroups.map(_.map(_._0).toArray).toList
    }
    val test = FixingTest(pointSets, seqInteger)
    Group(bsgsToUse.subgroupSearch( leaveInvariant, test).removingRedundantBasePoints)
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
  def rightCosetMinimalRepresentative(f: F): F = bsgs.withFullLexicographicBase.
    rightCosetMinimalRepresentativeUsingBSGSBase(f)(DomOrdering)

/*
The method `cosetIterator` enumerate coset representatives for the Subgroup `bySubgroup`.
The algorithm can probably be improved a lot.
*/ 
  def cosetIterator(bySubgroup: Group[F]): Iterator[F] = {
    // TODO: prove that you do not need a particular basis
    // very inefficient
    val bySubgroupLexicographicBase = Group(bySubgroup.bsgs.withFullLexicographicBase)
    assert(bySubgroup.generators.forall(contains))
    for (e <- elements.iterator if bySubgroupLexicographicBase.rightCosetMinimalRepresentative(e) == e)
    yield e
  }
}

class ChainGroup[F <: Finite[F]](val bsgs: BSGSChain[F])(implicit options: GroupOptions) extends Group[F] {
  def action = bsgs.action
  def generators = bsgs.strongGeneratingSet
}

abstract class LazyGroup[F <: Finite[F]](bsgsOption: Option[BSGSChain[F]]) extends Group[F] {
  @volatile var knownBSGS: Option[BSGSChain[F]] = bsgsOption
  def computeBSGS: BSGSChain[F]
  def bsgs = knownBSGS match {
    case None =>
      this.synchronized {
        knownBSGS match {
          case None =>
            val b = computeBSGS
            knownBSGS = Some(b)
            b
          case Some(b) => b
        }
      }
    case Some(b) => b
  }
}

class GeneratorsGroup[F <: Finite[F]](val action: PRepr[F], val generators: Seq[F], prescribedBase: List[Dom] = Nil, bsgsOption: Option[BSGSChain[F]] = None)(implicit options: GroupOptions) extends LazyGroup[F](bsgsOption) {
  def computeBSGS = BSGSChain.deterministicSchreierSims(action, prescribedBase, generators.toList)
  override def conjugatedBy(f: F) = {
    val finv = f.inverse
    new GeneratorsGroup(action,
      generators.map(x => finv * x * f), 
      prescribedBase.map(beta => action(f, beta)), 
      bsgsOption.map(b => b.conjugatedBy(f)) )
  }
}

class GeneratorsAndOrderGroup[F <: Finite[F]](val action: PRepr[F], val generators: Seq[F], givenOrder: BigInt, prescribedBase: List[Dom] = Nil, bsgsOption: Option[BSGSChain[F]] = None)(implicit options: GroupOptions) extends LazyGroup[F](bsgsOption) {
  def computeBSGS = options.useRandomizedAlgorithms match {
    case true =>
      val bag = RandomBag(generators, action.identity, max(10, generators.length), 50,
        options.randomGenerator)
      BSGSChain.randomSchreierSims(action, prescribedBase, bag.random(_), givenOrder)
    case false =>
      val b = BSGSChain.deterministicSchreierSims(action, prescribedBase, generators.toList)
      assert(b.order == givenOrder)
      b
  }
  override def conjugatedBy(f: F) = {
    val finv = f.inverse
    new GeneratorsAndOrderGroup(action,
      generators.map(x => finv * x * f),
      givenOrder,
      prescribedBase.map(beta => action(f, beta)), 
      bsgsOption.map(b => b.conjugatedBy(f)) )
  }
}

class RandomElementsAndOrderGroup[F <: Finite[F]](val action: PRepr[F], randomElement: Random => F,  givenOrder: BigInt, prescribedBase: List[Dom] = Nil, bsgsOption: Option[BSGSChain[F]] = None)(implicit options: GroupOptions) extends LazyGroup[F](bsgsOption) {
  def generators = bsgs.strongGeneratingSet
  def computeBSGS = {
    assert(options.useRandomizedAlgorithms)
    BSGSChain.randomSchreierSims(action, prescribedBase, randomElement, givenOrder)
  }
}

object Group {
  def apply[F <: Finite[F]](bsgs: BSGSChain[F])(implicit myOptions: GroupOptions = GroupOptions.default): Group[F] = new ChainGroup(bsgs)

  def apply[F <: Finite[F]](generators: F*)(implicit myOptions: GroupOptions, actionBuilder: F => PRepr[F]): Group[F] = {
    assert(!generators.isEmpty)
    val action = actionBuilder(generators.head)
    new GeneratorsGroup(action, generators)
  }

  def apply[F <: Finite[F]](finiteGroup: FiniteGroup[F])(implicit myOptions: GroupOptions, actionBuilder: F => PRepr[F]): Group[F] =
    fromFiniteGroup(actionBuilder(finiteGroup.identity), finiteGroup)

  def fromFiniteGroup[F <: Finite[F]](action: PRepr[F], finiteGroup: FiniteGroup[F])(implicit myOptions: GroupOptions) =
    myOptions.useRandomizedAlgorithms match {
      case true =>
        new RandomElementsAndOrderGroup(action, finiteGroup.random(_), finiteGroup.order)
      case false =>
        new GeneratorsAndOrderGroup(action, finiteGroup.generators, finiteGroup.order)
    }

  def fromGenerators[F <: Finite[F]](
    action: PRepr[F],
    generators: List[F],
    prescribedBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = 
    new GeneratorsGroup(action, generators, prescribedBase)

  def fromGeneratorsAndOrder[F <: Finite[F]](
    action: PRepr[F],
    generators: List[F],
    order: BigInt,
    prescribedBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) =
    new GeneratorsAndOrderGroup(action, generators, order, prescribedBase)

  def fromRandomElementsAndOrder[F <: Finite[F]](
    action: PRepr[F],
    randomElement: Random => F,
    order: BigInt,
    prescribedBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = 
    new RandomElementsAndOrderGroup(action, randomElement, order, prescribedBase)

  def fromBaseAndStrongGeneratingSet[F <: Finite[F]](
    action: PRepr[F],
    base: List[Dom],
    strongGeneratingSet: List[F])(implicit myOptions: GroupOptions = GroupOptions.default) = {
    val chain = BSGSChain.mutableFromBaseAndGeneratingSet(action, base, strongGeneratingSet)
    chain.makeImmutable
    new ChainGroup(chain)
  }
}
