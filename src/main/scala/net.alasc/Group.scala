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
  def action: Action[F]
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

  def fixing1[O](s: Seq[O]) = {
    val mapping = s.distinct.zipWithIndex.toMap
    val indices = s.map(mapping).toArray
    import Dom.ZeroBased._
    require_(s.size == actionDimension)
    val n = s.size
    def leaveInvariant(f: F): Boolean = {
      var i = 0
      while (i < n) {
        if (indices(act(f, i)) != indices(i))
          return false
        i += 1
      }
      return true
    }

    case class Test(remainingGroups: List[Array[Int]]) extends SubgroupSearchTest[F] {
      def test(baseImage: Dom, deltaP: Dom, action: Action[F], uPrevChain: List[TEntry[F]], transversal: Transversal[F]): SubgroupSearchTest[F] = {
        val group = remainingGroups.head
        if (indices(group(0)) != indices(baseImage))
          return null
        if (group.length > 1) {
          val uThisChain = transversal(deltaP) * new TChain(uPrevChain)
          var i = 1
          while (i < group.length) {
            val k = group(i)
            if (indices(k) != indices(uThisChain.action(k)))
              return null
            i += 1
          }
        }
        Test(remainingGroups.tail)
      }
    }

    val seqBase: List[Dom] = 
      s.zipWithIndex.sortBy(pair => mapping(pair._1)).map(pair => Dom._0(pair._2)).toList

    val orderedBSGS = bsgs.withBase(seqBase)

    val (reducedBSGS, groups) = orderedBSGS.removingRedundantBasePointsGrouped
    val groupArrays = groups.map(_.map(_._0).toArray)
    Group(reducedBSGS.subgroupSearch( leaveInvariant, Test(groupArrays) ).removingRedundantBasePoints)
  }

  def fixing[O](s: Seq[O]) = {
    val mapping = s.distinct.zipWithIndex.toMap
    val indices = s.map(mapping).toArray
    import Dom.ZeroBased._
    require_(s.size == actionDimension)
    val n = s.size
    def leaveInvariant(f: F): Boolean = {
      var i = 0
      while (i < n) {
        if (indices(act(f, i)) != indices(i))
          return false
        i += 1
      }
      return true
    }

    case class Test(remainingBase: List[Dom]) extends SubgroupSearchTest[F] {
      def test(baseImage: Dom, deltaP: Dom, act: Action[F], uPrevChain: List[TEntry[F]], transversal: Transversal[F]): SubgroupSearchTest[F] =
        (indices(remainingBase.head) == indices(baseImage)) match {
          case false => null
          case true => Test(remainingBase.tail)
        }
    }

    val seqBase: List[Dom] = 
      s.zipWithIndex.groupBy(_._1).toSeq.sortBy(_._2.length).flatMap(_._2).map(pair => Dom._0(pair._2)).toList
    println(seqBase)
    val orderedBSGS = bsgs.withBase(seqBase)

    val newBSGS =
      orderedBSGS.subgroupSearch( leaveInvariant, Test(orderedBSGS.base) ).removingRedundantBasePoints

    Group(newBSGS)
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

class GeneratorsGroup[F <: Finite[F]](val action: Action[F], val generators: Seq[F], prescribedBase: List[Dom] = Nil, bsgsOption: Option[BSGSChain[F]] = None)(implicit options: GroupOptions) extends LazyGroup[F](bsgsOption) {
  def computeBSGS = BSGSChain.deterministicSchreierSims(action, prescribedBase, generators.toList)
  override def conjugatedBy(f: F) = {
    val finv = f.inverse
    new GeneratorsGroup(action,
      generators.map(x => finv * x * f), 
      prescribedBase.map(beta => action(f, beta)), 
      bsgsOption.map(b => b.conjugatedBy(f)) )
  }
}

class GeneratorsAndOrderGroup[F <: Finite[F]](val action: Action[F], val generators: Seq[F], givenOrder: BigInt, prescribedBase: List[Dom] = Nil, bsgsOption: Option[BSGSChain[F]] = None)(implicit options: GroupOptions) extends LazyGroup[F](bsgsOption) {
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

class RandomElementsAndOrderGroup[F <: Finite[F]](val action: Action[F], randomElement: Random => F,  givenOrder: BigInt, prescribedBase: List[Dom] = Nil, bsgsOption: Option[BSGSChain[F]] = None)(implicit options: GroupOptions) extends LazyGroup[F](bsgsOption) {
  def generators = bsgs.strongGeneratingSet
  def computeBSGS = {
    assert(options.useRandomizedAlgorithms)
    BSGSChain.randomSchreierSims(action, prescribedBase, randomElement, givenOrder)
  }
}

object Group {
  def apply[F <: Finite[F]](bsgs: BSGSChain[F])(implicit myOptions: GroupOptions = GroupOptions.default): Group[F] = new ChainGroup(bsgs)

  def apply[F <: Finite[F]](generators: F*)(implicit myOptions: GroupOptions, actionBuilder: F => Action[F]): Group[F] = {
    assert(!generators.isEmpty)
    val action = actionBuilder(generators.head)
    new GeneratorsGroup(action, generators)
  }

  def apply[F <: Finite[F]](finiteGroup: FiniteGroup[F])(implicit myOptions: GroupOptions, actionBuilder: F => Action[F]): Group[F] =
    fromFiniteGroup(actionBuilder(finiteGroup.identity), finiteGroup)

  def fromFiniteGroup[F <: Finite[F]](action: Action[F], finiteGroup: FiniteGroup[F])(implicit myOptions: GroupOptions) =
    myOptions.useRandomizedAlgorithms match {
      case true =>
        new RandomElementsAndOrderGroup(action, finiteGroup.random(_), finiteGroup.order)
      case false =>
        new GeneratorsAndOrderGroup(action, finiteGroup.generators, finiteGroup.order)
    }

  def fromGenerators[F <: Finite[F]](
    action: Action[F],
    generators: List[F],
    prescribedBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = 
    new GeneratorsGroup(action, generators, prescribedBase)

  def fromGeneratorsAndOrder[F <: Finite[F]](
    action: Action[F],
    generators: List[F],
    order: BigInt,
    prescribedBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) =
    new GeneratorsAndOrderGroup(action, generators, order, prescribedBase)

  def fromRandomElementsAndOrder[F <: Finite[F]](
    action: Action[F],
    randomElement: Random => F,
    order: BigInt,
    prescribedBase: List[Dom] = Nil)(implicit myOptions: GroupOptions = GroupOptions.default) = 
    new RandomElementsAndOrderGroup(action, randomElement, order, prescribedBase)

  def fromBaseAndStrongGeneratingSet[F <: Finite[F]](
    action: Action[F],
    base: List[Dom],
    strongGeneratingSet: List[F])(implicit myOptions: GroupOptions = GroupOptions.default) = {
    val chain = BSGSChain.mutableFromBaseAndGeneratingSet(action, base, strongGeneratingSet)
    chain.makeImmutable
    new ChainGroup(chain)
  }
}
