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

abstract class Group[F <: FiniteElement[F]] extends FiniteGroup[F] {
  containingGroup =>

  val identity: F
  val options: GroupOptions
  val action: Action[F]

  def act(f: F, k: Dom): Dom
  def actionDomain: Iterable[Dom]
  def actionDimension: Int

  def withOptions(newOptions: GroupOptions): Group[F]

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil): Transversal[F]
  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil): Orbit[F]

  def bsgs: BSGSChain

/*
### Method to check the group consistency
*/
  def check = bsgs.check

/*
### `BSGSChain`: base class for the BSGS chain used by the `Group`
*/
  sealed abstract class BSGSChain {
    chain =>
/*
#### Chain node data
*/
    def isTerminal: Boolean

    def tail: BSGSChain

    def transversal: Transversal[F]
    def strongGeneratingSet: List[F]

/*
#### General data 
*/
    def beta: Dom = transversal.beta

    final def base: List[Dom] = isTerminal match {
      case true => Nil
      case false => beta :: tail.base
    }

    final def length: Int = isTerminal match {
      case true => 0
      case false => tail.length + 1
    }

    final def order: BigInt = isTerminal match {
      case true => BigInt(1)
      case false => if(transversal.size == 1) tail.order else transversal.size * tail.order
    }

    final def transversals: List[Transversal[F]] = isTerminal match {
      case true => Nil
      case false => transversal :: tail.transversals
    }

    def iterator = new Iterator[BSGSChain] {
      private var current: BSGSChain = chain
      def hasNext = !current.isTerminal
      def next = {
        if (hasNext) {
          val result = current
          current = result.tail
          result
        } else Iterator.empty.next
      }
    }

/*
#### Consistency checks of the BSGS chain

The method `check` throws an exception if an inconsistency is found.
*/

    def check = {
      assert(base.toSet.size == base.size)
      checkFixBase(Nil)
      checkNode
    }

    def checkFixBase(partialBase: List[Dom]): Unit = this match {
      case node: BSGSNode => {
        for (WithInverse(u, uinv) <- transversal.valuesIterator; b <- partialBase)
          assert(act(u, b) == b)
        for (g <- strongGeneratingSet; b <- partialBase)
          assert(act(g, b) == b)
        tail.checkFixBase(beta :: partialBase)
      }
      case _ => { }
    }

    def checkNode: Unit = this match {
      case node: BSGSNode => {
        assert(strongGeneratingSet.toSet.size == strongGeneratingSet.size) // no duplicates
        assert(tail.strongGeneratingSet.toSet.subsetOf(strongGeneratingSet.toSet))
        for ((b, WithInverse(u, uinv)) <- transversal) {
          assert(act(u, beta) == b)
          assert(act(uinv, b) == beta)
        }
        val o = makeOrbit(beta, strongGeneratingSet)
        assert(o.orbit == transversal.orbit)
        tail.checkNode
      }
      case _ => { }
    }

/*
#### Enumeration or generation of elements
*/

    def elements: Iterator[F] = this.isTerminal match {
      case true => Iterator(identity)
      case false => for {
        rest <- tail.elements
        b <- transversal.keysIterator
      } yield rest * transversal(b).u
    }

    def randomElement(gen: scala.util.Random): F = this.isTerminal match {
      case true => identity
      case false => tail.randomElement(gen) * transversal.randomElement(gen)
    }

/*
#### Sifting through the chain
*/

    case class SiftResult[F <: FiniteElement[F]](val transversalIndices: List[Dom], val remaining: F) {
      def prepend(b: Dom) = SiftResult(b :: transversalIndices, remaining)
    }

    def contains(f: F): Boolean = basicSift(f).remaining.isIdentity

    def basicSift(f: F): SiftResult[F] = this.isTerminal match {
      case true => SiftResult(Nil, f)
      case false => {
        val b = act(f, beta)
        if (!transversal.isDefinedAt(b))
          SiftResult(Nil, f)
        else {
          val nextF = f * transversal(b).uinv
          val SiftResult(transversalIndices, remaining) = tail.basicSift(nextF)
          SiftResult(b :: transversalIndices, remaining)
        }
      }
    }
/*
#### Base change algorithms
*/
    def conjugatedBy(f: F): BSGSChain = if (f.isIdentity) this else conjugatedBy(f, f.inverse)
    def conjugatedBy(f: F, finv: F): BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode =>
        new BSGSNode(transversal.conjugatedBy(f, finv), strongGeneratingSet.map(x => finv*x*f), tail.conjugatedBy(f, finv))
    }

    def removingRedundantBasePoints: BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode if transversal.size == 1 => tail.removingRedundantBasePoints
      case node: BSGSNode => {
        val newTail = tail.removingRedundantBasePoints
        if (newTail eq tail)
          this
        else
          new BSGSNode(transversal, strongGeneratingSet, newTail)
      }
    }
    /** Changes the current base. The base of the returned BSGSChain will start
      * with newBase, but can be longer if needed. No redundant base points will be 
      * kept in these additional points.
      */
    def withBase(newBase: List[Dom]): BSGSChain = options.baseChangeStrategy match {
      case BaseSwapOnly => withBaseNoConjugation(newBase)
      case BaseSwapAndConjugation => withBaseConjugation(newBase)
      case BaseFromScratch => withBaseFromScratch(newBase)
    }

    def fullLexicographicBase: List[Dom] = (1 to actionDimension).map(Dom._1(_)).toList

    def withFullLexicographicBase: BSGSChain = withBase(fullLexicographicBase)
    // TODO: this computes twice the new base
    def withLexicographicBase: BSGSChain = withBase(lexicographicBase())

    @annotation.tailrec final def findFirstLexicographicBasePoint(start: Dom = Dom._1(1)): Option[Dom] =
      (start._1 > actionDimension) match {
        case true => None
        case false => {
          strongGeneratingSet.exists(g => act(g, start) != start) match {
            case true => Some(start)
            case false => findFirstLexicographicBasePoint(Dom._1(start._1 + 1))
          }
        }
      }

    def lexicographicTail: BSGSChain = {
      if (beta._1 == actionDimension)
        new BSGSTerminal
      else {
        val nextBeta = beta.next
        tail match {
          case terminal: BSGSTerminal =>
            new BSGSNode(makeTransversal(nextBeta), Nil, terminal, true)
          case node: BSGSNode =>
            node.withHeadBasePoint(nextBeta)
        }
      }
    }

    def lexicographicBase(start: Dom = Dom._1(1)): List[Dom] = {
      val basePoint = findFirstLexicographicBasePoint(start)
      basePoint match {
        case None => Nil
        case Some(beta) => beta :: withHeadBasePoint(beta).tail.lexicographicBase(Dom._1(beta._1 + 1))
      }
    }

    def withBaseFromScratch(newBase: List[Dom]): BSGSChain = options.useRandomizedAlgorithms match {
      case true =>
        BSGSChain.randomSchreierSims(newBase, randomElement, order)
      case false =>
        BSGSChain.deterministicSchreierSims(newBase, generators)
    }

    def withBaseConjugation(newBase: List[Dom]): BSGSChain = {
      val (newChain, f, finv) = withBaseConjugationHelper(newBase, identity, identity)
      newChain.conjugatedBy(f)
    }
    def withBaseConjugationHelper(newBase: List[Dom], f: F, finv: F): (BSGSChain, F, F) = newBase match {
      case hd :: tl => {
        val alpha = act(finv, hd)
        if (!isTerminal) {
          if (!base.contains(alpha) && transversal.isDefinedAt(alpha)) {
            val (newTail, newF, newFinv) =
              tail.withBaseConjugationHelper(tl, transversal(alpha).u*f, finv*transversal(alpha).uinv)
            val newStrongGeneratingSet = (strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
            val newNode = new BSGSNode(transversal, newStrongGeneratingSet, newTail)
            return ((newNode, newF, newFinv))
          }
        }
        val swappedNode = withHeadBasePoint(alpha)
        val (newTail, newF, newFinv) = swappedNode.tail.withBaseConjugationHelper(tl, f, finv)
        val newStrongGeneratingSet = (swappedNode.strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
        val newNode = new BSGSNode(swappedNode.transversal, newStrongGeneratingSet, newTail)
        ((newNode, newF, newFinv))
      }
      case Nil => (removingRedundantBasePoints, f, finv)
    }

    def withBaseNoConjugation(newBase: List[Dom]): BSGSChain = newBase match {
      case hd :: tl => {
        val newHead = withHeadBasePoint(hd)
        val newTail = newHead.tail.withBaseNoConjugation(tl)
        val newStrongGeneratingSet = 
          (newHead.strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
        new BSGSNode(newHead.transversal, newStrongGeneratingSet, newTail)
      }
      case Nil => removingRedundantBasePoints
    }

    /** Called on a BSGSChain containing basePoint, returns
      * a BSGSChain with basePoint at head.
      */
    def putExistingBasePointInHead(basePoint: Dom): BSGSChain = {
      if (beta == basePoint)
        return this
      val newTail = tail.putExistingBasePointInHead(basePoint)
      val newStrongGeneratingSet = (strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
      val beforeSwap = new BSGSNode(transversal, newStrongGeneratingSet,
        newTail)
      beforeSwap.baseSwap
    }

    def withHeadBasePoint(basePoint: Dom): BSGSChain = {
      val withPoint = if (base.contains(basePoint)) this else insertBasePoint(basePoint)
      withPoint.putExistingBasePointInHead(basePoint)
    }

    /** Returns an updated BSGSChain with the new basePoint inserted.
      * 
      * The current BSGSChain must not contains basePoint
      */
    def insertBasePoint(basePoint: Dom): BSGSChain = this match {
      case terminal: BSGSTerminal =>
        new BSGSNode(makeTransversal(basePoint), Nil, terminal)
      case node: BSGSNode => {
        val orbit = makeOrbit(basePoint, strongGeneratingSet)
        if (orbit.size > 1)
          new BSGSNode(transversal, strongGeneratingSet, tail.insertBasePoint(basePoint))
        else
          new BSGSNode(makeTransversal(basePoint), strongGeneratingSet, this)
      }
    }

    def baseSwap: BSGSChain = options.useRandomizedAlgorithms match {
      case true => randomizedBaseSwap(options.randomGenerator)
      case false => deterministicBaseSwap
    }

    /** Deterministic base swap.
      * 
      * @return BSGS group with first two base elements swapped.
      * 
      * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
      * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
      */
    def deterministicBaseSwap: BSGSChain = {
      require_(isInstanceOf[BSGSNode])
      val node = asInstanceOf[BSGSNode]
      require_(!tail.isTerminal)

      var tList = tail.strongGeneratingSet.filter( t => act(t, tail.beta) == tail.beta )
      val beta1 = tail.beta
      var gammaSet = transversal.keysIterator.filter( k => k != beta && k != beta1 ).toSet
      var betaT = makeOrbit(beta, tList)
      var betaT1 = makeOrbit(beta1, tList)
      var beta1Gi = makeOrbit(beta1).updated(strongGeneratingSet, strongGeneratingSet)
      val siz = (transversal.size*tail.transversal.size)/beta1Gi.size
      def exploreGamma {
        val gamma = gammaSet.head
        val (x, xinv) = (transversal(gamma).u, transversal(gamma).uinv)
        if (!tail.transversal.isDefinedAt(act(xinv, beta1))) {
          var o = makeOrbit(gamma, tList)
          gammaSet = gammaSet diff o.orbit
        } else {
          val y = tail.transversal(act(xinv, beta1)).u
          val yx = y*x
          if(!betaT.contains(act(yx, beta))) {
            tList = yx :: tList
            betaT = betaT.updated(List(yx), tList)
            betaT1 = betaT1.updated(List(yx), tList)
            gammaSet = gammaSet diff betaT.orbit
          }
        }
      }
      while (betaT.size < siz) {
        assert(!gammaSet.isEmpty)
        exploreGamma
      }
      val nS = (tList diff strongGeneratingSet) ::: strongGeneratingSet
      val nTrv = makeTransversal(beta1, nS)
      val nS1 = nS.filter( s => act(s, beta1) == beta1 )
      val nTrv1 = makeTransversal(beta, nS1)
      new BSGSNode(nTrv, nS, new BSGSNode(nTrv1, nS1, tail.tail))
    }

    def randomizedBaseSwap(r: scala.util.Random): BSGSChain = {
      require_(isInstanceOf[BSGSNode])
      val node = asInstanceOf[BSGSNode]
      require_(!tail.isTerminal)
      val nBeta = tail.beta
      val nBeta1 = beta
      var nS = strongGeneratingSet
      var nS1 = strongGeneratingSet.filter(g => act(g, nBeta) == nBeta)
      var nTrv = makeTransversal(nBeta, nS)
      var nTrv1 = makeTransversal(nBeta1, nS1)
      val siz = (transversal.size*tail.transversal.size)/nTrv.size
      while (nTrv1.size < siz) {
        val g = chain.randomElement(r)
        val h = g * nTrv(act(g, nBeta)).uinv
        if (!nTrv1.isDefinedAt(act(h, nBeta1))) {
          nS = h :: nS
          nS1 = h :: nS1
          nTrv1 = nTrv1.updated(List(h), nS1)
          nTrv = nTrv.updated(List(h), nS)
        }
      }
      new BSGSNode(nTrv, nS, new BSGSNode(nTrv1, nS1, tail.tail))
    }
/*
#### Orderings and subgroup search algorithms

*/
    lazy val domainOrder = {
      assert(isImmutable)
      val a = Array.fill[Int](actionDimension)(-1)
      for ( (bel, i) <- base.zipWithIndex ) a(bel._0) = i
      var k = base.length
      for ( i <- 0 until actionDimension ) {
        if (a(i) == -1) {
          a(i) = k
          k += 1
        }
      }
      a
    }

    object DomainOrdering extends Ordering[Dom] {
      def compare(a: Dom, b: Dom) = Ordering.Int.compare(domainOrder(a._0), domainOrder(b._0))
    }

    object ElementOrdering extends Ordering[F] {
      def compare(a: F, b: F): Int = {
        for (bel <- base) {
          val ord = DomainOrdering.compare(act(a, bel), act(b, bel))
          if (ord != 0)
            return ord
        }
        0
      }
    }

    case class ImageOrdering(u: F) extends Ordering[Dom] {
      def compare(a: Dom, b: Dom) = DomainOrdering.compare(act(u, a), act(u, b))
    }

    /** Iterates through the elements of the represented group using the order
      * defined in Holt pp. 109-111
      */
    def orderedIterator(uPrev: F = identity): Iterator[F] = this match {
      case terminal: BSGSTerminal => Iterator(uPrev)
      case node: BSGSNode => for {
        b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
        uThis = transversal(b).u * uPrev
        u <- tail.orderedIterator(uThis)
      } yield u
    }

    def generalSearch(predicate: Predicate[F], test: BaseImageTest = TrivialBaseImageTest, uPrev: F = identity): Iterator[F] = this match {
      case terminal: BSGSTerminal => if (predicate(uPrev)) Iterator(uPrev) else Iterator.empty
      case node: BSGSNode => for {
        b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
        baseImage = act(uPrev, b)
        (takeIt, newTest) = test(baseImage) if takeIt
        uThis = transversal(b).u * uPrev
        u <- tail.generalSearch(predicate, newTest, uThis)
      } yield u
    }

    case class SubgroupSearchResult(val restartFrom: Int, val levelCompleted: Int) { }

    /** Recursive exploration of the elements of this group to build the subgroup.
      * 
      * @return The subgroup new generators and the level to restart the exploration from.
      */
    def subgroupSearchRec(predicate: Predicate[F], test: BaseImageTest, uPrev: F, level: Int, levelCompleted: Int, partialSubgroup: BSGSChain, startSubgroup: BSGSChain): SubgroupSearchResult = this match {
      case terminal: BSGSTerminal => {
        if (predicate(uPrev) && !uPrev.isIdentity) {
          startSubgroup.addStrongGeneratorsInChain(List(uPrev))
          return SubgroupSearchResult(levelCompleted - 1, levelCompleted)
        }
        return SubgroupSearchResult(level - 1, levelCompleted)
      }
      case node: BSGSNode => {
        var newLevelCompleted = levelCompleted
        val sortedOrbit = transversal.keysIterator.toList.sorted(ImageOrdering(uPrev))
        var sPrune = transversal.size
        for {
          deltaP <- sortedOrbit
          delta = act(uPrev, deltaP)
          (takeIt, newTest) = test(delta) if takeIt
          uThis = transversal(deltaP).u * uPrev
        } {
          if (sPrune < partialSubgroup.transversal.size)
            return SubgroupSearchResult(level - 1, level)
          val SubgroupSearchResult(subRestartFrom, subLevelCompleted) =
            tail.subgroupSearchRec(predicate, newTest, uThis, level + 1, newLevelCompleted, partialSubgroup.tail, startSubgroup)
          newLevelCompleted = subLevelCompleted
          if (subRestartFrom < level)
            return SubgroupSearchResult(subRestartFrom, newLevelCompleted)
          sPrune -= 1
        }
        SubgroupSearchResult(level - 1, level)
      }
    }

    def subgroupSearch(predicate: Predicate[F], test: BaseImageTest = TrivialBaseImageTest): BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode => {
        if (node.order == 1)
          return this
        val cons = BSGSChain.mutableFromBaseAndGeneratingSet(base, Nil)
        val SubgroupSearchResult(restartFrom, levelCompleted) = subgroupSearchRec(predicate, test, identity, 0, length, cons, cons)
        assert(levelCompleted == 0)
        cons.cleanupGenerators
        cons.removeRedundantGenerators
        cons.makeImmutable
        cons
      }
    }

    def intersection(h: BSGSChain): BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode => {
        val hWithBase = h.withBase(base)
        case class IntersectionTest(hSubgroup: BSGSChain, hPrev: F) extends BaseImageTest {
          def apply(baseImage: Dom): (Boolean, BaseImageTest) = {
            val b = act(hPrev.inverse, baseImage)
            if (!hSubgroup.transversal.isDefinedAt(b))
              return (false, null)
            val uh = hSubgroup.transversal(b).u
            return (true, IntersectionTest(hSubgroup.tail, uh * hPrev))
          }
        }
        subgroupSearch(hWithBase.contains(_), IntersectionTest(hWithBase, identity))
      }
    }
/*
#### Mutable methods used in the construction of the BSGS chain
*/
    def isImmutable: Boolean

    def makeImmutable: Unit = this match {
      case terminal: BSGSTerminal => { }
      case node: BSGSNode => {
        node.isImmutable = true
        tail.makeImmutable
      }
    }

    def removeRedundantGenerators {
      if (isTerminal)
        return
      val node = this.asInstanceOf[BSGSNode]
      // Straight-forward implementation of REMOVEGENS, section 4.4.4, p.95 of Holt.
      while(!tryToRemoveGenerator.isEmpty) { }
    }

    def tryToRemoveGenerator: Option[F] = this match {
      case terminal: BSGSTerminal => None
      case node: BSGSNode => {
        assert(!isImmutable)
        val orbitSize = transversal.size
        val toRemove = tail.tryToRemoveGenerator.orElse {
          val candidatesToRemoval: List[F] = strongGeneratingSet.filter(g => act(g, beta) != beta)
          def findCandidate: Option[F] = {
            for (h <- candidatesToRemoval) {
              val newGenerators: List[F] = strongGeneratingSet.filterNot(_ == h)
              val newOrbitSize = makeOrbit(beta, newGenerators).size
              if (newOrbitSize == orbitSize)
                return Some(h)
            }
            None
          }
          findCandidate
        }
        toRemove match {
          case Some(h) =>
            node.strongGeneratingSet = strongGeneratingSet.filterNot(_ == h)
            Some(h)
          case None => None
        }
      }
    }

    def putInOrder: Boolean = this match {
      case terminal: BSGSTerminal => false
      case node: BSGSNode => {
        assert(!isImmutable)
        while(tail.putInOrder) { }
        for (b <- transversal.keysIterator) {
          val ub = transversal(b).u
          for (x <- strongGeneratingSet) {
            // TODO: test if generator is trivial with more clever transversals
            if (!transversal.isDefinedAt(act(x, b)))
              node.transversal = node.transversal.updated(List(x), strongGeneratingSet)
            val schreierGen = ub*x*transversal(act(x, b)).uinv
            addElement(schreierGen) match {
              case None => { }
              case Some(someH) => {
                while(tail.putInOrder) { }
                addStrongGeneratorsHere(List(someH))
                return true
              }
            }
          }
        }
        false
      }
    }

    def addElement(f : F): Option[F] = {
      val node = this.asInstanceOf[BSGSNode]
      assert(!isImmutable)
      val b = act(f, beta)
      if (!transversal.isDefinedAt(b)) {
        addStrongGeneratorsHere(List(f))
        return Some(f)
      }
      val h = f * transversal(b).uinv
      assert(act(h, beta) == beta)
      if (tail.isTerminal) {
        if (h.isIdentity)
          return None
        val newBasePoint = actionDomain.find( k => act(h, k) != k ).get
        val newTransversal = makeTransversal(newBasePoint)
        val newTail = new BSGSNode(newTransversal, Nil, tail, false)
        node.tail = newTail
        addStrongGeneratorsInChain(List(h))
        return Some(h)
      } else {
        tail.addElement(h) match {
          case None => None
          case Some(gen) => {
            addStrongGeneratorsHere(List(gen))
            Some(gen)
          }
        }
      }
    }

    def addStrongGeneratorsHere(newGenerators: List[F]) {
      this match {
        case terminal: BSGSTerminal => assert(newGenerators.isEmpty)
        case node: BSGSNode => {
          assert(!isImmutable)
          node.strongGeneratingSet = newGenerators ++ node.strongGeneratingSet
          node.transversal = node.transversal.updated(newGenerators, node.strongGeneratingSet)
        }
      }
    }

    def addStrongGeneratorsInChain(newGenerators: List[F]) {
      this match {
        case terminal: BSGSTerminal => assert(newGenerators.isEmpty)
        case node: BSGSNode => {
          assert(!isImmutable)
          addStrongGeneratorsHere(newGenerators)
          val tailGenerators = newGenerators.filter(g => act(g, beta) == beta)
          node.tail.addStrongGeneratorsInChain(tailGenerators)
        }
      }
    }

    def collectAllGenerators: Set[F] = this match {
      case terminal: BSGSTerminal => Set.empty[F]
      case node: BSGSNode => tail.collectAllGenerators ++ node.strongGeneratingSet
    }

    def buildStrongGeneratorsList(newStrongGenerators: List[F]): List[F] = this match {
      case terminal: BSGSTerminal => {
        assert(newStrongGenerators.isEmpty)
        List.empty[F]
      }
      case node: BSGSNode => {
        val (fixingBeta, notFixingBeta) =
          newStrongGenerators.partition(g => act(g, beta) == beta)
        val generatorsTail = tail.buildStrongGeneratorsList(fixingBeta)
        node.strongGeneratingSet = notFixingBeta ++ generatorsTail
        node.strongGeneratingSet
      }
    }

    def cleanupGenerators {
      if (!isTerminal)
        buildStrongGeneratorsList(collectAllGenerators.toList)
    }

/*
#### Coset representatives algorithms

The method `rightCosetMinimalRepresentativeUsingBSGSBase finds the minimal
lexicographic representative of the right coset \\( S f \\) given the subgroup \\( S \\)
provided by this BSGSChain of the group G and an element f in G.

It retrieves the element \\( s \in S \\) such that \\( f' = s f \\) is lexicographically
minimal according to `domOrdering`, and returns \\( f' \\).

Lexicographic order is a total order according to the following rule:

\\[ d < e \Leftrightarrow (b_1^{d'} < b_1^{e'}) \text{ or } 
(b_1^{d'} = b_1^{e'} \text{ and } b_2^{d'} < b_2^{e'}) \text{ or ...} \\]

- where \\( d' = \phi(d), e' = \phi(e) \\) and \\( \phi \\) is the permutation action
  of the group,
- where the order \\( < \\) of domain elements is given by `domOrdering` and
  the indices used in the lexicographic ordering are given by the base of
  the subgroup. The subgroup should have a complete base (b1, b2 ...) = (1, 2, 3 ...)
  except if you know what you are doing.
*/
    def rightCosetMinimalRepresentativeUsingBSGSBase(f: F)(implicit domOrdering: Ordering[Dom]): F =
      chain match {
        // end of BSGS chain, we have our result
        case terminal: BSGSTerminal => f
        case node: BSGSNode => {
          // beta**(sk sk-1 ... s2 s1 f) = (beta**sk) ** (sk-1 ... s2 s1 f)  = b ** partial
          val minimumB = transversal.keysIterator.minBy(b => act(f, b))
          // special case: if minimumB == beta, then u = identity
          val newF = if(minimumB == beta) f else transversal(minimumB).u * f
          tail.rightCosetMinimalRepresentativeUsingBSGSBase(newF)
        }
      }

    def rightCosetMinimalRepresentativeUsingBSGSBase(f: F, finv: F)(implicit domOrdering: Ordering[Dom]): (F, F) =
      chain match {
        // end of BSGS chain, we have our result
        case terminal: BSGSTerminal => (f, finv)
        case node: BSGSNode => {
          // beta**(sk sk-1 ... s2 s1 f) = (beta**sk) ** (sk-1 ... s2 s1 f)  = b ** partial
          val minimumB = transversal.keysIterator.minBy(b => act(f, b))
          // special case: if minimumB == beta, then u = identity
          val (newF, newFinv) = if(minimumB == beta) (f, finv) else (transversal(minimumB).u * f, finv * transversal(minimumB).uinv)
          tail.rightCosetMinimalRepresentativeUsingBSGSBase(newF, newFinv)
        }
      }

  }

/*
### Algorithms for the construction of a BSGS chain
*/
  object BSGSChain {
    def randomSchreierSims(base: List[Dom], randomElement: Random => F, knownOrder: BigInt): BSGSChain = {
      def findBaseElement: Dom = {
        val f = randomElement(options.randomGenerator)
        actionDomain.find(b => act(f, b) != b).getOrElse(Dom._1(1))
      }
      val chain = mutableFromBase(if(base.isEmpty) List(findBaseElement) else base)
      while (chain.order < knownOrder)
        chain.addElement(randomElement(options.randomGenerator))
      chain.cleanupGenerators
      chain.removeRedundantGenerators
      chain.cleanupGenerators
      chain.makeImmutable
      chain
    }

    def deterministicSchreierSims(base: List[Dom], fromGenerators: List[F]) = {
      def findBaseElement: Dom = {
        if (fromGenerators.isEmpty)
          Dom._1(1)
        else
          actionDomain.find(b => act(fromGenerators.head, b) != b).get
      }
      val chain = mutableFromBaseAndGeneratingSet(if(base.isEmpty) List(findBaseElement) else base, fromGenerators)
      while (chain.putInOrder) { }
      chain.cleanupGenerators
      chain.removeRedundantGenerators
      chain.cleanupGenerators
      chain.makeImmutable
      chain
    }
    def fromBaseAndGeneratingSet(base: List[Dom], genSet: List[F]) = {
      val chain = mutableFromBaseAndGeneratingSet(base, genSet)
      chain.makeImmutable
      chain
    }
    def mutableFromBaseAndGeneratingSet(base: List[Dom], genSet: List[F]): BSGSChain = base match {
      case Nil => new BSGSTerminal
      case beta :: tailBase => {
        val transversal = makeTransversal(beta, genSet)
        val tailGenSet = genSet.filter(g => act(g, beta) == beta)
        val tail = mutableFromBaseAndGeneratingSet(tailBase, tailGenSet)
        new BSGSNode(transversal, genSet, tail, false)
      }
    }
    def mutableFromBase(base: List[Dom]): BSGSChain = base match {
      case Nil => new BSGSTerminal
      case beta :: tailBase => {
        val transversal = makeTransversal(beta)
        val tail = mutableFromBase(tailBase)
        new BSGSNode(transversal, Nil, tail, false)
      }
    }
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

  class Subgroup(val subBSGS: BSGSChain) extends FiniteGroup[F] {
    def canEqual(other: Any): Boolean =
      other.isInstanceOf[Subgroup] && (other.asInstanceOf[Subgroup].group eq group)
    override def equals(other: Any) = other match {
      case that: Subgroup => (this eq that) || ((that canEqual this) && ((this intersection that).order == order))
      case _ => false
    }
    override def hashCode = sys.error("No implementation of hashCode for Subgroup yet.")
    def group = containingGroup
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
    def rightCosetMinimalRepresentative(f: F): F =
      subBSGS.withFullLexicographicBase.rightCosetMinimalRepresentativeUsingBSGSBase(f)(Dom.IntOrder.DomOrdering)


  }
}

abstract class PGroup[P <: PermElement[P]](val identity: P, val options: GroupOptions = GroupOptions.default) extends Group[P] with PermGroup[P] {

  val action = TrivialAction(identity)

  def act(p: P, k: Dom) = p.image(k)
  def actionDomain = identity.domain
  def actionDimension = degree

  def withOptions(newOptions: GroupOptions) =
    PGroup.fromBSGS(identity, bsgs.base, bsgs.strongGeneratingSet, newOptions)

  def makeTransversal(newBeta: Dom, genSet: List[P] = Nil) =
    options.transversalBuilder.empty(newBeta, identity, action).updated(genSet, genSet)

  def makeOrbit(newBeta: Dom, genSet: List[P] = Nil) =
    options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)

  def degree = identity.size
  def explicitSift(product: P, remaining: Perm, chain: BSGSChain): (P, Perm) = chain.isTerminal match {
    case true => (product, remaining)
    case false => {
      val b = remaining.image(chain.beta)
      if (!chain.transversal.isDefinedAt(b))
        (product, remaining)
      else {
        val nextRemaining = remaining * chain.transversal(b).uinv.toExplicit
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
  def apply[P <: PermElement[P]](elements: P*) = {
    assert(!elements.isEmpty)
    val identity = elements.find(_.isIdentity) match {
      case Some(id) => id
      case None => elements.head * elements.head.inverse
    }
    val generators = elements.filterNot(_.isIdentity).toList
    fromGenerators(identity, generators, Nil)
  }

  def fromPermGroup[P <: PermElement[P]](
    permGroup: PermGroup[P],
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = myOptions.useRandomizedAlgorithms match {
    case true =>
      fromRandomElementsAndOrder(permGroup.identity, permGroup.randomElement, permGroup.order, myBase, myOptions)
    case false =>
      fromGenerators(permGroup.identity, permGroup.generators.toList, myBase, myOptions)
  }

  def fromGenerators[P <: PermElement[P]](
    myIdentity: P,
    myGenerators: List[P],
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity, myOptions) {
    lazy val bsgs = BSGSChain.deterministicSchreierSims(myBase, myGenerators)
  }

  def fromGeneratorsAndOrder[P <: PermElement[P]](
    myIdentity: P,
    myGenerators: List[P],
    myOrder: BigInt,
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity, myOptions) {
    lazy val bsgs = myOptions.useRandomizedAlgorithms match {
      case true => {
        val bag = RandomBag(myGenerators, myIdentity, max(10, myGenerators.length), 50, myOptions.randomGenerator)
        BSGSChain.randomSchreierSims(myBase, bag.randomElement, myOrder)
      }
      case false => BSGSChain.deterministicSchreierSims(myBase, myGenerators)
    }
  }

  def fromRandomElementsAndOrder[P <: PermElement[P]](
    myIdentity: P,
    myRandomElement: Random => P,
    myOrder: BigInt,
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity, myOptions) {
    require_(myOptions.useRandomizedAlgorithms == true)
    lazy val bsgs = BSGSChain.randomSchreierSims(myBase, myRandomElement, myOrder)
  }

  def fromBSGS[P <: PermElement[P]](
    myIdentity: P,
    myBase: List[Dom],
    myStrongGeneratingSet: List[P],
    myOptions: GroupOptions = GroupOptions.default) = new PGroup(myIdentity, myOptions) {
    val bsgs = {
      val chain = BSGSChain.mutableFromBaseAndGeneratingSet(myBase, myStrongGeneratingSet)
      chain.makeImmutable
      chain
    }
  }
}

abstract class FGroup[F <: FiniteElement[F]](
  val identity: F,
  val action: Action[F],
  val options: GroupOptions = GroupOptions.default) extends Group[F] {
  require_(action.faithful)

  def act(f: F, k: Dom) = action(f, k)
  def actionDomain = action.domain
  def actionDimension = action.dimension

  def withOptions(newOptions: GroupOptions) =
    FGroup.fromBSGS(identity, action, bsgs.base, bsgs.strongGeneratingSet, newOptions)

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil) =
    options.transversalBuilder.empty(newBeta, identity, action).updated(genSet, genSet)

  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil) =
    options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)
}

object FGroup {
  def fromFiniteGroup[F <: FiniteElement[F]](
    finiteGroup: FiniteGroup[F],
    myAction: Action[F],
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = myOptions.useRandomizedAlgorithms match {
    case true =>
      fromRandomElementsAndOrder(finiteGroup.identity, myAction, finiteGroup.randomElement, finiteGroup.order, myBase, myOptions)
    case false =>
      fromGenerators(finiteGroup.identity, myAction, finiteGroup.generators.toList, myBase, myOptions)
  }

  def fromGenerators[F <: FiniteElement[F]](
    myIdentity: F,
    myAction: Action[F],
    myGenerators: List[F],
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    lazy val bsgs = BSGSChain.deterministicSchreierSims(myBase, myGenerators)
  }

  def fromGeneratorsAndOrder[F <: FiniteElement[F]](
    myIdentity: F,
    myAction: Action[F],
    myGenerators: List[F],
    myOrder: BigInt,
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    lazy val bsgs = myOptions.useRandomizedAlgorithms match {
      case true => {
        val bag = RandomBag(myGenerators, myIdentity, max(10, myGenerators.length), 50, myOptions.randomGenerator)
        BSGSChain.randomSchreierSims(myBase, bag.randomElement, myOrder)
      }
      case false => BSGSChain.deterministicSchreierSims(myBase, myGenerators)
    }
  }

  def fromRandomElementsAndOrder[F <: FiniteElement[F]](
    myIdentity: F,
    myAction: Action[F],
    myRandomElement: Random => F,
    myOrder: BigInt,
    myBase: List[Dom] = Nil,
    myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    require_(myOptions.useRandomizedAlgorithms == true)
    lazy val bsgs = BSGSChain.randomSchreierSims(myBase, myRandomElement, myOrder)
  }

  def fromBSGS[F <: FiniteElement[F]](
    myIdentity: F,
    myAction: Action[F],
    myBase: List[Dom],
    myStrongGeneratingSet: List[F],
    myOptions: GroupOptions = GroupOptions.default) = new FGroup(myIdentity, myAction, myOptions) {
    val bsgs = {
      val chain = BSGSChain.mutableFromBaseAndGeneratingSet(myBase, myStrongGeneratingSet)
      chain.makeImmutable
      chain
    }
  }
}

