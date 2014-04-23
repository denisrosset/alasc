package net.alasc

import scala.util.Random

/*
 ### `BSGSChain`: base class for the BSGS chain used by the `Group`
 */
sealed abstract class BSGSChain[F <: Finite[F]] {
  chain =>
  /*
   #### Chain node data
   */
  def isTerminal: Boolean

  def tail: BSGSChain[F]
  def action: Action[F]
  def act(f: F, k: Dom) = action(f, k)
  def actionDomain = action.domain
  def actionDimension = action.dimension
  def identity = action.identity
  def transversal: Transversal[F]
  def strongGeneratingSet: List[F]

  def makeTransversal(newBeta: Dom, genSet: List[F] = Nil)(implicit options: GroupOptions): Transversal[F] = options.transversalBuilder.empty(newBeta, action).updated(genSet, genSet)

  def makeOrbit(newBeta: Dom, genSet: List[F] = Nil)(implicit options: GroupOptions): Orbit[F] =  options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)

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

  def iterator = new Iterator[BSGSChain[F]] {
    private var current: BSGSChain[F] = chain
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

  def check(implicit options: GroupOptions) = {
    assert(base.toSet.size == base.size)
    checkFixBase(Nil)
    checkNode
  }

  def checkFixBase(partialBase: List[Dom])(implicit options: GroupOptions): Unit = this match {
    case node: BSGSNode[F] => {
      for (WithInverse(u, uinv) <- transversal.valuesIterator; b <- partialBase)
        assert(act(u, b) == b)
      for (g <- strongGeneratingSet; b <- partialBase)
        assert(act(g, b) == b)
      tail.checkFixBase(beta :: partialBase)
    }
    case _ => { }
  }

  def checkNode(implicit options: GroupOptions): Unit = this match {
    case node: BSGSNode[F] => {
      assert(strongGeneratingSet.toSet.size == strongGeneratingSet.size) // no duplicates
      assert(tail.strongGeneratingSet.toSet.subsetOf(strongGeneratingSet.toSet))
      for ((b, WithInverse(u, uinv)) <- transversal) {
        assert(act(u, beta) == b)
        assert(act(uinv, b) == beta)
      }
      val o = makeOrbit(beta, strongGeneratingSet)
      assert(o.orbitSet == transversal.orbitSet)
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

  def random(implicit gen: scala.util.Random): F = this.isTerminal match {
    case true => identity
    case false => tail.random(gen) * transversal.random(gen)
  }

  /*
   #### Sifting through the chain
   */

  case class SiftResult[F <: Finite[F]](val transversalIndices: List[Dom], val remaining: F) {
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
  def conjugatedBy(f: F): BSGSChain[F] = if (f.isIdentity) this else conjugatedBy(f, f.inverse)
  def conjugatedBy(f: F, finv: F): BSGSChain[F] = this match {
    case terminal: BSGSTerminal[F] => terminal
    case node: BSGSNode[F] =>
      new BSGSNode[F](action, transversal.conjugatedBy(f, finv), strongGeneratingSet.map(x => finv*x*f), tail.conjugatedBy(f, finv))
  }

  def removingRedundantBasePoints: BSGSChain[F] = this match {
    case terminal: BSGSTerminal[F] => terminal
    case node: BSGSNode[F] if transversal.size == 1 => tail.removingRedundantBasePoints
    case node: BSGSNode[F] => {
      val newTail = tail.removingRedundantBasePoints
      if (newTail eq tail)
        this
      else
        new BSGSNode[F](action, transversal, strongGeneratingSet, newTail)
    }
  }
  /** Changes the current base. The base of the returned BSGSChain[F] will start
    * with newBase, but can be longer if needed. No redundant base points will be 
    * kept in these additional points.
    */
  def withBase(newBase: List[Dom])(implicit options: GroupOptions): BSGSChain[F] = options.baseChangeStrategy match {
    case BaseSwapOnly => withBaseNoConjugation(newBase)
    case BaseSwapAndConjugation => withBaseConjugation(newBase)
    case BaseFromScratch => withBaseFromScratch(newBase)
  }

  def fullLexicographicBase(implicit options: GroupOptions): List[Dom] = (1 to actionDimension).map(Dom._1(_)).toList

  def withFullLexicographicBase(implicit options: GroupOptions): BSGSChain[F] = withBase(fullLexicographicBase)
  // TODO: this computes twice the new base
  def withLexicographicBase(implicit options: GroupOptions): BSGSChain[F] = withBase(lexicographicBase())

  @annotation.tailrec final def findFirstLexicographicBasePoint(start: Dom = Dom._1(1))(implicit options: GroupOptions): Option[Dom] =
    (start._1 > actionDimension) match {
      case true => None
      case false => {
        strongGeneratingSet.exists(g => act(g, start) != start) match {
          case true => Some(start)
          case false => findFirstLexicographicBasePoint(Dom._1(start._1 + 1))
        }
      }
    }

  def lexicographicTail(implicit options: GroupOptions): BSGSChain[F] = {
    if (beta._1 == actionDimension)
      new BSGSTerminal[F](action)
    else {
      val nextBeta = beta.next
      tail match {
        case terminal: BSGSTerminal[F] =>
          new BSGSNode[F](action, makeTransversal(nextBeta), Nil, terminal, true)
        case node: BSGSNode[F] =>
          node.withHeadBasePoint(nextBeta)
      }
    }
  }

  def lexicographicBase(start: Dom = Dom._1(1))(implicit options: GroupOptions): List[Dom] = {
    val basePoint = findFirstLexicographicBasePoint(start)
    basePoint match {
      case None => Nil
      case Some(beta) => beta :: withHeadBasePoint(beta).tail.lexicographicBase(Dom._1(beta._1 + 1))
    }
  }

  def withBaseFromScratch(newBase: List[Dom])(implicit options: GroupOptions): BSGSChain[F] = options.useRandomizedAlgorithms match {
    case true =>
      BSGSChain.randomSchreierSims[F](action, newBase, random(_), order)
    case false =>
      BSGSChain.deterministicSchreierSims[F](action, newBase, strongGeneratingSet)
  }

  def withBaseConjugation(newBase: List[Dom])(implicit options: GroupOptions): BSGSChain[F] = {
    val (newChain, f, finv) = withBaseConjugationHelper(newBase, identity, identity)
    newChain.conjugatedBy(f)
  }
  def withBaseConjugationHelper(newBase: List[Dom], f: F, finv: F)(implicit options: GroupOptions): (BSGSChain[F], F, F) = newBase match {
    case hd :: tl => {
      val alpha = act(finv, hd)
      if (!isTerminal) {
        if (!base.contains(alpha) && transversal.isDefinedAt(alpha)) {
          val (newTail, newF, newFinv) =
            tail.withBaseConjugationHelper(tl, transversal(alpha).u*f, finv*transversal(alpha).uinv)
          val newStrongGeneratingSet = (strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
          val newNode = new BSGSNode[F](action, transversal, newStrongGeneratingSet, newTail)
          return ((newNode, newF, newFinv))
        }
      }
      val swappedNode = withHeadBasePoint(alpha)
      val (newTail, newF, newFinv) = swappedNode.tail.withBaseConjugationHelper(tl, f, finv)
      val newStrongGeneratingSet = (swappedNode.strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
      val newNode = new BSGSNode[F](action, swappedNode.transversal, newStrongGeneratingSet, newTail)
      ((newNode, newF, newFinv))
    }
    case Nil => (removingRedundantBasePoints, f, finv)
  }

  def withBaseNoConjugation(newBase: List[Dom])(implicit options: GroupOptions): BSGSChain[F] = newBase match {
    case hd :: tl => {
      val newHead = withHeadBasePoint(hd)
      val newTail = newHead.tail.withBaseNoConjugation(tl)
      val newStrongGeneratingSet =
        (newHead.strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
      new BSGSNode[F](action, newHead.transversal, newStrongGeneratingSet, newTail)
    }
    case Nil => removingRedundantBasePoints
  }

  /** Called on a BSGSChain[F] containing basePoint, returns
    * a BSGSChain[F] with basePoint at head.
    */
  def putExistingBasePointInHead(basePoint: Dom)(implicit options: GroupOptions): BSGSChain[F] = {
    if (beta == basePoint)
      return this
    val newTail = tail.putExistingBasePointInHead(basePoint)
    val newStrongGeneratingSet = (strongGeneratingSet diff newTail.strongGeneratingSet) ::: newTail.strongGeneratingSet
    val beforeSwap = new BSGSNode[F](action, transversal, newStrongGeneratingSet,
      newTail)
    beforeSwap.baseSwap
  }

  def withHeadBasePoint(basePoint: Dom)(implicit options: GroupOptions): BSGSChain[F] = {
    val withPoint = if (base.contains(basePoint)) this else insertBasePoint(basePoint)
    withPoint.putExistingBasePointInHead(basePoint)
  }

  /** Returns an updated BSGSChain[F] with the new basePoint inserted.
    * 
    * The current BSGSChain[F] must not contains basePoint
    */
  def insertBasePoint(basePoint: Dom)(implicit options: GroupOptions): BSGSChain[F] = this match {
    case terminal: BSGSTerminal[F] =>
      new BSGSNode[F](action, makeTransversal(basePoint), Nil, terminal)
    case node: BSGSNode[F] => {
      val orbit = makeOrbit(basePoint, strongGeneratingSet)
      if (orbit.size > 1)
        new BSGSNode[F](action, transversal, strongGeneratingSet, tail.insertBasePoint(basePoint))
      else
        new BSGSNode[F](action, makeTransversal(basePoint), strongGeneratingSet, this)
    }
  }

  def baseSwap(implicit options: GroupOptions): BSGSChain[F] = options.useRandomizedAlgorithms match {
    case true => randomizedBaseSwap(options)
    case false => deterministicBaseSwap
  }

  /** Deterministic base swap.
    * 
    * @return BSGS group with first two base elements swapped.
    * 
    * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
    * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
    */
  def deterministicBaseSwap(implicit options: GroupOptions): BSGSChain[F] = {
    require_(isInstanceOf[BSGSNode[F]])
    val node = asInstanceOf[BSGSNode[F]]
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
        gammaSet = gammaSet diff o.orbitSet
      } else {
        val y = tail.transversal(act(xinv, beta1)).u
        val yx = y*x
        if(!betaT.contains(act(yx, beta))) {
          tList = yx :: tList
          betaT = betaT.updated(List(yx), tList)
          betaT1 = betaT1.updated(List(yx), tList)
          gammaSet = gammaSet diff betaT.orbitSet
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
    new BSGSNode[F](action, nTrv, nS, new BSGSNode[F](action, nTrv1, nS1, tail.tail))
  }

  def randomizedBaseSwap(implicit options: GroupOptions): BSGSChain[F] = {
    require_(isInstanceOf[BSGSNode[F]])
    val node = asInstanceOf[BSGSNode[F]]
    require_(!tail.isTerminal)
    val nBeta = tail.beta
    val nBeta1 = beta
    var nS = strongGeneratingSet
    var nS1 = strongGeneratingSet.filter(g => act(g, nBeta) == nBeta)
    var nTrv = makeTransversal(nBeta, nS)
    var nTrv1 = makeTransversal(nBeta1, nS1)
    val siz = (transversal.size*tail.transversal.size)/nTrv.size
    while (nTrv1.size < siz) {
      val g = chain.random(options.randomGenerator)
      val h = g * nTrv(act(g, nBeta)).uinv
      if (!nTrv1.isDefinedAt(act(h, nBeta1))) {
        nS = h :: nS
        nS1 = h :: nS1
        nTrv1 = nTrv1.updated(List(h), nS1)
        nTrv = nTrv.updated(List(h), nS)
      }
    }
    new BSGSNode[F](action, nTrv, nS, new BSGSNode[F](action, nTrv1, nS1, tail.tail))
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
    case terminal: BSGSTerminal[F] => Iterator(uPrev)
    case node: BSGSNode[F] => for {
      b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
      uThis = transversal(b).u * uPrev
      u <- tail.orderedIterator(uThis)
    } yield u
  }

  def generalSearch(predicate: Predicate[F], test: BaseImageTest = TrivialBaseImageTest, uPrev: F = identity)(implicit options: GroupOptions): Iterator[F] = this match {
    case terminal: BSGSTerminal[F] => if (predicate(uPrev)) Iterator(uPrev) else Iterator.empty
    case node: BSGSNode[F] => for {
      b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
      baseImage = act(uPrev, b)
      (takeIt, newTest) = test(baseImage) if takeIt
      uThis = transversal(b).u * uPrev
      u <- tail.generalSearch(predicate, newTest, uThis)
    } yield u
  }

  /** Recursive exploration of the elements of this group to build the subgroup.
    * 
    * @return The subgroup new generators and the level to restart the exploration from.
    */
  def subgroupSearchRec(predicate: Predicate[F], orbits: Array[Array[Int]], ordering: Array[Int], test: BaseImageTest, uPrev: F, level: Int, levelCompleted: Int, partialSubgroup: BSGSChain[F], startSubgroup: BSGSChain[F])(implicit options: GroupOptions): SubgroupSearchResult = this match {
    case terminal: BSGSTerminal[F] => {
      if (predicate(uPrev) && !uPrev.isIdentity) {
        startSubgroup.addStrongGeneratorsInChain(List(uPrev))
        return SubgroupSearchResult(levelCompleted - 1, levelCompleted)
      }
      return SubgroupSearchResult(level - 1, levelCompleted)
    }
    case node: BSGSNode[F] => {
      var newLevelCompleted = levelCompleted
      val orbit = orbits(level)
      var ind = 0
      while (ind < orbit.length) {
        ordering(ind) = domainOrder(act(uPrev, Dom._0(orbit(ind)))._0)
        ind += 1
      }

      def bubbleSort(intArray: Array[Int], orderArray: Array[Int]) {
        def swap(i: Int, j: Int) {
          var temp = intArray(j)
          intArray(j) = intArray(i)
          intArray(i) = temp
          temp = orderArray(j)
          orderArray(j) = orderArray(i)
          orderArray(i) = temp
        }
        var swapped = true
        var n = intArray.length
        while (swapped) {
          swapped = false
          var i = 1
          while (i < n) {
            if (orderArray(i - 1) > orderArray(i)) {
              swap(i - 1, i)
              swapped = true
            }
            i += 1
          }
          n -= 1
        }
      }
      bubbleSort(orbit, ordering)
/*      def imageOrdering(a: Int, b: Int) = // true if a < b
        domainOrder(act(uPrev, Dom._0(a))._0) < domainOrder(act(uPrev, Dom._0(b))._0)
      scala.util.Sorting.stableSort[Int](sortedOrbit, imageOrdering(_,_))*/
      var sPrune = transversal.size
      var n = orbit.length
      ind = 0
      while (ind < n) {
        val i = orbit(ind)
        val deltaP = Dom._0(i)
        val delta = act(uPrev, deltaP)
        val takeIt = test.check(delta)
        if (takeIt) {
          val uThis = transversal(deltaP).u * uPrev
          if (sPrune < partialSubgroup.transversal.size)
            return SubgroupSearchResult(level - 1, level)
          val newTest = test.nextTest(delta)
          val ssr = tail.subgroupSearchRec(predicate, orbits, ordering, newTest, uThis, level + 1, newLevelCompleted, partialSubgroup.tail, startSubgroup)
          val subRestartFrom = ssr.restartFrom
          val subLevelCompleted = ssr.levelCompleted
          newLevelCompleted = subLevelCompleted
          if (subRestartFrom < level)
            return SubgroupSearchResult(subRestartFrom, newLevelCompleted)
          sPrune -= 1
        }
        ind += 1
      }
      SubgroupSearchResult(level - 1, level)
    }
  }

  def subgroupSearch(predicate: Predicate[F], test: BaseImageTest = TrivialBaseImageTest)(implicit options: GroupOptions): BSGSChain[F] = this match {
    case terminal: BSGSTerminal[F] => terminal
    case node: BSGSNode[F] => {
      if (node.order == 1)
        return this
      val orbits = transversals.map(_.keysIterator.map(_._0).toArray).toArray
      val ordering = Array.ofDim[Int](transversals.map(_.size).max)
      val cons = BSGSChain.mutableFromBaseAndGeneratingSet[F](action, base, Nil)
      val SubgroupSearchResult(restartFrom, levelCompleted) = subgroupSearchRec(predicate, orbits, ordering, test, identity, 0, length, cons, cons)
      assert(levelCompleted == 0)
      cons.cleanupGenerators
//      cons.removeRedundantGenerators
      cons.makeImmutable
      cons
    }
  }

  def intersection(h: BSGSChain[F])(implicit options: GroupOptions): BSGSChain[F] = this match {
    case terminal: BSGSTerminal[F] => terminal
    case node: BSGSNode[F] => {
      val hWithBase = h.withBase(base)
      case class IntersectionTest(hSubgroup: BSGSChain[F], hPrev: F) extends BaseImageTest {
        def check(baseImage: Dom) = {
          val b = act(hPrev.inverse, baseImage)
          hSubgroup.transversal.isDefinedAt(b)
        }
        def nextTest(baseImage: Dom) = {
          val b = act(hPrev.inverse, baseImage)
          val uh = hSubgroup.transversal(b).u
          IntersectionTest(hSubgroup.tail, uh * hPrev)
        }
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

  def stabilizesCurrentBasePoint(f: F): Boolean = (act(f, beta) != beta)
  /*
   Returns a strong generating set for this stabilizer chain, preserving the
   provided generators list `given`.
   */
  def strongGeneratingSetGiven(given: List[F])(implicit options: GroupOptions): List[F] = this match {
    case terminal: BSGSTerminal[F] => Nil
    case node: BSGSNode[F] =>
      val (thisGiven, tailGiven) = given.partition(stabilizesCurrentBasePoint)
      val tailGeneratingSet = tail.strongGeneratingSetGiven(tailGiven)
      assert((thisGiven.toSet intersect tailGeneratingSet.toSet).isEmpty) // TODO remove
      val thisFixed = thisGiven ++ tailGeneratingSet
      val fixedOrbit = makeOrbit(beta, thisFixed)
      val orbitSizeGoal = transversal.size
      def removeRedundant(additionalGenerators: Set[F]): Set[F] = {
        additionalGenerators.foreach { gen =>
          val newAdditional = additionalGenerators - gen
          val newOrbit = fixedOrbit.updated(newAdditional ++ thisFixed, thisFixed)
          if (newOrbit.size == orbitSizeGoal)
            return removeRedundant(newAdditional)
        }
        additionalGenerators
      }
      removeRedundant(strongGeneratingSet.toSet diff thisFixed.toSet).toList ++ thisFixed
  }
  /*
   #### Mutable methods used in the construction of the BSGS chain
   */
  def isImmutable: Boolean

  def makeImmutable: Unit = this match {
    case terminal: BSGSTerminal[F] =>
    case node: BSGSNode[F] =>
      node.isImmutable = true
      tail.makeImmutable
  }

  def removeRedundantGenerators(implicit options: GroupOptions) {
    if (isTerminal)
      return
    val node = this.asInstanceOf[BSGSNode[F]]
    // Straight-forward implementation of REMOVEGENS, section 4.4.4, p.95 of Holt.
    while(!tryToRemoveGenerator.isEmpty) { }
  }

  def tryToRemoveGenerator(implicit options: GroupOptions): Option[F] = this match {
    case terminal: BSGSTerminal[F] => None
    case node: BSGSNode[F] => {
      assert(!isImmutable)
      val orbitSize = transversal.size
      val toRemove = tail.tryToRemoveGenerator.orElse {
        val candidatesToRemoval: List[F] = strongGeneratingSet.filter(stabilizesCurrentBasePoint)
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

  def putInOrder(implicit options: GroupOptions): Boolean = this match {
    case terminal: BSGSTerminal[F] => false
    case node: BSGSNode[F] => {
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

  def addElement(f : F)(implicit options: GroupOptions): Option[F] = {
    val node = this.asInstanceOf[BSGSNode[F]]
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
      val newTail = new BSGSNode[F](action, newTransversal, Nil, tail, false)
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
      case terminal: BSGSTerminal[F] => assert(newGenerators.isEmpty)
      case node: BSGSNode[F] => {
        assert(!isImmutable)
        node.strongGeneratingSet = newGenerators ++ node.strongGeneratingSet
        node.transversal = node.transversal.updated(newGenerators, node.strongGeneratingSet)
      }
    }
  }

  def addStrongGeneratorsInChain(newGenerators: List[F]) {
    this match {
      case terminal: BSGSTerminal[F] => assert(newGenerators.isEmpty)
      case node: BSGSNode[F] => {
        assert(!isImmutable)
        addStrongGeneratorsHere(newGenerators)
        val tailGenerators = newGenerators.filter(g => act(g, beta) == beta)
        node.tail.addStrongGeneratorsInChain(tailGenerators)
      }
    }
  }

  def collectAllGenerators: Set[F] = this match {
    case terminal: BSGSTerminal[F] => Set.empty[F]
    case node: BSGSNode[F] => tail.collectAllGenerators ++ node.strongGeneratingSet
  }

  def buildStrongGeneratorsList(newStrongGenerators: List[F]): List[F] = this match {
    case terminal: BSGSTerminal[F] => {
      assert(newStrongGenerators.isEmpty)
      List.empty[F]
    }
    case node: BSGSNode[F] => {
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
   provided by this BSGSChain[F] of the group G and an element f in G.

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
      case terminal: BSGSTerminal[F] => f
      case node: BSGSNode[F] => {
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
      case terminal: BSGSTerminal[F] => (f, finv)
      case node: BSGSNode[F] => {
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
  def randomSchreierSims[F <: Finite[F]](action: Action[F], base: List[Dom], randomElement: Random => F, knownOrder: BigInt, startingGeneratingSet: List[F] = Nil)(implicit options: GroupOptions): BSGSChain[F] = {
    def findBaseElement: Dom = {
      val f = randomElement(options.randomGenerator)
      action.domain.find(b => action(f, b) != b).getOrElse(Dom._1(1))
    }
    val chain = ((base.isEmpty, startingGeneratingSet.isEmpty)) match {
      case (false, _) => mutableFromBaseAndGeneratingSet(action, base, startingGeneratingSet)
      case (true, false) => mutableFromGenerators(action, startingGeneratingSet)
      case (true, true) => mutableFromBaseAndGeneratingSet(action, List(findBaseElement), startingGeneratingSet)
    }
    while (chain.order < knownOrder)
      chain.addElement(randomElement(options.randomGenerator))
    chain.cleanupGenerators
    chain.removeRedundantGenerators
    chain.cleanupGenerators
    chain.makeImmutable
    chain
  }

  def deterministicSchreierSims[F <: Finite[F]](action: Action[F], base: List[Dom], fromGenerators: List[F])(implicit options: GroupOptions) = {
    def findBaseElement: Dom = {
      if (fromGenerators.isEmpty)
        Dom._1(1)
      else
        action.domain.find(b => action(fromGenerators.head, b) != b).get
    }
    val chain = mutableFromBaseAndGeneratingSet(action, if(base.isEmpty) List(findBaseElement) else base, fromGenerators)
    while (chain.putInOrder) { }
    chain.cleanupGenerators
    chain.removeRedundantGenerators
    chain.cleanupGenerators
    chain.makeImmutable
    chain
  }
  def fromBaseAndGeneratingSet[F <: Finite[F]](action: Action[F], base: List[Dom], genSet: List[F])(implicit options: GroupOptions) = {
    val chain = mutableFromBaseAndGeneratingSet(action, base, genSet)
    chain.makeImmutable
    chain
  }
  def mutableFromGenerators[F <: Finite[F]](action: Action[F], genSet: List[F], startingBasePoint: Dom = Dom._1(1))(implicit options: GroupOptions): BSGSChain[F] =
    genSet.isEmpty match {
      case true => new BSGSTerminal[F](action)
      case false =>
        (startingBasePoint._1 to action.dimension).map(Dom._1).find(k => genSet.exists(
          g => action(g, k) != k)) match {
          case None =>
            new BSGSTerminal[F](action)
          case Some(beta) =>
            val transversal = makeTransversal(action, beta, genSet)
            val tailGenSet = genSet.filter(g => action(g, beta) == beta)
            val tail = mutableFromGenerators(action, tailGenSet, Dom._1(startingBasePoint._1 + 1))
            new BSGSNode[F](action, transversal, genSet, tail, false)
        }
    }

  def mutableFromBaseAndGeneratingSet[F <: Finite[F]](action: Action[F], base: List[Dom], genSet: List[F])(implicit options: GroupOptions): BSGSChain[F] = base match {
    case Nil => new BSGSTerminal[F](action)
    case beta :: tailBase => {
      val transversal = makeTransversal(action, beta, genSet)
      val tailGenSet = genSet.filter(g => action(g, beta) == beta)
      val tail = mutableFromBaseAndGeneratingSet(action, tailBase, tailGenSet)
      new BSGSNode[F](action, transversal, genSet, tail, false)
    }
  }

  def mutableFromBase[F <: Finite[F]](action: Action[F], base: List[Dom])(implicit options: GroupOptions): BSGSChain[F] = base match {
    case Nil => new BSGSTerminal[F](action)
    case beta :: tailBase => {
      val transversal = makeTransversal(action, beta)
      val tail = mutableFromBase(action, tailBase)
      new BSGSNode[F](action, transversal, Nil, tail, false)
    }
  }

  def makeTransversal[F <: Finite[F]](action: Action[F], newBeta: Dom, genSet: List[F] = Nil)(implicit options: GroupOptions): Transversal[F] = options.transversalBuilder.empty(newBeta, action).updated(genSet, genSet)

  def makeOrbit[F <: Finite[F]](action: Action[F], newBeta: Dom, genSet: List[F] = Nil)(implicit options: GroupOptions): Orbit[F] =  options.orbitBuilder.empty(newBeta, action).updated(genSet, genSet)
}

final class BSGSNode[F <: Finite[F]](
  val action: Action[F],
  var transversal: Transversal[F],
  var strongGeneratingSet: List[F],
  var tail: BSGSChain[F],
  var isImmutable: Boolean = true) extends BSGSChain[F] {
  def isTerminal = false
}

final class BSGSTerminal[F <: Finite[F]](val action: Action[F]) extends BSGSChain[F] {
  def isTerminal = true
  def isImmutable = true
  def strongGeneratingSet = List.empty[F]
  def transversal = throw new IllegalArgumentException("Cannot get transversal of BSGS chain terminal.")
  def tail = throw new IllegalArgumentException("Cannot get tail of BSGS chain terminal.")
}
