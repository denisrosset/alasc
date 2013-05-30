package net.alasc
package bsgs

import scala.annotation.tailrec
import scala.util.Random
import language.implicitConversions
import scala.language.higherKinds

case class SiftResult[F <: PermElement[F]](val transversalIndices: List[Dom], val remaining: F) {
  def prepend(b: Dom) = SiftResult(b :: transversalIndices, remaining)
}

sealed abstract class BSGSGroup[E <: PermElement[E]] extends PermGroup[BSGSElement[E]] {
  // Mutable/immutable toggle
  private[bsgs] def isImmutable: Boolean
  private[bsgs] def makeImmutable: Unit

  // Chain structure
  /** Tests whether this stabilizer chain element is the last. */
  def isTerminal: Boolean
  /** Returns the next subgroup in the stabilizer chain.
    * @note throws IllegalArgumentException on the terminal element. */
  def tail: BSGSGroup[E]
  /** Returns the length of the stabilizer chain. */
  def length: Int

  // Group operations
  def compatible(e: BSGSElement[E]): Boolean
  def contains(e: BSGSElement[E]) = true
  def contains(el: E) = basicSift(el).remaining.isIdentity
  def degree = representedIdentity.size
  def elements: Iterator[BSGSElement[E]]
  def fromExplicit(p: Perm): Option[BSGSElement[E]] = {
    implicit def conversion(e: E) = e.explicit
    val SiftResult(sequence, remaining) = basicSift(p)
    if (remaining.isIdentity)
      Some(fromSequence(sequence))
    else
      None
  }
  def generators = strongGeneratingSet.iterator.map(sift(_)._1)
  def identity: BSGSElement[E]
  def order: BigInt
  def random(implicit gen:scala.util.Random): BSGSElement[E]
//  def toTeX = TeX("{}^{"+degree+"}_{"+order+"} \\text{BSGS} \\left ( \\begin{array}{" + "c"*length + "}" + base.mkString(" & ") + "\\\\" + transversalSizes.mkString(" & ") + "\\end{array} \\right )")

  // BSGS data
  /** Base element for current node in stabilizer chain. */
  def beta: Dom
  /** Base of transversal chain. */
  def base: Base
  def strongGeneratingSet: List[E]
  def representedIdentity: E
  def transversal: TransLike[E]
  /** Size of transversal of this node in stabilizer chain. */
  def transversalSize = transversal.size
  /** List of transversal sizes. */
  def transversalSizes: List[Int]

  // BSGS operations
  /** Sifts an element through the stabilizer chain. */
  def sift(e: E): (BSGSElement[E], E) = {
    assert(isImmutable)
    val SiftResult(sequence, remaining) = basicSift(e)
    (fromSequence(sequence), remaining)
  }

  /** Constructs a BSGS element from a sequence of transversal indices.
    * 
    * @param sequence Sequence of transversal indices.
    * 
    * @return Constructed BSGS group element.
    */
  def fromSequence(sequence: List[Dom]): BSGSElement[E]

  /** Constructs a BSGS element from a base image. */
  def fromBaseImage(baseImage: List[Dom]): BSGSElement[E]

  /** Sifts element f through the stabilizer chains.
    * 
    * @param f    Element to sift through
    * 
    * @return Transversal indices and the part remaining after sifting.
    */
  def basicSift[F <: PermElement[F]](f: F)(implicit conv: E => F): SiftResult[F]
  def transversalElement(level: Int, b: Dom): BSGSElement[E]

  // Modifications
  def cleanedBase: BSGSGroup[E]
  def convertElements[F <: PermElement[F]](f: E => F): BSGSGroup[F]

  // Orderings
  lazy val domainOrder = {
    assert(isImmutable)
    val a = Array.fill[Int](degree)(-1)
    val b = base
    for ( (bel, i) <- b.zipWithIndex ) a(bel._0) = i
    var k = base.length
    for ( i <- 0 until degree ) {
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

  object BSGSOrdering extends Ordering[BSGSElement[E]] {
    def compare(a: BSGSElement[E], b: BSGSElement[E]): Int = {
      for (bel <- base) {
        val ord = DomainOrdering.compare(a.image(bel), b.image(bel))
        if (ord != 0)
          return ord
      }
      0
    }
  }

  object ElementOrdering extends Ordering[E] {
    def compare(a: E, b: E): Int = {
      for (bel <- base) {
        val ord = DomainOrdering.compare(a.image(bel), b.image(bel))
        if (ord != 0)
          return ord
      }
      0
    }
  }

  case class ImageOrdering(u: E) extends Ordering[Dom] {
    def compare(a: Dom, b: Dom) = DomainOrdering.compare(u.image(a), u.image(b))
  }

  /** Iterates through the elements of the represented group using the order
    * defined in Holt TODO:(add ref. of pages)
    */
  def orderedIterator(uPrev: E): Iterator[E]

  // Search
  def generalSearch(predicate: Predicate[E], test: BaseImageTest = TrivialBaseImageTest, uPrev:E = representedIdentity): Iterator[E]

  def subgroupSearch(predicate: Predicate[E], test: BaseImageTest = TrivialBaseImageTest): BSGSGroup[E]

  def intersection(g: BSGSGroup[E]): BSGSGroup[E]

  private[bsgs] case class SubgroupSearchResult(val restartFrom: Int, val levelCompleted: Int) { }
  /** Recursive exploration of the elements of this group to build the subgroup.
    * 
    * @return The subgroup new generators and the level to restart the exploration from.
    */
  private[bsgs] def subgroupSearchRec(predicate: Predicate[E], test: BaseImageTest, uPrev: E, level: Int, levelCompleted: Int, partialSubgroup: BSGSGroup[E], startSubgroup: BSGSGroup[E]): SubgroupSearchResult

  // Subgroups
  def cosets(subgroup: BSGSGroup[E]): Transversal[E]

  // Base swaps
  def deterministicBaseSwap: BSGSGroup[E]
  def randomizedBaseSwap(implicit r: scala.util.Random): BSGSGroup[E]

  // Mutable operations
  private[bsgs] def putInOrder: Boolean
  private[bsgs] def addStrongGeneratorsHere(l: List[E]): Unit
  private[bsgs] def addStrongGeneratorsInChain(l: List[E]): Unit
  private[bsgs] def addElement(e: E): Option[E]
  private[bsgs] def removeRedundantGenerators: List[E]
}

final case class BSGSGroupTerminal[E <: PermElement[E]] private[bsgs](val id: E) extends BSGSGroup[E] {
  // Mutable/immutable toggle
  private[bsgs] def isImmutable = true
  private[bsgs] def makeImmutable { }

  // Chain structure
  def isTerminal = true
  def tail = throw new IllegalArgumentException("Cannot get tail of BSGS chain terminal.")
  def length = 0
  private[this] def element = BSGSElementTerminal(this)

  // Group operations
  def compatible(e: BSGSElement[E]) = e.isTerminal
  def elements = Iterator(element)
  def identity = element
  def order = BigInt(1)
  def random(implicit gen: Random) = element

  // BSGS data
  def beta = throw new IllegalArgumentException("Cannot get base element of BSGS chain terminal.")
  def base = Nil
  def strongGeneratingSet = List.empty[E]
  def representedIdentity = id
  def transversal = throw new IllegalArgumentException("Cannot get transversal of BSGS chain terminal.")
  def transversalSizes = Nil

  // BSGS operations
  def fromSequence(sequence: List[Dom]) = { require_(sequence.isEmpty); element }
  def fromBaseImage(baseImage: List[Dom]) =  { require_(baseImage.isEmpty); element }
  def basicSift[F <: PermElement[F]](f: F)(implicit conv: E => F) = SiftResult(Nil, f)
  def transversalElement(level: Int, b: Dom) = element

  // Modifications
  def cleanedBase = this
  def convertElements[F <: PermElement[F]](f: E => F) =
    BSGSGroupTerminal(f(id))

  // Orderings
  def orderedIterator(uPrev: E) = Iterator(uPrev)

  // Search
  def generalSearch(predicate: Predicate[E], test: BaseImageTest, uPrev:E) = if (predicate(uPrev)) Iterator(uPrev) else Iterator.empty

  def subgroupSearch(predicate: Predicate[E], test: BaseImageTest) = this

  private[bsgs] def subgroupSearchRec(predicate: Predicate[E], test: BaseImageTest, uPrev: E, level: Int, levelCompleted: Int, partialSubgroup: BSGSGroup[E], startSubgroup: BSGSGroup[E]): SubgroupSearchResult = {
    if (predicate(uPrev) && !uPrev.isIdentity) {
      startSubgroup.addStrongGeneratorsInChain(List(uPrev))
      return SubgroupSearchResult(levelCompleted - 1, levelCompleted)
    }
    return SubgroupSearchResult(level - 1, levelCompleted)
  }

  def intersection(g: BSGSGroup[E]) = this

  // Subgroups
  def cosets(g: BSGSGroup[E]) = TransversalTerminal(id)

  // Base swaps
  def deterministicBaseSwap: BSGSGroup[E] = throw new IllegalArgumentException("Cannot swap base of BSGS chain terminal")
  def randomizedBaseSwap(implicit r: scala.util.Random): BSGSGroup[E] = throw new IllegalArgumentException("Cannot swap base of BSGS chain terminal")


  // Mutable operations
  private[bsgs] def putInOrder = false
  private[bsgs] def addStrongGeneratorsInChain(h: List[E]) = if(!h.isEmpty) throw new IllegalArgumentException("Cannot add strong generators to BSGS chain terminal.")
  private[bsgs] def addStrongGeneratorsHere(h: List[E]) = if(!h.isEmpty) throw new IllegalArgumentException("Cannot add strong generators to BSGS chain terminal.")
  private[bsgs] def addElement(e: E) = throw new IllegalArgumentException("Cannot add element to BSGS chain terminal.")
  /** Removes the redundant generators in the strong generating set.
    */
  private[bsgs] def removeRedundantGenerators = List.empty[E]
}

final class BSGSGroupNode[E <: PermElement[E]](
  private[bsgs] var trv: TransLike[E],
  private[bsgs] var sg: List[E],
  private[bsgs] var id: E,
  private[bsgs] var isImmutable: Boolean,
  private[bsgs] var tl: BSGSGroup[E])
    extends BSGSGroup[E] {
  // Mutable/immutable toggle
  def this(newTrv: TransLike[E], newSg: List[E], newId: E, newTl: BSGSGroup[E]) =
    this(newTrv, newSg, newId, true, newTl)
  private[bsgs] def makeImmutable {
    isImmutable = true
    tail.makeImmutable
  }

  // Chain structure
  def isTerminal = false
  def tail = tl
  def length = tail.length + 1

  // Group operations
  def compatible(e: BSGSElement[E]) = e match {
    case BSGSElementNode(g, b, tl) => g == this && tail.compatible(e.tail)
    case _ => false
  }
  def elements = for ( n <- tail.elements; b <- trv.keysIterator ) yield
    BSGSElementNode(this, b, n)
  def identity = BSGSElementNode(this, trv.beta, tail.identity)
  def order = transversalSize * tail.order
  def random(implicit gen: Random) =
    BSGSElementNode(this, trv.random(gen)._1, tail.random(gen))


  // BSGS data
  def beta = trv.beta
  def base = beta :: tail.base
  def strongGeneratingSet = sg
  def representedIdentity = id
  def transversal = trv
  def transversalSizes = trv.size :: tail.transversalSizes

  // BSGS operations
  def fromSequence(sequence: List[Dom]): BSGSElement[E] =
    BSGSElementNode(this, sequence.head, tail.fromSequence(sequence.tail))
  def fromBaseImage(baseImage: List[Dom]) =
    BSGSElementNode(this, baseImage.head, tail.fromBaseImage(baseImage.tail.map(k => trv.uinv(baseImage.head).image(k))))

  def basicSift[F <: PermElement[F]](f: F)(implicit conv: E => F) = {
    val b = f.image(beta)
    if (!trv.isDefinedAt(b))
      SiftResult(Nil, f)
    else {
      val nextF = f * trv.uinv(b)
      val SiftResult(transversalIndices, remaining) = tail.basicSift(nextF)
      SiftResult(b :: transversalIndices, remaining)
    }
  }
  def transversalElement(level: Int, b: Dom) = new BSGSElementNode(this, if (level == 0) b else beta, tail.transversalElement(level - 1, b))

  // Modifications
  def cleanedBase = {
    if (trv.size == 1)
      tail.cleanedBase
    else
      new BSGSGroupNode(trv, sg, id, tail.cleanedBase)
  }
  def convertElements[F <: PermElement[F]](f: E => F) =
    new BSGSGroupNode(trv.mapValues(f), sg.map(f), f(id), tail.convertElements(f))

  // Orderings
  def orderedIterator(uPrev: E) =  for {
    b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
    uThis = transversal.u(b) * uPrev
    u <- tail.orderedIterator(uThis)
  } yield u

  // Search
  /** From Holt, p. 114 GENERALSEARCH */
  def generalSearch(predicate: Predicate[E], test: BaseImageTest, uPrev: E): Iterator[E] = for {
    b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
    baseImage = uPrev.image(b)
    (takeIt, newTest) = test(baseImage) if takeIt
    uThis = transversal.u(b) * uPrev
    u <- tail.generalSearch(predicate, newTest, uThis)
  } yield u

  def subgroupSearch(predicate: Predicate[E], test: BaseImageTest) = {
    val cons = BSGS.mutableFromBaseAndGeneratingSet(base, Nil, id, trv.builder)
    val SubgroupSearchResult(restartFrom, levelCompleted) = subgroupSearchRec(predicate, test, id, 0, length, cons, cons)
    assert(levelCompleted == 0)
    cons.removeRedundantGenerators
    cons.makeImmutable
    cons
  }

  private[bsgs] def subgroupSearchRec(predicate: Predicate[E], test: BaseImageTest, uPrev: E, level: Int, levelCompleted: Int, partialSubgroup: BSGSGroup[E], startSubgroup: BSGSGroup[E]): SubgroupSearchResult = {
    var newLevelCompleted = levelCompleted
    val sortedOrbit = transversal.keysIterator.toList.sorted(ImageOrdering(uPrev))
    var sPrune = trv.size
    for (
      deltaP <- sortedOrbit;
      delta = uPrev.image(deltaP);
      (takeIt, newTest) = test(delta) if takeIt;
      uThis = transversal.u(deltaP) * uPrev
    ) {
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

  def intersection(h: BSGSGroup[E]): BSGSGroup[E] = {
    case class IntersectionTest(hSubgroup: BSGSGroup[E], hPrev: E) extends BaseImageTest {
      def apply(baseImage: Dom): (Boolean, BaseImageTest) = {
        val b = hPrev.invImage(baseImage)
        if (!hSubgroup.transversal.isDefinedAt(b))
          return (false, null)
        val uh = hSubgroup.transversal.u(b)
        return (true, IntersectionTest(hSubgroup.tail, uh * hPrev))
      }
    }
    subgroupSearch(h.contains(_), IntersectionTest(h, representedIdentity))
  }

  // Subgroups
  def cosets(g: BSGSGroup[E]) = {
    var o = OrbitSet.fromSet(trv.beta, g.strongGeneratingSet)
    var addedGenerators = g.strongGeneratingSet
    var uList = List.empty[E]
    for (b <- trv.keysIterator) {
      if (!o.isDefinedAt(b)) {
        val newGenerator = trv.u(b)
        uList = newGenerator :: uList
        addedGenerators = newGenerator :: addedGenerators
        o = o.updated(List(newGenerator), addedGenerators)
      }
    }
    uList = id :: uList
    TransversalNode(uList, tail.cosets(g.tail))
  }

  // Base swaps
  /** Deterministic base swap.
    * 
    * @param transComp  Transversal companion object.
    * 
    * @return BSGS group with first two base elements swapped.
    * 
    * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
    * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
    */
  def deterministicBaseSwap: BSGSGroup[E] = {
    if (tail.isTerminal) throw new IllegalArgumentException("Cannot swap base of last element in the BSGS chain.")
    val builder = transversal.builder
    var tList = tail.strongGeneratingSet.filter( t => t.image(tail.beta) == tail.beta )
    val beta1 = tail.beta
    var gammaSet = transversal.keysIterator.filter( k => k != beta && k != beta1 ).toSet
    var betaT = OrbitSet.empty(beta)
    var betaT1 = OrbitSet.empty(beta1)
    betaT = betaT.updated(tList, tList)
    betaT1 = betaT1.updated(tList, tList)
    var beta1Gi = OrbitSet.empty(beta1)
    beta1Gi = beta1Gi.updated(sg, sg)
    val siz = (transversal.size*tail.transversal.size)/beta1Gi.size
    def exploreGamma {
      val gamma = gammaSet.head
      val (x, xinv) = trv(gamma)
      if (!tail.transversal.isDefinedAt(beta1**xinv)) {
        var o = OrbitSet.empty(gamma)
        o = o.updated(tList, tList)
        gammaSet = gammaSet diff o.orbit
      } else {
        val y = tail.transversal.u(beta1**xinv)
        val yx = y*x
        if(!betaT.contains(beta**yx)) {
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
    val nS = sg ++ tList
    val nTrv = builder.empty(beta1, id).updated(nS, nS)
    val nS1 = nS.filter( s => s.image(beta1) == beta1 )
    val nTrv1 = builder.empty(beta, id).updated(nS1, nS1)
    new BSGSGroupNode(nTrv, tList, id, new BSGSGroupNode(nTrv1, tList.filter( t => t.image(beta1) == beta1), id, tail.tail))
  }

  def randomizedBaseSwap(implicit r: scala.util.Random): BSGSGroup[E] = {
    if (tail.isTerminal) throw new IllegalArgumentException("Cannot swap base of last element in the BSGS chain.")
    val nBeta = tail.beta
    val nBeta1 = beta
    val builder = transversal.builder
    var nTrv = builder.empty(nBeta, id)
    var nTrv1 = builder.empty(nBeta1, id)
    var nS = sg
    var nS1 = sg.filter(_.image(nBeta) == nBeta)
    nTrv = nTrv.updated(nS, nS)
    nTrv1 = nTrv1.updated(nS1, nS1)
    val siz = (transversal.size*tail.transversal.size)/nTrv.size
    while (nTrv1.size < siz) {
      val g = random(r)
      val h = g.represents * nTrv.uinv(g.image(nBeta))
      if (!nTrv1.isDefinedAt(h.image(nBeta1))) {
        nTrv1 = nTrv1.updated(List(h), h :: nS)
        nS1 = h :: nS1
        nS = h :: nS
      }
    }
    new BSGSGroupNode(nTrv, nS, id, new BSGSGroupNode(nTrv1, nS1, id, tail.tail))
  }


  // Mutable operations
  private[bsgs] def putInOrder: Boolean = {
    assert(!isImmutable)
    while(tail.putInOrder) { }
    for (b <- trv.keysIterator) {
      val ub = trv.u(b)
      for (x <- sg) { // TODO: test if generator is trivial with more clever transversals
        if (!trv.isDefinedAt(x.image(b)))
          trv = trv.updated(List(x), sg)
        val schreierGen = ub*x*trv.uinv(x.image(b))
        addElement(schreierGen).map( someH => {
          while(tail.putInOrder) { }
          addStrongGeneratorsHere(List(someH))
          return true
        } )
      }
    }
    false
  }

  private[bsgs] def addStrongGeneratorsHere(l: List[E]) {
    assert(!isImmutable)
    sg = l ++ sg
    trv = trv.updated(l, sg)
  }

  private[bsgs] def addStrongGeneratorsInChain(l: List[E]) {
    assert(!isImmutable)
    addStrongGeneratorsHere(l)
    tail.addStrongGeneratorsInChain(l.filter(_.image(beta) == beta))
  }

  private[bsgs] def addElement(e: E): Option[E] = {
    assert(!isImmutable)
    val b = e.image(beta)
    if (!trv.isDefinedAt(b)) {
      addStrongGeneratorsHere(List(e))
      return Some(e)
    }
    val h = e * trv.uinv(b)
    assert(h.image(beta) == beta)
    if (tail.isTerminal) {
      if (h.isIdentity)
        return None
      val newBase = e.domain.find( k => h.image(k) != k ).get
      val newTrans = trv.builder.empty(newBase, id)
      tl = new BSGSGroupNode(newTrans, Nil, id, false, tl)
      addStrongGeneratorsInChain(List(h))
      return Some(h)
    } else
      tail.addElement(h).map(gen => {addStrongGeneratorsHere(List(gen)); gen})
  }
  private[bsgs] def removeRedundantGenerators: List[E] = {
    // Straight-forward implementation of REMOVEGENS, section 4.4.4, p.95 of Holt.
    assert(!isImmutable)
    var removed = tail.removeRedundantGenerators
    var myGenerators = strongGeneratingSet.diff(removed)
    def tryToRemove: Boolean = {
      val candidatesToRemoval = myGenerators.diff(tail.strongGeneratingSet)
      for (g <- candidatesToRemoval) {
        val newGenerators = myGenerators.diff(List(g))
        val o = OrbitSet.fromSet(beta, newGenerators)
        if (o.size == transversal.size) {
          myGenerators = newGenerators
          removed = g :: removed
          return true
        }
      }
      return false
    }
    while(tryToRemove) { }
    assert( OrbitSet.fromSet(beta, myGenerators).size == transversal.size )
    sg = myGenerators
    removed
  }
}
