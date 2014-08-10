package net.alasc.math
package bsgs

import scala.annotation.tailrec
import scala.collection.mutable.{BitSet => MutableBitSet}
import scala.collection.immutable.{BitSet => ImmutableBitSet}
import scala.util.Random
import spire.syntax.groupAction._
import spire.syntax.group._
import spire.syntax.eq._
import net.alasc.algebra._
import net.alasc.syntax.permutation._
import net.alasc.syntax.subgroup._

/** Builder for a BSGS chain.
  * 
  * A BSGSBuilder can be in three different states:
  * 
  * - fully immutable: no node is mutable, mutable operations will copy nodes as needed,
  * - partly mutable: the beginning of the chain is mutable, while the end is immutable,
  * - fully mutable: the whole chain is mutable.
  * 
  * Mutable nodes are identified because their `prev` reference is initialized so that the
  * mutable part of the BSGS chain is a double-linked list.
  * 
  * If a node is mutable (= is part of a BSGSBuilder, and has not been exported), then its `prev` is not null, and
  * either points to this node's parent it has one, or to this node itself if this node is the first in the chain.
  *
  * Moreover, if a node is mutable, all its parents must be mutable.
  * 
  * If the node is immutable, then `prev` is `null`.
  *
  * When the chain is fully/partly mutable, `last0` points to the last mutable node.
  * 
  * From those assumptions the state of the builder is valid for:
  * 
  * - when fully immutable: `start` is either a terminal, or `start.prev` is `null`, and `last0` is null,
  * - when partly/fully mutable: `start` is a node, `start.prev` points to `start`, and `last0` points to the last mutable node,
  * - when fully mutable: additionally, `last0.tail` is a terminal node.
  * 
  * `BSGSBuilder` is not thread-safe.
  */
final class BSGSBuilder[P](implicit val algebra: Permutation[P]) {
  /** Start of the BSGS chain. */
  protected[BSGSBuilder] var start: BSGS[P] = new BSGSTerm[P]

  def chain: BSGS[P] = start

  /** Last mutable node in the chain. */
  protected[BSGSBuilder] var lastMutable: BSGSMutableNode[P] = null

  def isEmpty = start.isInstanceOf[BSGSTerm[P]]

  def isFullyImmutable: Boolean = lastMutable eq null

  def isPartlyMutable: Boolean = (lastMutable ne null) && lastMutable.tail.isInstanceOf[BSGSNode[P]]

  def isFullyMutable: Boolean = (lastMutable ne null) && lastMutable.tail.isInstanceOf[BSGSTerm[P]]

  /** Makes the chain fully immutable. */
  def makeImmutable: Unit = if (lastMutable ne null) {
    @tailrec def removePrev(node: BSGSMutableNode[P]): Unit = {
      val prevSave = node.prev
      node.prev = null
      if (node ne prevSave) removePrev(prevSave)
    }
    lastMutable = null
  }

  /** Converts the current chain to immutable, and returns it. */
  def toBSGS: BSGS[P] = {
    makeImmutable
    start
  }

  /** Applies a function with side-effects to every mutable node. */
  def foreachMutable[A](f: BSGSMutableNode[P] => A): Unit = {
    @tailrec def rec(node: BSGSMutableNode[P]): Unit = {
      f(node)
      if (node.prev ne node)
        rec(node.prev)
    }
    if (lastMutable ne null)
      rec(lastMutable)
  }

  /** Conjugates the BSGS chain using the given group element. */
  def conjugate(ip: InversePair[P]): this.type = {
    if (!isEmpty) {
      makeFullyMutable(sys.error("Cannot be empty"))
      foreachMutable { _.conjugateThisNode(ip) }
    }
    this
  }

  /** Finds the last node in the chain. */
  @tailrec def findLastNode(node: BSGSNode[P]): BSGSNode[P] = node.tail match {
    case _: BSGSTerm[P] => node
    case tailNode: BSGSNode[P] => findLastNode(tailNode)
  }

  /** Makes the chain mutable up to the given node `upTo`.
    * 
    * @note A call to `makeMutable` invalidates all references to immutable nodes before
    *       the node made mutable in the chain.
    */
  def makeMutable(upTo: BSGSNode[P])(implicit builder: BSGSMutableNodeBuilder): BSGSMutableNode[P] = {
    @tailrec def createMutable(at: BSGSNode[P], prev: BSGSMutableNode[P]): BSGSMutableNode[P] =
      at match {
        case _ if at.isImmutable =>
          val newNode = builder(at, Some(prev))
          prev.tail = newNode
          lastMutable = newNode
          if (at ne upTo) // current node has children, including upTo
            createMutable(newNode.tail.asInstanceOf[BSGSNode[P]], newNode)
          else
            newNode
        case bm: BSGSMutableNode[P] =>
          if (bm ne upTo)
            createMutable(bm.tail.asInstanceOf[BSGSNode[P]], bm)
          else
            bm
        case _ => sys.error("A node should either be immutable or an instance of BSGSMutableNode.")
      }
    if (upTo.isImmutable) {
      val upToIsStart = start eq upTo
      if (lastMutable eq null) {
        assert(start.isInstanceOf[BSGSNode[P]]) // if upTo is a node, then start is a node
        val startNode = start.asInstanceOf[BSGSNode[P]]
        val newStartNode = builder(startNode, None)
        start = newStartNode
        lastMutable = newStartNode
      }
      if (!upToIsStart)  // if start ne upTo -> lastMutable has children including upTo
        createMutable(lastMutable.tail.asInstanceOf[BSGSNode[P]], lastMutable)
      else
        start.asInstanceOf[BSGSMutableNode[P]]
    }
    else upTo.asInstanceOf[BSGSMutableNode[P]]
  }

  /** Makes the chain fully mutable. */
  def makeFullyMutable(beta: => Int)(implicit builder: BSGSMutableNodeBuilder): Unit = start match {
    case _: BSGSTerm[P] =>
      val newStart = builder(beta, None, start)
      start = newStart
      lastMutable = newStart
    case node: BSGSNode[P] => if (!isFullyMutable) makeMutable(findLastNode(node))
  }

  /** Appends a new base point to the BSGS chain. */
  def append(beta: Int)(implicit builder: BSGSMutableNodeBuilder): BSGSMutableNode[P] = {
    if (isEmpty)
      mutableStartNode(beta)
    else {
      val newNode = builder(beta, Some(lastMutable), lastMutable.tail)
      lastMutable.tail = newNode
      lastMutable = newNode
      newNode
    }
  }

  /** Prepends a base point to the BSGS chain.
    * 
    * @param beta  New base point to prepend, must be fixed by all strong generators
    *              for the chain to stay valid after insertion.
    */
  def preprend(beta: Int)(implicit builder: BSGSMutableNodeBuilder) =
    if (isEmpty)
      makeFullyMutable(beta)
    else {
      val newNode = builder(beta, None, start)
      if (!start.isImmutable)
        start.asInstanceOf[BSGSMutableNode[P]].prev = newNode
      start = newNode
      if (lastMutable eq null)
        lastMutable = newNode
    }

  /** Adds the given generators to the BSGS chain, adding base elements
    * if necessary, and updates the transversals.
    * 
    * Not tail recursive. The chain is not completed (either the randomized or the 
    * deterministic Schreier-Sims algorithm should be used for that purpose).
    * 
    * @param generators  Sequence of generators. Must not contain the identity.
    */
  def addGenerators(generators: Iterable[P])(implicit builder: BSGSMutableNodeBuilder): Unit = {
    if (generators.isEmpty) return

    val remaining = scala.collection.mutable.HashSet.empty[P]
    remaining ++= generators

    def rec(mutableNode: BSGSMutableNode[P]): Unit = {
      val generatorsThere = remaining.filter(g => (mutableNode.beta <|+| g) != mutableNode.beta)
      remaining --= generatorsThere
      if (!remaining.isEmpty) {
        mutableNode.tail match {
          case _: BSGSTerm[P] =>
            val nextBeta = remaining.head.supportMin
            assert(nextBeta != -1)
            rec(append(nextBeta))
          case tailNode: BSGSNode[P] => rec(makeMutable(tailNode))
        }
      }
      generatorsThere.foreach( mutableNode.addStrongGeneratorHere(_) )
    }
    rec(mutableStartNode(generators.head.supportMin.max(0)))
  }

  /** Returns the start element if it is a mutable node, makes the start mutable if it is an immutable node,
    * or creates a start node with base point `beta`. */
  def mutableStartNode(beta: => Int)(implicit builder: BSGSMutableNodeBuilder): BSGSMutableNode[P] = start match {
    case _: BSGSTerm[P] =>
      val newNode = builder(beta, None, start)
      start = newNode
      lastMutable = newNode
      newNode
    case node: BSGSNode[P] => makeMutable(node)
  }

  /** Returns the start element if it is a node, or creates a start node with base point `beta`. */
  def startNode(beta: => Int)(implicit builder: BSGSMutableNodeBuilder): BSGSNode[P] = start match {
    case _: BSGSTerm[P] => mutableStartNode(beta)
    case node: BSGSNode[P] => node
  }

  /** Tail-recursive method used by `siftAndUpdateBase`. */
  @tailrec def siftAndUpdateBaseFrom(node: BSGSNode[P], p: P)(implicit builder: BSGSMutableNodeBuilder): Option[(BSGSMutableNode[P], P)] = {
    val b = node.beta <|+| p
    if (!node.inOrbit(b)) return Some(makeMutable(node) -> p)
    val h = p |+| node.uInv(b)
    assert(node.beta <|+| h == node.beta) // TODO remove
    node.tail match {
      case _: BSGSTerm[P] if h.isId => None
      case _: BSGSTerm[P] =>
        val newBasePoint = h.supportMin
        assert(newBasePoint != -1) // there is a support for h =!= identity
        Some(append(newBasePoint) -> h)
      case tailNode: BSGSNode[P] => siftAndUpdateBaseFrom(tailNode, h)
    }
  }

  /** Sifts the element `p` through the BSGS chain, and returns either:
    * 
    * - `None` if `p` can be sifted completely,
    * - `Some(pair)` where `pair` describes the node and the
    *   (incompletely) sifted element that should be inserted up to it.
    * 
    * Will insert new base points if necessary.
    * 
    * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
    */
  def siftAndUpdateBase(p: P)(implicit builder: BSGSMutableNodeBuilder): Option[(BSGSMutableNode[P], P)] =
    siftAndUpdateBaseFrom(startNode(p.supportMin.max(0)), p)


  /** Finds if a new strong generator can be found at the given `node`, assuming that
    * the chain starting at `node.tail` is complete.
    * 
    * If a new strong generator is found, returns some pair containing
    * the node and the strong generator to insert there.
    */
  def findNewStrongGeneratorAt(node: BSGSNode[P])(implicit builder: BSGSMutableNodeBuilder): Option[(BSGSMutableNode[P], P)] = {
    node.foreachOrbit { b =>
      val ub = node.u(b)
      for (x <- node.strongGeneratingSet) {
        val i = b <|+| x
        val ubx = ub |+| x
        if (!node.inOrbit(i))
          return Some(makeMutable(node) -> x)
        if (ubx =!= node.u(i)) {
          val schreierGen = ubx |+| node.uInv(i)
          siftAndUpdateBaseFrom(node, schreierGen) match {
            case some: Some[(BSGSMutableNode[P], P)] => return some
            case _ =>
          }
        }
      }
    }
    None
  }

  /** Deterministic Schreier-Sims algorithm. */
  def completeStrongGenerators: Unit =
    if (!isEmpty) {
      makeFullyMutable(sys.error("Not empty"))
      completeStrongGeneratorsAt(lastMutable)
    }

  /** Completes the set of strong generators starting at `node`, assuming that `node.tail` is already completed.
    * 
    * Inspired (but rewritten) from SCHREIERSIMS, page 91 of Holt (2005).
    */
  @tailrec def completeStrongGeneratorsAt(node: BSGSMutableNode[P])(implicit builder: BSGSMutableNodeBuilder): Unit =
    findNewStrongGeneratorAt(node) match {
      // current node does not have new strong generators, but node has parent that has to be completed
      case None if node.prev ne node => completeStrongGeneratorsAt(node.prev)
      // current node does not have new strong generators, and current node starts the chain, we are finished
      case None if node.prev eq node =>
      case Some((where, newGenerator)) =>
        where.addStrongGeneratorHere(newGenerator)
        completeStrongGeneratorsAt(where)
    }

  def newBaseFromScratch(newBase: Iterable[Int])(implicit options: BSGSOptions): Unit = options.algorithmType match {
    case Deterministic =>
      val newBuilder = BSGSBuilder.fromGenerators(chain.strongGeneratingSet, newBase)
      newBuilder.completeStrongGenerators
      start = newBuilder.start
      lastMutable = newBuilder.lastMutable
    case Randomized =>
      val newBuilder = BSGSBuilder.fromBase(newBase)
      while(newBuilder.chain.order <= chain.order) {
        for ( (node, p) <- newBuilder.siftAndUpdateBase(chain.random(options.randomGenerator)) )
          node.addStrongGeneratorHere(p)
      }
      start = newBuilder.start
      lastMutable = newBuilder.lastMutable
  }

  /** Swaps two adjacent nodes in the BSGS chain, and returns the first node after the swap, its tail
    * and the size goal for the orbit of the tail node.
    */
  def baseSwapBase(node1Before: BSGSMutableNode[P]): (BSGSMutableNode[P], BSGSMutableNode[P], Int) = {
    val node2Before = node1Before.tail.mapOrElse(makeMutable(_), sys.error("Node must have a tail node to be swapped."))
    val sizesProduct = BigInt(node1Before.orbitSize) * BigInt(node2Before.orbitSize)

    val node1BeforePrevOption = if (node1Before.prev eq node1Before) None else Some(node1Before.prev)
    val node1Beta = node2Before.beta
    val node2Beta = node1Before.beta
    val node2Tail = node2Before.tail

    val node1 = node2Before
    val node2 = node1Before
    val node2OwnGenerators = node1Before.ownGeneratorsPairs.filter(g => (node1Beta <|+| g) == node1Beta)

    node1.prev = node1BeforePrevOption.getOrElse(node1)
    node1.tail = node2
    node1.beta = node1Beta
    node1.clearTransversalAndKeepOwnGenerators
    node2.prev = node1
    node2.tail = node2Tail
    if (!node2Tail.isImmutable) { node2Tail.asInstanceOf[BSGSMutableNode[P]].prev = node2 }
    node2.beta = node2Beta
    node2.clearTransversalAndReplaceOwnGenerators(node2OwnGenerators)

    node1.strongGeneratingSetPairs.foreach { ip => node1.updateTransversal(ip) }
    node2.strongGeneratingSetPairs.foreach { ip => node2.updateTransversal(ip) }

    (node1, node2, (sizesProduct / node1.orbitSize).toInt)
  }

  def baseSwap(node: BSGSMutableNode[P])(implicit options: BSGSOptions) = options.algorithmType match {
    case Deterministic => deterministicBaseSwap(node)
    case Randomized => randomizedBaseSwap(node, options.randomGenerator)
  }

  /** Deterministic base swap.
    * 
    * @param node First base point to swap with its tail.
    * 
    * @return the two swapped mutable nodes.
    * 
    * Based on Derek Holt "Handbook of Computational Group Theory", 2005, page 103.
    * Note that their line 3 is wrong, betaT has to be used instead of betaT1.
    * 
    * See also http://www.math.uni-rostock.de/~rehn/docs/diploma-thesis-cs-rehn.pdf for an alternate
    * implementation.
    */
  def deterministicBaseSwap(node: BSGSMutableNode[P])(implicit builder: BSGSMutableNodeBuilder): (BSGSMutableNode[P], BSGSMutableNode[P]) = {
    import OrbitInstances._
    val nodeCopy = builder(node, None)
    val gammaSet = MutableBitSet.empty ++ node.orbit
    val (node1, node2, sizeGoal2) = baseSwapBase(node)
    gammaSet -= node1.beta
    gammaSet -= node2.beta
    while (node2.orbitSize < sizeGoal2) {
      val gamma = gammaSet.head
      val ipx@InversePair(x, xInv) = nodeCopy.uPair(gamma)
      val b = node2.beta <|+| xInv
      if (!node2.inOrbit(b))
        gammaSet --= ImmutableBitSet(gamma) <|+| node1.strongGeneratingSet
      else {
        val ipy = node2.uPair(b)
        val ipyx = ipy |+| ipx
        if (!node2.inOrbit(node2.beta <|+| ipyx)) {
          node2.addToOwnGenerators(ipyx)
          node2.updateTransversal(ipyx)
          node1.updateTransversal(ipyx)
          gammaSet --= ImmutableBitSet(node2.beta) <|+| node1.strongGeneratingSet
        }
        // TODO: what happens if node2.inOrbit is true ?
        // gamma will not be removed from gammaSet
        // and deterministicBaseSwap could loop forever
      }
    }
    (node1, node2)
  }

  /** Randomized base swap.
    * 
    * @param node First base point to swap with its tail.
    * 
    * @return the two swapped mutable nodes.
    * 
    * Based on algorithm 2.8 of http://www.math.uni-rostock.de/~rehn/docs/diploma-thesis-cs-rehn.pdf .
    */
  def randomizedBaseSwap(node: BSGSMutableNode[P], rand: Random)(implicit builder: BSGSMutableNodeBuilder): (BSGSMutableNode[P], BSGSMutableNode[P]) = {
    val nodeCopy = builder(node, None)
    val (node1, node2, sizeGoal2) = baseSwapBase(node)
    while (node2.orbitSize < sizeGoal2) {
      val g = nodeCopy.randomU(rand)
      val h = g |+| node1.uInv(node1.beta <|+| g)
      val hPair = InversePair(h, h.inverse)
      if (!node2.inOrbit(node2.beta <|+| h)) {
        node2.addToOwnGenerators(hPair)
        node2.updateTransversal(hPair)
        node1.updateTransversal(hPair)
      }
    }
    (node1, node2)
  }
}

object BSGSBuilder {
  def empty[P: Permutation] = new BSGSBuilder
  /** Constructs a BSGS builder from an immutable BSGS chain. */
  def apply[P](immutableBSGS: BSGS[P]): BSGSBuilder[P] = {
    assert(immutableBSGS.isImmutable)
    implicit def algebra = immutableBSGS.algebra
    val builder = empty
    builder.start = immutableBSGS
    builder
  }

  def fromBase[P: Permutation](base: Iterable[Int])(implicit nb: BSGSMutableNodeBuilder): BSGSBuilder[P] = {
    val builder = empty
    base.foreach( builder.append(_) )
    builder
  }

  def fromGenerators[P: Permutation](generators: Iterable[P], base: Iterable[Int] = Iterable.empty) = {
    val builder = fromBase(base)
    builder.addGenerators(generators)
    builder
  }
}
