package net.alasc

import scala.util.Random
import scala.math.max
import scala.annotation.tailrec

case class ChainContext(
  val useRandomizedAlgorithms: Boolean,
  val randomGenerator: scala.util.Random,
  val transversalBuilder: TransversalBuilder,
  val orbitBuilder: OrbitBuilder,
  val baseChangeStrategy: BaseChangeStrategy)

object Chain {
  val defaultContext = ChainContext(
    useRandomizedAlgorithms = true,
    randomGenerator = scala.util.Random,
    transversalBuilder = TransversalExplicit,
    orbitBuilder = OrbitSet,
    baseChangeStrategy = BaseSwapAndConjugation)
}

/** Result of a sift through the stabilizer chain.
  * 
  * Is constructed by the `sift` method of `Chain`.
  */
case class Sifted[E <: Finite[E], R <: Finite[R]](val elements: List[E], val remaining: R)

sealed trait Chain[F <: Finite[F]] {
  chain =>

  /** The faithful permutation action used to construct the stabilizer chain. */
  def action: Action[F]

  /** Returns the identity. */
  def identity: F = action.identity

  /** Tests if this node is the terminal node of chain. */ 
  def isTerminal: Boolean

  /** The strong generators for this particular level of the chain. */
  def ownGenerators: Seq[F]

  def strongGenerators: Seq[F] = for {
    node <- toStream
    generator <- node.ownGenerators
  } yield generator

  /** The base of the stabilizer chain. */
  def base: List[Dom] = baseRec(Nil)
  @tailrec final def baseRec(reversedAcc: List[Dom]): List[Dom] = this match {
    case terminal: Terminal[F] => reversedAcc.reverse
    case node: Node[F] => node.tail.baseRec(node.beta :: reversedAcc)
  }

  /** The length of the stabilizer chain. */
  def length: Int = lengthRec(0)
  @tailrec final protected def lengthRec(acc: Int): Int = this match {
    case terminal: Terminal[F] => acc
    case node: Node[F] => node.tail.lengthRec(acc + 1)
  }

  /** The order of the group represented by the stabilizer chain. */
  def order: BigInt = orderRec(BigInt(1))
  @tailrec final protected def orderRec(acc: BigInt): BigInt = this match {
    case terminal: Terminal[F] => acc
    case node: Node[F] => node.tail.orderRec(acc * node.transversal.size)
  }

  /** Returns the chain as a `Stream` of `Node`. */
  def toStream: Stream[Node[F]] = this match {
    case terminal: Terminal[F] => Stream.empty
    case node: Node[F] => node #:: node.tail.toStream
  }

  /** Returns an iterator throught the elements of the group defined by this chain. */
  def elements: Iterator[F] = this match {
    case terminal: Terminal[F] => Iterator(identity)
    case node: Node[F] => for {
      rest <- node.tail.elements
      b <- node.transversal.keysIterator
    } yield rest * node.transversal(b).u
  }

  /** Returns an element of the group defined by this chain at random. */
  def random(implicit gen: Random = Random): F = this match {
    case terminal: Terminal[F] => identity
    case node: Node[F] => node.tail.random(gen) * node.transversal.random(gen)
  }

  def contains(f: F): Boolean = sift(f).remaining.isIdentity

  /** Sifts the element `f` through this chain and returns a `Sifted` result. */
  def sift(f: F): Sifted[F, F] = siftRec(Sifted(Nil, f))
  @tailrec final protected def siftRec(sifted: Sifted[F, F]): Sifted[F, F] = this match {
    case terminal: Terminal[F] => sifted
    case node: Node[F] =>
      val b = action(sifted.remaining, node.beta)
      if (!node.transversal.isDefinedAt(b))
        sifted
      else {
        val TEntry(u, uinv) = node.transversal(b)
        node.tail.siftRec(Sifted(u :: sifted.elements, sifted.remaining * uinv))
      }
  }

  def siftPerm(p: Perm): Sifted[F, Perm] = siftPermRec(Sifted(Nil, p))
  @tailrec final protected def siftPermRec(sifted: Sifted[F, Perm]): Sifted[F, Perm] = this match {
    case terminal: Terminal[F] => sifted
    case node: Node[F] =>
      val b = sifted.remaining.image(node.beta)
      if (!node.transversal.isDefinedAt(b))
        sifted
      else {
        val TEntry(u, uinv) = node.transversal(b)
        node.tail.siftPermRec(Sifted(u :: sifted.elements, sifted.remaining * action.toPerm(uinv)))
      }
  }

  /** Returns the current chain conjugated by the element `f`, i.e. if this chain described the
    * group `G`, the conjugated chain describes the group `finv G f`.
    */
  def conjugatedBy(f: F): Chain[F]

  def removingRedundantBasePoints: Chain[F]
}

trait Node[F <: Finite[F]] extends Chain[F] {
  def isTerminal = false
  def transversal: Transversal[F]
  def beta: Dom = transversal.beta
  def tail: Chain[F]
}

case class Terminal[F <: Finite[F]](action: Action[F]) extends Chain[F] {
  def isTerminal = true
  def ownGenerators = Nil
  def conjugatedBy(f: F) = this
  def removingRedundantBasePoints = this
}

final class NodeBuilder[F <: Finite[F]](val action: Action[F],
  private[alasc] var myTransversal: Transversal[F],
  private[alasc] var myOwnGenerators: Seq[F],
  private[alasc] var myTail: Chain[F]) extends Node[F] {

  def this(action: Action[F], beta: Dom)(implicit context: ChainContext) = this(action, context.transversalBuilder.empty(beta, action), Nil, Terminal(action))

  private var isExported: Boolean = false

  def tail = myTail
  def transversal = myTransversal
  def ownGenerators = myOwnGenerators

  /** Exports this `NodeBuilder` as an immutable `Node`. */
  def export: Node[F] = {
    exportRec
    this
  }

  private def exportRec {
    myTail match {
      case terminal: Terminal[F] =>
      case node: NodeBuilder[F] =>
        assert(!isExported)
        isExported = true
        node.exportRec
      case _ => throw new IllegalArgumentException("Cannot export a standard node.")
    }
  }

  def conjugatedBy(f: F) = {
    if (!f.isIdentity) {
      val newChain = new NodeBuilder(action, beta)
      newChain.conjugatedByRec(f, f.inverse, this)
      newChain.export
    } 
    else this
  }
  @tailrec private final def conjugatedByRec(f: F, finv: F, node: Node[F]) {
    myTransversal = node.transversal.conjugatedBy(f, finv)
    myOwnGenerators = node.ownGenerators.map(x => finv*x*f)
    node.tail match {
      case tail: Terminal[F] =>
      case tail: Node[F] =>
        val newTail = new NodeBuilder(action, tail.beta)
        myTail = newTail
        newTail.conjugatedByRec(f, finv, tail)
    }
  }

  def removingRedundantBasePoints = {

  }

  @tailrec private final def removingRedundantBasePointsRec(node: Node[F], changed: Boolean = false): Boolean = {
    node.transversal.size match {
      case 1 =>
        node.tail match {
          case tail: Node[F] =>
            removingRedundantBasePointsRec(tail, true)
          case tail: Terminal[F] =>
            myTail = tail
            changed
        }
      case _ =>
        node.tail match {
          case tail: Node[F] =>
            val newTail = new NodeBuilder(action, tail.transversal, tail.)
            myTail = newTail
            newTail.removingRedundantBasePointsRec(tail, changed)
          case tail: Terminal[F] =>
            myTail = tail
            changed
        }
    }
  }
}
