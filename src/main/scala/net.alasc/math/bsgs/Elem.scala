package net.alasc.math
package bsgs

import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.reflect.ClassTag

import scala.collection
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Random

import spire.algebra.Group
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._

/** Generic element to describe BSGS data. */
sealed trait Elem[P] extends AnyRef

sealed trait StartOrNode[P] extends Elem[P] { elem =>
  implicit def action: FaithfulPermutationAction[P]
  def next: Chain[P]
}

final class PrevSeq[P](val elem: MutableStartOrNode[P]) extends Seq[MutableNode[P]] {
  override def foreach[U](f: MutableNode[P] => U): Unit = MutableChainRec.foreach(elem, f)
  def length = iterator.length
  def apply(k: Int) = iterator.drop(k).next
  def iterator: Iterator[MutableNode[P]] = new Iterator[MutableNode[P]] {
    private var cursor: MutableStartOrNode[P] = elem
    def hasNext = !cursor.isInstanceOf[Start[P]]
    def next = cursor match {
      case IsMutableNode(mn) =>
        cursor = mn.prev
        mn
      case _: Node[P] => sys.error("An immutable node cannot precede a mutable node in the chain.")
      case _ => Iterator.empty.next
    }
  }
  override def isEmpty = elem.isInstanceOf[Start[P]]
  override def head: MutableNode[P] = elem match {
    case IsMutableNode(prev) => prev
    case _ => throw new NoSuchElementException("Empty seq")
  }
  override def tail: PrevSeq[P] = elem match {
    case IsMutableNode(prev) => new PrevSeq(prev)
    case _ => throw new UnsupportedOperationException("Empty seq")
  }
}

final class NextSeq[P](val chain: Chain[P]) extends Seq[Node[P]] {
  override def foreach[U](f: Node[P] => U): Unit = ChainRec.foreach(chain, f)
  def length = ChainRec.length(chain)
  def apply(k: Int) = iterator.drop(k).next
  def iterator: Iterator[Node[P]] = new Iterator[Node[P]] {
    private var cursor: Chain[P] = chain
    def hasNext = !cursor.isTerminal
    def next = cursor match {
      case node: Node[P] =>
          cursor = node.next
        node
      case _ => Iterator.empty.next
    }
  }
  override def isEmpty = chain.isInstanceOf[Term[P]]
  override def head: Node[P] = chain match {
    case node: Node[P] => node
    case _ => throw new NoSuchElementException("Empty seq")
  }
  override def tail: NextSeq[P] = chain match {
    case node: Node[P] => new NextSeq[P](node.next)
    case _ => throw new UnsupportedOperationException("Empty seq")
  }
}

sealed trait MutableStartOrNode[P] extends StartOrNode[P] { elem =>
  protected[bsgs] def next_= (value: Chain[P]): Unit

  /** Seq starting with this element and going for the previous nodes of the chain, in order. */
  def nodesPrev: Seq[MutableNode[P]] = new PrevSeq[P](elem)
}

class Start[P](var next: Chain[P])(implicit val action: FaithfulPermutationAction[P]) extends MutableStartOrNode[P] {
  /** Pretty prints the builder, while doing basic chain consistency checks. */
  override def toString = {
    import scala.collection.mutable.StringBuilder
    var sb = new StringBuilder
    sb ++= "()"
    @tailrec def rec(chain: Chain[P]): Unit = chain match {
      case IsMutableNode(mn) =>
        sb ++= s" <-> ${mn.beta}(${mn.orbitSize})"
        rec(mn.next)
      case node: Node[P] =>
        sb ++= s" -> ${node.beta}(${node.orbitSize})"
        rec(node.next)
      case _: Term[P] =>
        sb ++= " -> ()"
    }
    rec(next)
    sb.mkString
  }
}

/** Base class for elements in a BSGS chain, i.e. nodes or terminal elements, implementing
  * the chain as a single-linked list, with a double-linked list for mutable elements.
  */
sealed trait Chain[P] extends Elem[P] {
  chain =>

  override def toString = nodesNext.map { node => s"${node.beta}(${node.orbitSize})" }.mkString(" -> ")

  /** Tests whether this element terminates the BSGS chain. */
  def isTerminal: Boolean

  /** Tests whether this node is immutable. */
  def isImmutable: Boolean

  /** Tests whether this node is mutable. */
  def isMutable: Boolean

  /** Seq starting with this element and going for the next nodes of the chain, in order. */
  def nodesNext: Seq[Node[P]] = new NextSeq[P](chain)

  /** Maps the function `f` is this chain element is a node, or returns the default value. */
  def mapOrElse[A](f: Node[P] => A, default: => A): A = chain match {
    case node: Node[P] => f(node)
    case _ => default
  }

  /** Tests whether if `k` is fixed by the group described by this chain. */
  def isFixed(k: Int): Boolean = ChainRec.isFixed(chain, k)

  /** Returns the strong generating set for the BSGS chain starting from this node.
    * 
    * @note The strong generating set is stored piece by piece by having each
    *       node storing explicitly only the generators appearing at its level.
    */
  def strongGeneratingSet: Iterable[P] = new Iterable[P] {
    override def foreach[U](f: P => U): Unit = {
      @tailrec def rec(current: Chain[P]): Unit = current match {
        case node: Node[P] =>
          node.ownGeneratorsPairs.foreach( ip => f(ip.g) )
          rec(node.next)
        case _: Term[P] =>
      }
      rec(chain)
    }
    def iterator = chain match {
      case _: Term[P] => Iterator.empty
      case thisNode: Node[P] => new Iterator[P] {
        var node: Node[P] = thisNode
        def hasNextNode = !node.next.isTerminal
        var nodeIterator = node.ownGeneratorsPairs.iterator
        def findNonEmptyIterator: Boolean = {
          while(!nodeIterator.hasNext) {
            node.next match {
              case nextNode: Node[P] => node = nextNode
              case _: Term[P] => return false
            }
            nodeIterator = node.ownGeneratorsPairs.iterator
          }
          true
        }
        def hasNext = nodeIterator.hasNext || findNonEmptyIterator
        def next: P = {
          if (!findNonEmptyIterator) return Iterator.empty.next
          nodeIterator.next.g
        }
      }
    }
  }

  /** Returns the strong generating set pairs for the BSGS chain starting from this node. */
  def strongGeneratingSetPairs: Iterable[InversePair[P]] = new Iterable[InversePair[P]] {
    override def foreach[U](f: InversePair[P] => U): Unit = {
      @tailrec def rec(current: Chain[P]): Unit = current match {
        case node: Node[P] =>
          node.ownGeneratorsPairs.foreach(f)
          rec(node.next)
        case _: Term[P] =>
      }
      rec(chain)
    }
    def iterator = chain match {
      case _: Term[P] => Iterator.empty
      case thisNode: Node[P] => new Iterator[InversePair[P]] {
        var node: Node[P] = thisNode
        def hasNextNode = !node.next.isTerminal
        var nodeIterator = node.ownGeneratorsPairs.iterator
        def findNonEmptyIterator: Boolean = {
          while(!nodeIterator.hasNext) {
            node.next match {
              case nextNode: Node[P] => node = nextNode
              case _: Term[P] => return false
            }
            nodeIterator = node.ownGeneratorsPairs.iterator
          }
          true
        }
        def hasNext = nodeIterator.hasNext || findNonEmptyIterator
        def next: InversePair[P] = {
          if (!findNonEmptyIterator) return Iterator.empty.next
          nodeIterator.next
        }
      }
    }
  }

  def length: Int = ChainRec.length(chain)

  def base: Seq[Int] = ChainRec.base(chain)

  def baseEquals(baseToCheck: Seq[Int]) = ChainRec.baseEquals(chain, baseToCheck.iterator)

  def basicSift(p: P)(implicit ev: FiniteGroup[P]): (Seq[Int], P) = ChainRec.basicSift(chain, p)

  def sifts(p: P)(implicit ev: FiniteGroup[P]): Boolean = ChainRec.sifts(chain, p)

  /** If the current element is a node, returns the next stabilizer group in chain and the current node
    * viewed as a transversal. If the current element is a terminal, creates and returns an empty transversal with
    * base point `beta`.
    */
  def detach(beta: => Int)(implicit ev: FiniteGroup[P]): (Chain[P], Transversal[P]) = chain match {
    case node: Node[P] => (node.next, node)
    case term: Term[P] => (term, Transversal.empty(beta))
  }
}

object Chain {
  implicit def ChainSubgroup[P: ClassTag: FiniteGroup]: Subgroup[Chain[P], P] = new ChainSubgroup[P]
  implicit def ChainCheck[P: ClassTag: FiniteGroup]: Check[Chain[P]] = new ChainCheck[P]
}

/** Node in a BSGS chain.
  * 
  * A `Node` can be in three different states:
  * 
  * - stand-alone if both `next` and `prev` are null and/or unavailable,
  * - in a chain if `next` points to the next node in the chain,
  * - immutable if `next` points to the next node in the chain but `prev` is null and/or unavailable,
  * - mutable if `next` and `prev` point to the next and previous nodes in the chain. Note that if
  *   a node is mutable, then the previous node is mutable as well.
  * 
  * The set of strong generators is represented by storing with each node only the strong generators that stabilize
  * the previous base points, but not the current base point.
  */
trait Node[P] extends Chain[P] with StartOrNode[P] with Transversal[P] {
  node =>
  /** Permutation action for the type `P`. */
  implicit def action: FaithfulPermutationAction[P]

  def isTerminal = false
  def isStandalone: Boolean

  def next: Chain[P]
  def beta: Int

  /** If the base is beta(1) -> ... -> beta(m-1) -> beta(m) current base -> tail.beta,
    * ownGenerators contains all the strong generators g that have beta(i) <|+| g = beta(i) for i < m,
    * and beta(m) <|+| g =!= beta(m).
    */
  def ownGenerators: Iterable[P]
  def ownGeneratorsPairs: Iterable[InversePair[P]]

  def orbitSize: Int

  def inOrbit(b: Int): Boolean
  def orbit: Iterable[Int]
  def foreachOrbit[U](f: Int => U): Unit
  def orbitSet: Set[Int] = {
    val bitset = mutable.BitSet.empty
    foreachOrbit { bitset += _ }
    bitset.toImmutable
  }
  def randomOrbit(rand: Random): Int

  def iterable: Iterable[(Int, InversePair[P])]
  def foreachU[N](f: P => N): Unit
  def uPair(b: Int): InversePair[P]
  def u(b: Int): P
  def uInv(b: Int): P
  def randomU(rand: Random): P
}

case class TrivialNode[P](beta: Int, id: P, next: Chain[P])(implicit val action: FaithfulPermutationAction[P]) extends Node[P] {
  def isImmutable = true
  def isMutable = false
  def foreachOrbit[U](f: Int => U) = f(beta)
  def foreachU[N](f: P => N): Unit = f(id)
  def inOrbit(b: Int) = b == beta
  def isStandalone = false
  def iterable = Iterable(beta -> InversePair(id, id))
  def orbit = Iterable(beta)
  def orbitSize = 1
  def ownGenerators = Iterable.empty
  def ownGeneratorsPairs = Iterable.empty
  def randomOrbit(rand: Random) = beta
  def randomU(rand: Random) = id
  def u(b: Int) = if (b == beta) id else sys.error("Not in orbit")
  def uInv(b: Int) = if (b == beta) id else sys.error("Not in orbit")
  def uPair(b: Int) = if (b == beta) InversePair(id, id) else sys.error("Not in orbit")
}

object Node {
  def trivial[P](beta: Int, next: Chain[P] = Term[P])(implicit action: FaithfulPermutationAction[P], ev: FiniteGroup[P]): Node[P] =
    new TrivialNode[P](beta, ev.id, next)
  /** Extractor for `Node` from `Elem`. */
  def unapply[P](elem: Elem[P]): Option[Node[P]] = elem match {
    case node: Node[P] => Some(node)
    case _ => None
  }
}

/** Represents the end of a BSGS chain, or, when viewed as a group, the trivial group (). */
class Term[P] extends Chain[P] {
  def isTerminal = true
  def isImmutable = true
  def isMutable = false
}

object Term {
  val instance = new Term[Nothing]
  def apply[P] = instance.asInstanceOf[Term[P]]
}

trait MutableNode[P] extends Node[P] with MutableStartOrNode[P] {
  override def toString = if (isStandalone) s"Node ($beta) orbit $orbit" else super.toString
  def isImmutable = prev eq null
  def isStandalone = (prev eq null) && (next eq null)
  def isMutable = (prev ne null)

  def prev: MutableStartOrNode[P]
  protected[bsgs] def prev_= (value: MutableStartOrNode[P]): Unit

  /** Makes the current node immutable. */
  protected[bsgs] def makeImmutable: Unit = {
    next.mapOrElse(node => assert(node.isImmutable), ())
    prev = null
  }

  protected[bsgs] def removeRedundantGenerators: Unit

  /** Adds `newGenerators` (given as a traversable of `InversePair`) to this node `ownGenerators`,
    * without changing other nodes or updating any transversals. */
  protected[bsgs] def addToOwnGenerators(newGenerators: Traversable[InversePair[P]])(implicit ev: FiniteGroup[P]): Unit
  protected[bsgs] def addToOwnGenerators(newGenerator: InversePair[P])(implicit ev: FiniteGroup[P]): Unit

  /** Updates this node transversal by the addition of `newGenerators`,
    * provided as a traversable `InversePair`.
    * 
    * @note `newGenerators` must be already part of the strong generating set, i.e.
    *       have been added to this node or a children node `ownGenerators`
    *       by using addToOwnGenerators.
    */
  protected[bsgs] def updateTransversal(newGenerators: Traversable[InversePair[P]])(implicit ev: FiniteGroup[P]): Unit
  protected[bsgs] def updateTransversal(newGenerator: InversePair[P])(implicit ev: FiniteGroup[P]): Unit

  /** Conjugates the current node by the group element `ip`, provided as an input pair
    * to avoid multiple inverse element computations. */
  protected[bsgs] def conjugate(ip: InversePair[P])(implicit ev: FiniteGroup[P]): Unit
}

final class ChainSubgroup[P](implicit val algebra: FiniteGroup[P]) extends Subgroup[Chain[P], P] {
  def iterator(chain: Chain[P]) = ChainRec.elementsIterator(chain)
  def order(chain: Chain[P]): BigInt = ChainRec.order(chain, BigInt(1))
  def generators(chain: Chain[P]): Iterable[P] = chain.strongGeneratingSet
  def randomElement(chain: Chain[P], rand: Random) = chain.mapOrElse(node => ChainRec.random(node.next, rand, node.randomU(rand)), algebra.id)
  def contains(chain: Chain[P], g: P) = chain.sifts(g)
}

final class ChainCheck[P](implicit pClassTag: ClassTag[P], algebra: FiniteGroup[P]) extends Check[Chain[P]] {
  def check(chain: Chain[P]): Unit = {
    chain match {
      case node: Node[P] =>
        implicit def action = node.action
        val alg = algorithms.BasicAlgorithms.deterministic[P]
        val checkChain = alg.completeChainFromGenerators(chain.strongGeneratingSet, chain.base)
        assert(checkChain.start.next.nodesNext.map(_.orbitSize).sameElements(node.nodesNext.map(_.orbitSize)))
      case _ =>
    }
    val baseSoFar = mutable.ArrayBuffer.empty[Int]
    @tailrec def rec(current: Chain[P], checkImmutable: Boolean = false): Unit = current match {
      case node: Node[P] =>
        implicit def action = node.action
        for (g <- node.ownGenerators; b <- baseSoFar)
          assert((b <|+| g) == b)
        for (g <- node.ownGenerators)
          assert((node.beta <|+| g) != node.beta)
        baseSoFar += node.beta
        if (checkImmutable)
          assert(node.isImmutable)
        rec(node.next, node.isImmutable)
      case _: Term[P] =>
    }
  }
}
