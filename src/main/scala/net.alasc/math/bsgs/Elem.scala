package net.alasc.math
package bsgs

import scala.language.implicitConversions
import scala.annotation.tailrec

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
  implicit def action: PermutationAction[P]
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

class Start[P](var next: Chain[P])(implicit val action: PermutationAction[P]) extends MutableStartOrNode[P] {
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

  /** Seq starting with this element and going for the next nodes of the chain, in order. */
  def nodesNext: Seq[Node[P]] = new NextSeq[P](chain)

  /** Maps the function `f` is this chain element is a node, or returns the default value. */
  def mapOrElse[A](f: Node[P] => A, default: => A): A = chain match {
    case node: Node[P] => f(node)
    case _ => default
  }

  /** Returns the strong generating set for the BSGS chain starting from this node.
    * 
    * @note The strong generating set is stored piece by piece by having each
    *       node storing explicitly only the generators appearing at its level.
    */
  def strongGeneratingSet: Iterable[P] = nodesNext.flatMap(_.ownGenerators)

  def strongGeneratingSetPairs: Iterable[InversePair[P]] = nodesNext.flatMap(_.ownGeneratorsPairs)

  def length: Int = ChainRec.length(chain)

  def base: Seq[Int] = ChainRec.base(chain)

  def baseEquals(baseToCheck: Seq[Int]) = ChainRec.baseEquals(chain, baseToCheck.iterator)

  def basicSift(p: P)(implicit ev: FiniteGroup[P]): (Seq[Int], P) = ChainRec.basicSift(chain, p)

  def sifts(p: P)(implicit ev: FiniteGroup[P]): Boolean = ChainRec.sifts(chain, p)
}

object Chain {
  implicit def ChainSubgroup[P](implicit algebra: FiniteGroup[P]): Subgroup[Chain[P], P] = new ChainSubgroup[P]
  implicit def ChainCheck[P](implicit algebra: FiniteGroup[P]): Check[Chain[P]] = new ChainCheck[P]
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
trait Node[P] extends Chain[P] with StartOrNode[P] {
  node =>
  /** Permutation action for the type `P`. */
  implicit def action: PermutationAction[P]

  def isTerminal = false
  def isStandalone: Boolean

  /** Tests whether this node is immutable. */
  def isImmutable: Boolean

  /** Tests whether this node is mutable. */
  def isMutable: Boolean

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
  def orbitSet: collection.BitSet = {
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

object Node {
  /** Extractor for `Node` from `Elem`. */
  def unapply[P](elem: Elem[P]): Option[Node[P]] = elem match {
    case node: Node[P] => Some(node)
    case _ => None
  }
}

/** Represents the end of a BSGS chain, or, when viewed as a group, the trivial group (). */
class Term[P] extends Chain[P] {
  def isTerminal = true
}

object Term {
  val instance = new Term[Nothing]
  def apply[P] = instance.asInstanceOf[Term[P]]
}

trait MutableNode[P] extends Node[P] with MutableStartOrNode[P] {
  def isImmutable = prev eq null
  def isStandalone = (prev eq null) && (next eq null)
  def isMutable = (prev ne null)

  def prev: MutableStartOrNode[P]
  protected[bsgs] def prev_= (value: MutableStartOrNode[P]): Unit

  /** Makes the current mode immutable. */
  protected[bsgs] def makeImmutable: Unit = {
    next.mapOrElse(node => assert(node.isImmutable), ())
    prev = null
  }

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
  def elements(chain: Chain[P]): Iterable[P] = new Iterable[P] {
    override def stringPrefix = "Elements"
    def iterator = ChainRec.elementsIterator(chain)
  }
  def order(chain: Chain[P]): BigInt = ChainRec.order(chain, BigInt(1))
  def generators(chain: Chain[P]): Iterable[P] = chain.strongGeneratingSet
  def randomElement(chain: Chain[P], rand: Random) = chain.mapOrElse(node => ChainRec.random(node.next, rand, node.randomU(rand)), algebra.id)
  override def contains(chain: Chain[P], g: P) = chain.sifts(g)
}

final class ChainCheck[P](implicit val algebra: FiniteGroup[P]) extends Check[Chain[P]] {
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
