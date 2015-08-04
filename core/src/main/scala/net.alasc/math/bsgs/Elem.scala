package net.alasc.math
package bsgs

import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.reflect.ClassTag

import scala.collection
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.syntax.subgroup._
import net.alasc.syntax.monoid._

/** Generic element to describe BSGS data. */
sealed trait Elem[P]

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

final class StrongGeneratingSetIterator[P](private[this] var chain: Chain[P], private[this] var index: Int, private[this] var inverse: Boolean) extends Iterator[P] {
  @inline @tailrec def hasNext: Boolean = chain match {
    case term: Term[P] => false
    case node: Node[P] =>
      if (index == node.nOwnGenerators) {
        index = 0
        chain = node.next
        hasNext
      } else true
  }
  def next: P = if (hasNext) {
    val node = chain.asInstanceOf[Node[P]]
    val p = if (inverse) node.ownGeneratorInv(index) else node.ownGenerator(index)
    index += 1
    p
  } else Iterator.empty.next
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

  /** Tests whether this chain is trivial, i.e. describes the trivial group. */
  def isTrivial: Boolean = ChainRec.isTrivial(chain)

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
          cforRange(0 until node.nOwnGenerators)( i => f(node.ownGenerator(i)) )
          rec(node.next)
        case _: Term[P] =>
      }
      rec(chain)
    }
    def iterator = new StrongGeneratingSetIterator[P](chain, 0, false)
  }

  def strongGeneratingSetInv: Iterable[P] = new Iterable[P] {
    def iterator = new StrongGeneratingSetIterator[P](chain, 0, true)
  }

  def length: Int = ChainRec.length(chain)

  def base: Seq[Int] = ChainRec.base(chain)

  def baseEquals(baseToCheck: Seq[Int]) = ChainRec.baseEquals(chain, baseToCheck.iterator)

  def basicSift(p: P)(implicit finiteGroup: FiniteGroup[P], equality: Eq[P]): (Seq[Int], P) = ChainRec.basicSift(chain, p)

  def sifts(p: P)(implicit finiteGroup: FiniteGroup[P], equality: Eq[P]): Boolean = ChainRec.sifts(chain, p)

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
  implicit def ChainSubgroup[P: ClassTag: Eq: FiniteGroup]: Subgroup[Chain[P], P] = new ChainSubgroup[P]
  implicit def ChainCheck[P: ClassTag: Eq: FiniteGroup]: Check[Chain[P]] = new ChainCheck[P]
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
    * ownGenerator(0 .. numOwnGenerators - 1) contains all the strong generators g that
    *  have beta(i) <|+| g = beta(i) for i < m, and beta(m) <|+| g =!= beta(m).
    */
  def ownGenerator(i: Int): P
  /** Inverses of own generators, order of inverses corresponding to `generatorsArray`. */
  def ownGeneratorInv(i: Int): P
  def nOwnGenerators: Int

  def ownGenerators: IndexedSeq[P] = new IndexedSeq[P] {
    def apply(i: Int) = ownGenerator(i)
    def length = nOwnGenerators
  }

  def randomOrbit(rand: Random): Int

  def foreachU(f: P => Unit): Unit
  def u(b: Int): P
  def uInv(b: Int): P
  def randomU(rand: Random): P
}

object Node {
  def trivial[P](beta: Int, next: Chain[P] = Term[P])(implicit action: FaithfulPermutationAction[P], ev: FiniteGroup[P], ct: ClassTag[P]): Node[P] =
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

  /** Adds `newGenerators` (given with their inverses) to this node `ownGenerators`,
    * without changing other nodes or updating any transversals. */
  protected[bsgs] def addToOwnGenerators(newGens: Iterable[P], newGensInv: Iterable[P])(implicit ev: FiniteGroup[P], ct: ClassTag[P]): Unit
  protected[bsgs] def addToOwnGenerators(newGen: P, newGenInv: P)(implicit ev: FiniteGroup[P], ct: ClassTag[P]): Unit

  /** Updates this node transversal by the addition of `newGens`,
    * provided with their inverses.
    * 
    * @note `newGens` must be already part of the strong generating set, i.e.
    *       have been added to this node or a children node `ownGenerators`
    *       by using addToOwnGenerators.
    */
  protected[bsgs] def updateTransversal(newGens: Iterable[P], newGensInv: Iterable[P])(implicit ev: FiniteGroup[P]): Unit
  protected[bsgs] def updateTransversal(newGen: P, newGenInv: P)(implicit ev: FiniteGroup[P]): Unit

  /** Conjugates the current node by the group element `g`, provided with its inverse
    * `gInv` to avoid multiple inverse element computations. 
    */
  protected[bsgs] def conjugate(g: P, gInv: P)(implicit ev: FiniteGroup[P], ct: ClassTag[P]): Unit
}

final class ChainSubgroup[P](implicit val equality: Eq[P], val finiteGroup: FiniteGroup[P]) extends Subgroup[Chain[P], P] {
  def iterator(chain: Chain[P]) = ChainRec.elementsIterator(chain)
  def order(chain: Chain[P]): BigInt = ChainRec.order(chain, BigInt(1))
  def generators(chain: Chain[P]): Iterable[P] = chain.strongGeneratingSet
  def randomElement(chain: Chain[P], rand: Random) = chain.mapOrElse(node => ChainRec.random(node.next, rand, node.randomU(rand)), Group[P].id)
  def contains(chain: Chain[P], g: P) = chain.sifts(g)
}
