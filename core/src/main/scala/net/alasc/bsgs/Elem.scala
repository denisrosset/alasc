package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random
import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt
import net.alasc.algebra._
import net.alasc.bsgs.internal.StrongGeneratingSetIndexedSeq
import net.alasc.syntax.group._

/** Generic element to describe BSGS data.
  * @tparam G Group element type
  * @tparam A Permutation action for `G`
  *  */
sealed trait Elem[G, A <: PermutationAction[G] with Singleton]

sealed trait StartOrNode[G, A <: PermutationAction[G] with Singleton] extends Elem[G, A] {

  implicit def action: A

  /** Next element in this BSGS chain. */
  def next: Chain[G, A]

}

sealed trait MutableStartOrNode[G, A <: PermutationAction[G] with Singleton] extends StartOrNode[G, A] {

  protected[bsgs] def next_= (value: Chain[G, A]): Unit

}

final class Start[G, A <: PermutationAction[G] with Singleton](var next: Chain[G, A])
                                                       (implicit val action: A) extends MutableStartOrNode[G, A] {
  /** Pretty prints the builder, while doing basic chain consistency checks. */
  override def toString = {
    import scala.collection.mutable.StringBuilder
    var sb = new StringBuilder
    sb ++= "()"
    @tailrec def rec(chain: Chain[G, A]): Unit = chain match {
      case IsMutableNode(mn) =>
        sb ++= s" <-> ${mn.beta}(${mn.orbitSize})"
        rec(mn.next)
      case node: Node[G, A] =>
        sb ++= s" -> ${node.beta}(${node.orbitSize})"
        rec(node.next)
      case _: Term[G, A] =>
        sb ++= " -> ()"
    }
    rec(next)
    sb.mkString
  }

}

/** Base class for elements in a BSGS chain, i.e. nodes or terminal elements, implementing
  * the chain as a single-linked list, with a double-linked list for mutable elements.
  */
sealed trait Chain[G, A <: PermutationAction[G] with Singleton] extends Elem[G, A] {
  chain =>

  override def toString = nodesIterator.map { node => s"${node.beta}(${node.orbitSize})" }.mkString(" -> ")

  /** Tests whether this element terminates the BSGS chain. */
  def isTerminal: Boolean

  /** Tests whether this node is immutable. */
  def isImmutable: Boolean

  /** Tests whether this node is mutable. */
  def isMutable: Boolean

  /** Tests whether this chain is trivial, i.e. describes the trivial group. */
  def isTrivial: Boolean = ChainRec.isTrivial(chain)

  /** Iterator through the nodes of this chain, including the current one if applicable. */
  def nodesIterator: Iterator[Node[G, A]] = new Iterator[Node[G, A]] {
    private var cursor: Chain[G, A] = chain
    def hasNext = !cursor.isTerminal
    def next = cursor match {
      case node: Node[G, A] =>
        cursor = node.next
        node
      case _ => Iterator.empty.next
    }
  }

  /** Maps the function `f` is this chain element is a node, or returns the default value. */
  def mapOrElse[B](f: Node[G, A] => B, default: => B): B = chain match {
    case node: Node[G, A] => f(node)
    case _ => default
  }

  /** Tests whether if `k` is fixed by the group described by this chain. */
  def isFixed(k: Int): Boolean = ChainRec.isFixed(chain, k)

  /** Returns the strong generating set for the BSGS chain starting from this node.
    * 
    * @note The strong generating set is stored piece by piece by having each
    *       node storing explicitly only the generators appearing at its level.
    */
  def strongGeneratingSet: IndexedSeq[G] = new StrongGeneratingSetIndexedSeq[G](chain)

  def nStrongGenerators: Int = ChainRec.nStrongGenerators(chain)

  def kthStrongGenerator(k: Int): G = ChainRec.kthStrongGenerator(chain, k)

  def elementsIterator(implicit group: Group[G]): Iterator[G]

  def order: SafeLong = ChainRec.order(chain, SafeLong(1))

  def randomElement(rand: Random)(implicit group: Group[G]): G

  def length: Int = ChainRec.length(chain)

  def base: Seq[Int] = ChainRec.base(chain)

  /** Tests whether the base of this chain is in the lexicographic order:
    *
    * - successive elements are increasing
    * - the base starts with the smallest moved point
    */
  def hasLexicographicBase: Boolean = ChainRec.hasLexicographicBase(chain)

  def baseEquals(baseToCheck: Seq[Int]) = ChainRec.baseEquals(chain, baseToCheck.iterator)

  def basicSift(g: G)(implicit group: Group[G], equ: Eq[G]): (Seq[Int], G) = ChainRec.basicSift(chain, g)

  /*
  def siftOther[Q:Eq:Group:PermutationAction](q: Q)(implicit group: Group[G], equ: Eq[G]): Opt[G] = chain match {
    case node: Node[G, A] =>
      implicit def action: A = node.action
      ChainRec.siftOther(chain, group.id, q)
    case _: Term[G, A] => if (q.isId) Opt(group.id) else Opt.empty
  }*/

  /** Sifts an element through the chain.
    *
    * @param g Element to sift through the chain.
    * @return  Opt(remainder) if the element can be sifted through the whole chain, otherwise Opt.empty.
    */
  def sift(g: G)(implicit group: Group[G]): Opt[G] = ChainRec.sift(chain, g)

  def siftsFaithful(g: G)(implicit group: Group[G], equ: Eq[G]): Boolean =
    ChainRec.sift(chain, g) match {
      case Opt(remainder) => remainder.isId
      case _ => false
    }

  def sifts(g: G, kernel: Chain.Generic[G])(implicit group: Group[G], equ: Eq[G]): Boolean =
    ChainRec.sift(chain, g) match {
      case Opt(rem1) => kernel.siftsFaithful(rem1)
      case _ => false
    }

  /** If the current element is a node, returns the next stabilizer group in chain and the current node
    * viewed as a transversal. If the current element is a terminal, creates and returns an empty transversal with
    * base point `beta`.
    */
  def detach(beta: => Int)(implicit group: Group[G]): (Chain[G, A], Transversal[G, A]) = chain match {
    case node: Node[G, A] => (node.next, node)
    case term: Term[G, A] => (term, Transversal.empty(beta))
  }

}

object Chain {

  type Generic[G] = Chain[G, A] forSome { type A <: PermutationAction[G] with Singleton }

  implicit def ChainCheck[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]: Check[Chain[G, A]] =
    new ChainCheck[G, A]

  /** Returns a `Chain[G, action.type]` if it can be shown that the provided Chain.Generic[G] is of that type.
    *
    * @param chainGen Chain to check the action of.
    * @param action   Action to match
    * @return         Opt(chain) if the chain is a Node with the given action, or a Term.
    */
  @inline final def extractGrpChain[G](chainGen: Chain.Generic[G], action: PermutationAction[G]): Opt[Chain[G, action.type]] =
  chainGen match {
    case node: Node[G, _] if node.action eq action => Opt(node.asInstanceOf[Node[G, action.type]])
    case term: Term[G, _] => Opt(term.asInstanceOf[Term[G, action.type]])
  }

  /** Returns the action that the given node and chain have in common, if any.
    *
    * @param lhs Node to test
    * @param rhs Chain to test
    * @return    Opt(action) if both the node and chain share it, otherwise Opt.empty.
    */
  @inline final def commonAction[G](lhs: Node.Generic[G], rhs: Chain.Generic[G]): Opt[PermutationAction[G]] =
  rhs match {
    case term: Term[G, _] => Opt(lhs.action)
    case node: Node[G, _] if node.action eq lhs.action => Opt(lhs.action)
    case _ => Opt.empty[PermutationAction[G]]
  }

  /** Casts the provided Chain.Generic[G] into Chain[G, action.type] for the provided action. The safety of the operation
    * is not verified. */
  @inline final def inActionUnsafe[G](lhs: Chain.Generic[G], action: PermutationAction[G]): Chain[G, action.type] = lhs.asInstanceOf[Chain[G, action.type]]

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
trait Node[G, A <: PermutationAction[G] with Singleton]
  extends Chain[G, A] with StartOrNode[G, A] with Transversal[G, A] {
  node =>
  /** Permutation action for the type `P`. */
  implicit def action: A

  def isTerminal = false
  def isStandalone: Boolean

  def next: Chain[G, A]
  def beta: Int

  /** If the base is beta(1) -> ... -> beta(m-1) -> beta(m) current base -> tail.beta,
    * ownGenerator(0 .. numOwnGenerators - 1) contains all the strong generators g that
    *  have beta(i) <|+| g = beta(i) for i < m, and beta(m) <|+| g =!= beta(m).
    */
  def ownGenerator(i: Int): G
  /** Inverses of own generators, order of inverses corresponding to `generatorsArray`. */
  def ownGeneratorInv(i: Int): G
  def nOwnGenerators: Int

  def ownGenerators: IndexedSeq[G] = new IndexedSeq[G] {
    def apply(i: Int) = ownGenerator(i)
    def length = nOwnGenerators
  }

  def elementsIterator(implicit group: Group[G]): Iterator[G] = for {
    rest <- next.elementsIterator
    b <- orbit.iterator
  } yield rest |+| u(b)

  def randomElement(rand: Random)(implicit group: Group[G]): G = ChainRec.random(node.next, rand, node.randomU(rand))

  def randomOrbit(rand: Random): Int

  def foreachU(f: G => Unit): Unit
  def u(b: Int): G
  def uInv(b: Int): G
  def randomU(rand: Random): G
}

object Node {

  /** Casts the provided Node.Generic[G] into Node[G, action.type] for the provided action. The safety of the operation
    * is not verified. */
  @inline final def inActionUnsafe[G](lhs: Node.Generic[G], action: PermutationAction[G]): Node[G, action.type] = lhs.asInstanceOf[Node[G, action.type]]

  type Generic[G] = Node[G, _ <: PermutationAction[G] with Singleton]

  def trivial[G:ClassTag:Group, A <: PermutationAction[G] with Singleton](beta: Int, next: Chain[G, A] = Term[G, A])
                                                                         (implicit action: A): Node[G, A] =
    new TrivialNode[G, A](beta, next)

  /** Extractor for `Node` from `Elem`. */
  def unapply[G, A <: PermutationAction[G] with Singleton](elem: Elem[G, A]): Option[Node[G, A]] = elem match {
    case node: Node[G, A] => Some(node)
    case _ => None
  }

}

/** Represents the end of a BSGS chain, or, when viewed as a group, the trivial group (). */
class Term[G, A <: PermutationAction[G] with Singleton] extends Chain[G, A] {

  def isTerminal = true
  def isImmutable = true
  def isMutable = false

  def elementsIterator(implicit group: Group[G]): Iterator[G] = Iterator(group.id)

  def randomElement(rand: Random)(implicit group: Group[G]): G = group.id

}

object Term {

  type Generic[G] = Term[G, _ <: PermutationAction[G] with Singleton]

  val instance = new Term[Nothing, Null]
  def apply[G, A <: PermutationAction[G] with Singleton] = instance.asInstanceOf[Term[G, A]]

  def generic[G]: Generic[G] = instance.asInstanceOf[Term.Generic[G]]

}

trait MutableNode[G, A <: PermutationAction[G] with Singleton]
  extends Node[G, A] with MutableStartOrNode[G, A] {

  override def toString = if (isStandalone) s"Node ($beta) orbit $orbit" else super.toString

  def isImmutable = prev eq null
  def isStandalone = (prev eq null) && (next eq null)
  def isMutable = (prev ne null)

  def prev: MutableStartOrNode[G, A]
  protected[bsgs] def prev_= (value: MutableStartOrNode[G, A]): Unit

  /** Makes the current node immutable. */
  protected[bsgs] def makeImmutable(): Unit = {
    next.mapOrElse(node => assert(node.isImmutable), ())
    prev = null
  }

  protected[bsgs] def removeRedundantGenerators(): Unit

  /** Adds `newGenerators` (given with their inverses) to this node `ownGenerators`,
    * without changing other nodes or updating any transversals. */
  protected[bsgs] def addToOwnGenerators(newGens: Iterable[G], newGensInv: Iterable[G])
                                        (implicit group: Group[G], classTag: ClassTag[G]): Unit
  protected[bsgs] def addToOwnGenerators(newGen: G, newGenInv: G)
                                        (implicit group: Group[G], classTag: ClassTag[G]): Unit

  /** Updates this node transversal by the addition of `newGens`,
    * provided with their inverses.
    * 
    * @note `newGens` must be already part of the strong generating set, i.e.
    *       have been added to this node or a children node `ownGenerators`
    *       by using addToOwnGenerators.
    */
  protected[bsgs] def updateTransversal(newGens: Iterable[G], newGensInv: Iterable[G])
                                       (implicit group: Group[G], classTag: ClassTag[G]): Unit
  protected[bsgs] def updateTransversal(newGen: G, newGenInv: G)
                                       (implicit group: Group[G], classTag: ClassTag[G]): Unit

  /** Conjugates the current node by the group element `g`, provided with its inverse
    * `gInv` to avoid multiple inverse element computations. 
    */
  protected[bsgs] def conjugate(g: G, gInv: G)
                               (implicit group: Group[G], classTag: ClassTag[G]): Unit

}

object MutableNode {

  type Generic[G] = MutableNode[G, _ <: PermutationAction[G] with Singleton]

}
