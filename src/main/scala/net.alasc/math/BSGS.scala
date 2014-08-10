package net.alasc.math

import scala.collection.immutable.{ BitSet => ImmutableBitSet }
import scala.collection.mutable.{ BitSet => MutableBitSet }
import scala.util.Random
import scala.annotation.tailrec

import spire.syntax.group._

import bsgs._
import net.alasc.algebra._
import net.alasc.syntax.subgroup._

/** Implementation of a BSGS chain as a single linked list. */
sealed trait BSGS[P] {
  implicit def algebra: Permutation[P]

  /** Tests whether this element terminates the BSGS chain. */
  def isTerminal: Boolean

  def mapOrElse[A](f: BSGSNode[P] => A, default: => A): A = this match {
    case node: BSGSNode[P] => f(node)
    case _: BSGSTerm[P] => default
  }

  /** Tests whether this node is mutable. */
  def isImmutable: Boolean

  /** If the base is beta(1) -> ... -> beta(m-1) -> beta(m) current base -> tail.beta,
    * ownGenerators contains all the strong generators g that have beta(i) <|+| g = beta(i) for i < m,
    * and beta(m) <|+| g =!= beta(m).
    */
  def ownGenerators: Iterable[P]
  def ownGeneratorsPairs: Iterable[InversePair[P]]

  /** Returns the strong generating set for the BSGS chain starting from this node.
    * 
    * @note The strong generating set is stored piece by piece by having each
    *       node storing explicitly only the generators appearing at its level.
    */
  def strongGeneratingSet: Iterable[P] = chain.flatMap(_.ownGenerators)

  def strongGeneratingSetPairs: Iterable[InversePair[P]] = chain.flatMap(_.ownGeneratorsPairs)

  /** Iterable for the nodes of the chain. */
  def chain: Iterable[BSGSNode[P]] = new Iterable[BSGSNode[P]] {
    override def stringPrefix = "Iterable"
    override def foreach[U](f: BSGSNode[P] ⇒ U): Unit = BSGSRec.foreach(BSGS.this, f)
    def iterator: Iterator[BSGSNode[P]] = new Iterator[BSGSNode[P]] {
      private var cursor: BSGS[P] = BSGS.this
      def hasNext = !cursor.isTerminal
      def next = cursor match {
        case _: BSGSTerm[P] => Iterator.empty.next
        case node: BSGSNode[P] =>
          val result = node
          cursor = node.tail
          result
      }
    }
  }

  def length: Int = BSGSRec.length(this)

  def base: List[Int] = BSGSRec.base(this)

  def baseEquals(baseToCheck: List[Int]) = BSGSRec.baseEquals(this, baseToCheck)

  def basicSift(p: P): (List[Int], P) = BSGSRec.basicSift(this, p)

  def conjugatedBy(ip: InversePair[P]): BSGS[P]

  def conjugatedBy(p: P): BSGS[P] = conjugatedBy(InversePair(p, p.inverse))

  def check: Unit = {
    assert(base.toSet.size == base.size) // base elements are unique
    checkFixBase(Nil)
    checkNode
  }

  def checkNode: Unit = ???
  def checkFixBase(partialBase: List[Int]): Unit = ???
}

final class BSGSTerm[P](implicit val algebra: Permutation[P]) extends BSGS[P] {
  def isTerminal = true
  def isImmutable = true
  def ownGenerators = Seq.empty
  def ownGeneratorsPairs = Seq.empty

  def conjugatedBy(ip: InversePair[P]) = this
}

/** Node in a BSGS chain.
  * 
  * See `BSGSMutableNode` and `BSGSBuilder` for a discussion of mutable nodes.
  * 
  * The set of strong generators is represented by storing with each node only the strong generators that stabilize
  * the previous base points, but not the node corresponding base point.
  */
trait BSGSNode[P] extends BSGS[P] {
  def isTerminal = false
  def tail: BSGS[P]
  def beta: Int

  def orbitSize: Int

  def inOrbit(b: Int): Boolean
  def orbit: Iterable[Int]
  def foreachOrbit[U](f: Int => U): Unit
  def orbitSet: ImmutableBitSet = {
    val bitset = MutableBitSet.empty
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

object BSGS {

  implicit def BSGSSubgroup[P](implicit algebra: Permutation[P]): Subgroup[BSGS[P], P] = new BSGSSubgroup[P]

  /** Deterministic Schreier-Sims algorithm. */
  def deterministicSchreierSims[P: Permutation](generators: Iterable[P], givenBase: Iterable[Int] = Iterable.empty)(implicit options: BSGSOptions): BSGS[P] = BSGSBuilder.deterministicSchreierSims(generators, givenBase).toBSGS

  /* Randomized BSGS Schreier-Sims algorithm. */ 
  def randomizedSchreierSims[P: Permutation](randomElement: Random => P, order: BigInt, givenBase: Iterable[Int] = Iterable.empty)(implicit options: BSGSOptions): BSGS[P] =
    BSGSBuilder.randomizedSchreierSims(randomElement, order, givenBase).toBSGS

  def fromGenerators[P: Permutation](generators: Iterable[P], givenBase: Iterable[Int] = Iterable.empty)(implicit options: BSGSOptions): BSGS[P] = deterministicSchreierSims(generators, givenBase)

  def fromGeneratorsAndOrder[P: Permutation](generators: Iterable[P], order: BigInt, givenBase: Iterable[Int] = Iterable.empty)(implicit options: BSGSOptions): BSGS[P] = options.algorithmType match {
    case Deterministic =>
      val chain = deterministicSchreierSims(generators, givenBase)
      assert(chain.order == order)
      chain
    case Randomized =>
      val bag = RandomBag(generators)
      randomizedSchreierSims(bag.random(_), order, givenBase)
  }

  def fromSubgroup[S, P](subgroup: S, givenBase: Iterable[Int] = Iterable.empty)(implicit algebra: Permutation[P], sg: Subgroup[S, P], options: BSGSOptions): BSGS[P] =
    options.algorithmType match {
      case Randomized => randomizedSchreierSims(rand => subgroup.random(rand), subgroup.order, givenBase)
      case Deterministic =>
        val chain = deterministicSchreierSims(subgroup.generators, givenBase)
        assert(chain.order == subgroup.order)
        chain
    }
}

final class BSGSSubgroup[P](implicit val algebra: Permutation[P]) extends Subgroup[BSGS[P], P] {
  def elements(bsgs: BSGS[P]): Iterable[P] = new Iterable[P] {
    override def stringPrefix = "Elements"
    def iterator = BSGSRec.elementsIterator(bsgs)
  }
  def order(bsgs: BSGS[P]): BigInt = BSGSRec.order(bsgs, BigInt(1))
  def generators(bsgs: BSGS[P]): Iterable[P] = bsgs.strongGeneratingSet
  def random(bsgs: BSGS[P], rand: Random) = bsgs.mapOrElse(node => BSGSRec.random(node.tail, rand, node.randomU(rand)), algebra.id)
}
