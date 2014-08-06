package net.alasc.math

import scala.annotation.tailrec
import scala.util.Random
import spire.syntax.group._
import spire.syntax.groupAction._
import net.alasc.algebra._
import bsgs._
import scala.collection.mutable.ListBuffer
import net.alasc.syntax.subgroup._

object BSGSRec {
  @tailrec final def base[P](bsgs: BSGS[P], buffer: ListBuffer[Int]): List[Int] = bsgs match {
    case _: BSGSTerm[P] => buffer.toList
    case node: BSGSNode[P] => base(node.tail, buffer += node.beta)
  }

  @tailrec final def length[P](bsgs: BSGS[P], acc: Int): Int = bsgs match {
    case _: BSGSTerm[P] => acc
    case node: BSGSNode[P] => length(node.tail, acc + 1)
  }

  @tailrec def basicSift[P: Permutation](bsgs: BSGS[P], remaining: P, transversalIndices: ListBuffer[Int]): (List[Int], P) = bsgs match {
    case _: BSGSTerm[P] => (transversalIndices.toList, remaining)
    case node: BSGSNode[P] =>
      val b = node.beta <|+| remaining
      if (!node.transversal.isDefinedAt(b))
        (transversalIndices.toList, remaining)
      else {
        val nextRemaining = remaining |+| node.transversal(b).gInv
        basicSift(node.tail, nextRemaining, transversalIndices += b)
      }
    }
}

/** Implementation of a BSGS chain as a single linked list.
  * 
  * Inspired by scala.collection.immutable.List.
  */
sealed abstract class BSGS[P] {
  implicit def algebra: Permutation[P]
  def isTerminal: Boolean

  def ownGenerators: List[P]

  def strongGeneratingSet: Iterable[P] = new Iterable[P] {
    override def stringPrefix = "Iterable"
    def iterator: Iterator[P] = for {
      chain <- BSGS.this.iterator
      p <- chain.ownGenerators
    } yield p
  }

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

  def length: Int

  def base: List[Int]

  def basicSift(p: P): (List[Int], P) = BSGSRec.basicSift(this, p, ListBuffer.empty[Int])

  def conjugatedBy(ip: InversePair[P]): BSGS[P] = ???

  def conjugatedBy(p: P): BSGS[P] = conjugatedBy(InversePair(p, p.inverse))

  def check: Unit = {
    assert(base.toSet.size == base.size) // base elements are unique
    checkFixBase(Nil)
    checkNode
  }

  def checkNode: Unit = ???
  def checkFixBase(partialBase: List[Int]): Unit = ???
}

final class BSGSNode[P](private[alasc] var tv: Transversal[P], private[alasc] var og: List[P], private[alasc] var tl: BSGS[P])(implicit val algebra: Permutation[P]) extends BSGS[P] {
  def isTerminal = false
  def tail = tl
  def transversal = tv
  def beta = transversal.beta
  def ownGenerators = og
  def length: Int = BSGSRec.length(tail, 1)
  def base = BSGSRec.base(tail, ListBuffer(beta))
}

final class BSGSTerm[P](implicit val algebra: Permutation[P]) extends BSGS[P] {
  def isTerminal = true
  def ownGenerators = Nil
  def length = 0
  def base = Nil
}

object BSGS {
  implicit def BSGSSubgroup[P](implicit algebra: Permutation[P]): Subgroup[BSGS[P], P] =
    new BSGSSubgroup[P]

  def fromSubgroup[S, P](subgroup: S, gen: Random)(implicit algebra: Permutation[P], sg: Subgroup[S, P], tb: TransversalBuilder): BSGS[P] = {
    val buffer = new BSGSBuffer[P]
    while (buffer.chain.order < subgroup.order)
      buffer.addElement(subgroup.random(gen))
    buffer.toBSGS
  }
}

final class BSGSSubgroup[P](implicit val algebra: Permutation[P]) extends Subgroup[BSGS[P], P] {
  def elementsIterator(bsgs: BSGS[P]): Iterator[P] = bsgs match {
    case _: BSGSTerm[P] => Iterator(algebra.id)
    case node: BSGSNode[P] => for {
      rest <- elementsIterator(node.tail)
      b <- node.transversal.keysIterator
    } yield rest |+| node.transversal(b).g
  }

  def elements(bsgs: BSGS[P]): Iterable[P] = new Iterable[P] {
    override def stringPrefix = "Elements"
    def iterator = elementsIterator(bsgs)
  }

  @tailrec def orderRec(bsgs: BSGS[P], acc: BigInt): BigInt = bsgs match {
    case _: BSGSTerm[P] => acc
    case node: BSGSNode[P] => orderRec(node.tail, acc * node.transversal.size)
  }

  def order(bsgs: BSGS[P]): BigInt = orderRec(bsgs, BigInt(1))

  def generators(bsgs: BSGS[P]): Seq[P] = bsgs.strongGeneratingSet.toSeq

  @tailrec def randomRec(bsgs: BSGS[P], rand: Random, acc: P): P = bsgs match {
    case _: BSGSTerm[P] => acc
    case node: BSGSNode[P] => randomRec(node.tail, rand, node.transversal.random(rand).g |+| acc)
  }

  def random(bsgs: BSGS[P], rand: Random) = bsgs match {
    case _: BSGSTerm[P] => algebra.id
    case node: BSGSNode[P] => randomRec(node.tail, rand, node.transversal.random(rand).g)
  }
}
