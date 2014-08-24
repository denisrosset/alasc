package net.alasc.math
package bsgs

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.util.Random

import spire.algebra.Group
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._

object MutableChainRec {
  @tailrec def foreach[P, U](elem: MutableStartOrNode[P], f: MutableNode[P] => U): Unit = elem match {
    case IsMutableNode(mn) =>
      f(mn)
      foreach(mn.prev, f)
    case _: Node[P] => sys.error("An immutable node cannot precede a mutable node in the chain.")
    case _: Start[P] => // finished
  }
}

object ChainRec {
  @tailrec def foreach[P, U](chain: Chain[P], f: Node[P] => U): Unit = chain match {
    case node: Node[P] =>
      f(node)
      foreach(node.next, f)
    case _: Term[P] => // finished
  }

  @tailrec final def length[P](chain: Chain[P], acc: Int = 0): Int = chain match {
    case node: Node[P] => length(node.next, acc + 1)
    case _: Term[P] => acc
  }

  @tailrec def random[P: Group](chain: Chain[P], rand: Random, acc: P): P = chain match {
    case node: Node[P] => random(node.next, rand, node.randomU(rand) |+| acc)
    case _: Term[P] => acc
  }

  @tailrec def order[P](chain: Chain[P], acc: BigInt): BigInt = chain match {
    case node: Node[P] => order(node.next, acc * node.orbitSize)
    case _: Term[P] => acc
  }

  @tailrec final def base[P](chain: Chain[P], buffer: ArrayBuffer[Int] = ArrayBuffer.empty[Int]): Seq[Int] = chain match {
    case node: Node[P] => base(node.next, buffer += node.beta)
    case _: Term[P] => buffer.result
  }

  @tailrec final def baseEquals[P](chain: Chain[P], baseToCheck: Iterator[Int]): Boolean = chain match {
    case node: Node[P] =>
      if (baseToCheck.hasNext) {
        val checkBeta = baseToCheck.next
        if (node.beta != checkBeta)
          false
        else
          baseEquals(node, baseToCheck)
      }
      else
        false
    case _: Term[P] => baseToCheck.isEmpty
  }

  def elementsIterator[P: FiniteGroup](chain: Chain[P])(implicit algebra: Group[P]): Iterator[P] =
    chain.mapOrElse(
      node => for (rest <- elementsIterator(node.next); b <- node.orbit.iterator) yield rest |+| node.u(b),
      Iterator(algebra.id)
    )

  @tailrec def sifts[P: FiniteGroup](chain: Chain[P], remaining: P): Boolean = chain match {
    case node: Node[P] =>
      implicit def action = node.action
      val b = node.beta <|+| remaining
      if (!node.inOrbit(b))
        false
      else
        sifts(node.next, remaining |+| node.uInv(b))
    case _: Term[P] => remaining.isId
  }

  @tailrec def basicSift[P: FiniteGroup](chain: Chain[P], remaining: P,
    transversalIndices: debox.Buffer[Int] = debox.Buffer.empty[Int]): (Seq[Int], P) =
    chain match {
      case node: Node[P] =>
        implicit def action = node.action
        val b = node.beta <|+| remaining
        if (!node.inOrbit(b))
          (transversalIndices.toArray, remaining) // TODO replace by toSeq if debox implements it
        else {
          val nextRemaining = remaining |+| node.uInv(b)
          transversalIndices += b
          basicSift(node.next, nextRemaining, transversalIndices)
        }
      case _: Term[P] => (transversalIndices.toArray, remaining) // TODO replace by toSeq if debox implements it
    }
}
