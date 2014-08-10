package net.alasc.math
package bsgs

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.util.Random

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._

object BSGSRec {
  @tailrec def random[P: Permutation](bsgs: BSGS[P], rand: Random, acc: P): P = bsgs match {
    case _: BSGSTerm[P] => acc
    case node: BSGSNode[P] => random(node.tail, rand, node.randomU(rand) |+| acc)
  }

  @tailrec def order[P](bsgs: BSGS[P], acc: BigInt): BigInt = bsgs match {
    case _: BSGSTerm[P] => acc
    case node: BSGSNode[P] => order(node.tail, acc * node.orbitSize)
  }

  def elementsIterator[P](bsgs: BSGS[P])(implicit algebra: Permutation[P]): Iterator[P] =
    bsgs.mapOrElse(
      node => for (rest <- elementsIterator(node.tail); b <- node.orbit.iterator) yield rest |+| node.u(b),
      Iterator(algebra.id)
    )

  @tailrec final def base[P](bsgs: BSGS[P], buffer: ListBuffer[Int] = ListBuffer.empty[Int]): List[Int] = bsgs match {
    case _: BSGSTerm[P] => buffer.toList
    case node: BSGSNode[P] => base(node.tail, buffer += node.beta)
  }

  @tailrec final def length[P](bsgs: BSGS[P], acc: Int = 0): Int = bsgs match {
    case _: BSGSTerm[P] => acc
    case node: BSGSNode[P] => length(node.tail, acc + 1)
  }

  @tailrec def basicSift[P: Permutation](bsgs: BSGS[P], remaining: P, transversalIndices: ListBuffer[Int] = ListBuffer.empty[Int]): (List[Int], P) = bsgs match {
    case _: BSGSTerm[P] => (transversalIndices.toList, remaining)
    case node: BSGSNode[P] =>
      val b = node.beta <|+| remaining
      if (!node.inOrbit(b))
        (transversalIndices.toList, remaining)
      else {
        val nextRemaining = remaining |+| node.uInv(b)
        basicSift(node.tail, nextRemaining, transversalIndices += b)
      }
  }

  @tailrec def foreach[P: Permutation, U](bsgs: BSGS[P], f: BSGSNode[P] ⇒ U): Unit = bsgs match {
    case _: BSGSTerm[P] =>
    case node: BSGSNode[P] =>
      f(node)
      foreach(node.tail, f)
  }
}
