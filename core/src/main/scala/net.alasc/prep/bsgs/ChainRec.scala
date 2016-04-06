package net.alasc.prep
package bsgs

import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.perms.Perm

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
  @tailrec def isFixed[P](chain: Chain[P], k: Int): Boolean = chain match {
    case node: Node[P] =>
      cforRange(0 until node.nOwnGenerators) { i =>
        if (node.action.actr(k, node.ownGenerator(i)) != k) return false
      }
      isFixed(node.next, k)
    case _: Term[P] => true
  }

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

  @tailrec def isTrivial[P](chain: Chain[P]): Boolean = chain match {
    case node: Node[P] if node.orbitSize != 1 => false
    case node: Node[P] => isTrivial(node.next)
    case _: Term[P] => true
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

  @tailrec def sifts[P:Eq:Group](chain: Chain[P], remaining: P): Boolean = chain match {
    case node: Node[P] =>
      implicit def action = node.action
      val b = node.beta <|+| remaining
      if (!node.inOrbit(b))
        false
      else
        sifts(node.next, remaining |+| node.uInv(b))
    case _: Term[P] => remaining.isId
  }

  @tailrec def siftOther[P:Eq:Group:FaithfulPermutationAction, Q:Eq:Permutation](chain: Chain[P], pInv: P, q: Q): Opt[P] =
    chain match {
      case node: Node[P] =>
        val b = (node.beta <|+| q) <|+| pInv
        if (!node.inOrbit(b))
          Opt.empty[P]
        else
          siftOther(node.next, pInv |+| node.uInv(b), q)
      case _: Term[P] =>
        val p = pInv.inverse
        val pPerm = FaithfulPermutationAction[P].to[Perm](p)
        val qPerm = FaithfulPermutationAction[Q].to[Perm](q)
        if (pPerm === qPerm) Opt(p) else Opt.empty[P]
    }

  @tailrec def basicSift[P:Group](chain: Chain[P], remaining: P,
    transversalIndices: metal.mutable.Buffer[Int] = metal.mutable.Buffer.empty[Int]): (Seq[Int], P) =
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
