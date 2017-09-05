package net.alasc.bsgs

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.perms.Perm
import net.alasc.syntax.permutationAction._

object MutableChainRec {

  @tailrec def foreach[G, A <: PermutationAction[G] with Singleton, U]
    (elem: MutableStartOrNode[G, A], f: MutableNode[G, A] => U): Unit = elem match {
    case IsMutableNode(mn) =>
      f(mn)
      foreach(mn.prev, f)
    case _: Node[G, A] => sys.error("An immutable node cannot precede a mutable node in the chain.")
    case _: Start[G, A] => // finished
  }

}

object ChainRec {

  /** Tests whether this chain has a lexicographic base, i.e. the base point for any stabilizer in the chain
    * is the smallest domain element it moves.
    */
  def hasLexicographicBase[G, A <: PermutationAction[G] with Singleton](chain: Chain[G, A]): Boolean = {
    def smallestPointMovedByOwnGenerators(node: Node[G, A]): Int =
      if (node.nOwnGenerators == 0) -1 else {
        import node.action
        var mn = node.ownGenerator(0).smallestMovedPoint.get // generator are never identity
        cforRange(1 until node.nOwnGenerators) { i =>
          mn = spire.math.min(mn, node.ownGenerator(i).smallestMovedPoint.get)
        }
        mn
      }
    /* Returns the smallest domain element moved by `chain`, or a special value below:
     * -1 if nothing is moved, -2 if the lexicographic condition failed.
     */
    def rec(current: Chain[G, A]): Int = current match {
      case node: Node[G, A] =>
        rec(node.next) match {
          case -2 => -2 // test failed for the next stabilizer
          case -1 =>
            val mn = smallestPointMovedByOwnGenerators(node)
            if (mn == -1) mn // all nodes so far are trivial
            else if (node.beta == mn) mn // this node is the first non-trivial node, and is correct
            else -2 // test failed
          case nextM =>
            smallestPointMovedByOwnGenerators(node) match {
              case -1 => // this node is trivial
                if (node.beta < nextM) nextM // current node respects the lexicographic condition
                else -2 // current node does not respect the lexicographic condition
              case thisM =>
                val mn = spire.math.min(thisM, nextM)
                if (node.beta == mn) mn // current node respects the condition
                else -2 // current fails
            }
        }
      case _: Term[G, A] => -1
    }
    rec(chain) != -2
  }

  @tailrec def isFixed[G, A <: PermutationAction[G] with Singleton](chain: Chain[G, A], k: Int): Boolean = chain match {
    case node: Node[G, A] =>
      cforRange(0 until node.nOwnGenerators) { i =>
        if (node.action.actr(k, node.ownGenerator(i)) != k) return false
      }
      isFixed(node.next, k)
    case _: Term[G, A] => true
  }

  @tailrec def foreach[G, A <: PermutationAction[G] with Singleton, U](chain: Chain[G, A], f: Node[G, A] => U): Unit = chain match {
    case node: Node[G, A] =>
      f(node)
      foreach(node.next, f)
    case _: Term[G, A] => // finished
  }

  @tailrec final def length(chain: Chain[_, _], acc: Int = 0): Int = chain match {
    case node: Node[_, _] => length(node.next, acc + 1)
    case _: Term[_, _] => acc
  }

  @tailrec def random[G:Group](chain: Chain[G, _], rand: Random, acc: G): G = chain match {
    case node: Node[G, _] => random(node.next, rand, node.randomU(rand) |+| acc)
    case _: Term[G, _] => acc
  }

  @tailrec def order(chain: Chain[_, _], acc: SafeLong): SafeLong = chain match {
    case node: Node[_, _] => order(node.next, acc * node.orbitSize)
    case _: Term[_, _] => acc
  }

  @tailrec def isTrivial[G, A <: PermutationAction[G] with Singleton](chain: Chain[G, A]): Boolean = chain match {
    case node: Node[G, A] if node.orbitSize != 1 => false
    case node: Node[G, A] => isTrivial(node.next)
    case _: Term[G, A] => true
  }

  @tailrec final def base(chain: Chain[_, _], buffer: ArrayBuffer[Int] = ArrayBuffer.empty[Int]): Seq[Int] = chain match {
    case node: Node[_, _] => base(node.next, buffer += node.beta)
    case _: Term[_, _] => buffer.result()
  }

  @tailrec final def baseEquals(chain: Chain[_, _], baseToCheck: Iterator[Int]): Boolean = chain match {
    case node: Node[_, _] =>
      if (baseToCheck.hasNext) {
        val checkBeta = baseToCheck.next
        if (node.beta != checkBeta)
          false
        else
          baseEquals(node, baseToCheck)
      }
      else
        false
    case _: Term[_, _] => baseToCheck.isEmpty
  }

  @tailrec def sift[G:Group, A <: PermutationAction[G] with Singleton]
  (chain: Chain[G, A], remaining: G): Opt[G] = chain match {
    case node: Node[G, A] =>
      implicit def action: A = node.action
      val b = node.beta <|+| remaining
      if (!node.inOrbit(b))
        Opt.empty[G]
      else
        sift(node.next, remaining |+| node.uInv(b))
    case _: Term[G, A] => Opt(remaining)
  }

  @tailrec def findSameAction[G:Eq:Group, A <: PermutationAction[G] with Singleton, Q:PermutationAction]
    (chain: Chain[G, A], q: Q, gInv: G)(implicit action: A): Opt[G] =
    chain match {
      case node: Node[G, A] =>
        val b = (node.beta <|+| q) <|+| gInv
        if (!node.inOrbit(b))
          Opt.empty[G]
        else
          findSameAction(node.next, q, gInv |+| node.uInv(b))
      case _: Term[G, A] =>
        val g = gInv.inverse
        if (action.hasSameAction(g, q)) Opt(g) else Opt.empty[G]
    }

  @tailrec def basicSift[G:Group, A <: PermutationAction[G] with Singleton]
    (chain: Chain[G, A], remaining: G,
     transversalIndices: metal.mutable.Buffer[Int] = metal.mutable.Buffer.empty[Int]): (Seq[Int], G) =
    chain match {
      case node: Node[G, A] =>
        implicit def action: A = node.action
        val b = node.beta <|+| remaining
        if (!node.inOrbit(b))
          (transversalIndices.toScala, remaining)
        else {
          val nextRemaining = remaining |+| node.uInv(b)
          transversalIndices += b
          basicSift(node.next, nextRemaining, transversalIndices)
        }
      case _: Term[G, A] => (transversalIndices.toScala, remaining)
    }

  @tailrec def nStrongGenerators(chain: Chain[_, _], acc: Int = 0): Int = chain match {
    case node: Node[_, _] => nStrongGenerators(node.next, acc + node.nOwnGenerators)
    case _: Term[_, _] => acc
  }

  @tailrec def kthStrongGenerator[G](chain: Chain[G, _ <: PermutationAction[G] with Singleton], k: Int): G = chain match {
    case node: Node[G, _] if k < node.nOwnGenerators => node.ownGenerator(k)
    case node: Node[G, _] => kthStrongGenerator(node.next, k - node.nOwnGenerators)
    case _: Term[G, _] => throw new IndexOutOfBoundsException(s"Generator index out of bounds")
  }

}
