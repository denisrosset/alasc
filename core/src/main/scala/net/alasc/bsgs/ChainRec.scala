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

  @tailrec def foreach[G, F <: FaithfulPermutationAction[G] with Singleton, U]
    (elem: MutableStartOrNode[G, F], f: MutableNode[G, F] => U): Unit = elem match {
    case IsMutableNode(mn) =>
      f(mn)
      foreach(mn.prev, f)
    case _: Node[G, F] => sys.error("An immutable node cannot precede a mutable node in the chain.")
    case _: Start[G, F] => // finished
  }

}

object ChainRec {

  /** Tests whether this chain has a lexicographic base, i.e. the base point for any stabilizer in the chain
    * is the smallest domain element it moves.
    */
  def hasLexicographicBase[G, F <: FaithfulPermutationAction[G] with Singleton](chain: Chain[G, F]): Boolean = {
    def smallestPointMovedByOwnGenerators(node: Node[G, F]): Int =
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
    def rec(current: Chain[G, F]): Int = current match {
      case node: Node[G, F] =>
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
      case _: Term[G, F] => -1
    }
    rec(chain) != -2
  }

  @tailrec def isFixed[G, F <: FaithfulPermutationAction[G] with Singleton](chain: Chain[G, F], k: Int): Boolean = chain match {
    case node: Node[G, F] =>
      cforRange(0 until node.nOwnGenerators) { i =>
        if (node.action.actr(k, node.ownGenerator(i)) != k) return false
      }
      isFixed(node.next, k)
    case _: Term[G, F] => true
  }

  @tailrec def foreach[G, F <: FaithfulPermutationAction[G] with Singleton, U](chain: Chain[G, F], f: Node[G, F] => U): Unit = chain match {
    case node: Node[G, F] =>
      f(node)
      foreach(node.next, f)
    case _: Term[G, F] => // finished
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

  @tailrec def isTrivial[G, F <: FaithfulPermutationAction[G] with Singleton](chain: Chain[G, F]): Boolean = chain match {
    case node: Node[G, F] if node.orbitSize != 1 => false
    case node: Node[G, F] => isTrivial(node.next)
    case _: Term[G, F] => true
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

  @tailrec def sifts[G:Eq:Group, F <: FaithfulPermutationAction[G] with Singleton]
    (chain: Chain[G, F], remaining: G): Boolean = chain match {
    case node: Node[G, F] =>
      implicit def action: F = node.action
      val b = node.beta <|+| remaining
      if (!node.inOrbit(b))
        false
      else
        sifts(node.next, remaining |+| node.uInv(b))
    case _: Term[G, F] => remaining.isId
  }

  @tailrec def siftOther[G:Eq:Group, F <: FaithfulPermutationAction[G] with Singleton, Q:Permutation]
    (chain: Chain[G, F], gInv: G, q: Q)(implicit action: F): Opt[G] =
    chain match {
      case node: Node[G, F] =>
        val b = (node.beta <|+| q) <|+| gInv
        if (!node.inOrbit(b))
          Opt.empty[G]
        else
          siftOther(node.next, gInv |+| node.uInv(b), q)
      case _: Term[G, F] =>
        val g = gInv.inverse
        val gPerm = FaithfulPermutationAction[G].toPermutation[Perm](g)
        val qPerm = FaithfulPermutationAction[Q].toPermutation[Perm](q)
        if (gPerm === qPerm) Opt(g) else Opt.empty[G]
    }

  @tailrec def basicSift[G:Group, F <: FaithfulPermutationAction[G] with Singleton]
    (chain: Chain[G, F], remaining: G,
     transversalIndices: metal.mutable.Buffer[Int] = metal.mutable.Buffer.empty[Int]): (Seq[Int], G) =
    chain match {
      case node: Node[G, F] =>
        implicit def action: F = node.action
        val b = node.beta <|+| remaining
        if (!node.inOrbit(b))
          (transversalIndices.toScala, remaining)
        else {
          val nextRemaining = remaining |+| node.uInv(b)
          transversalIndices += b
          basicSift(node.next, nextRemaining, transversalIndices)
        }
      case _: Term[G, F] => (transversalIndices.toScala, remaining)
    }

  @tailrec def nStrongGenerators(chain: Chain[_, _], acc: Int = 0): Int = chain match {
    case node: Node[_, _] => nStrongGenerators(node.next, acc + node.nOwnGenerators)
    case _: Term[_, _] => acc
  }

}
