package net.alasc.math
package bsgs
package algorithms

import scala.annotation.tailrec

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra.{FiniteGroup, PermutationAction}
import net.alasc.syntax.permutationAction._
import net.alasc.util._

trait MutableAlgorithms[P] extends Algorithms[P] {
  implicit def nodeBuilder: NodeBuilder[P]
}

trait AppendBaseAlgorithms[P] extends MutableAlgorithms[P] {
  /** Sifts the element `p` through the BSGS chain starting at `elem`, and returns either:
    * 
    * - `None` if `p` can be sifted completely,
    * - `Some(pair)` where `pair` describes the node and the
    *   (incompletely) sifted element that should be inserted up to it.
    * 
    * Will insert new base points if necessary.
    * 
    * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
    */
  @tailrec final def siftAndUpdateBaseFrom(mutableChain: MutableChain[P], elem: StartOrNode[P], p: P): RefOption[(MutableNode[P], P)] = {
    implicit def action = mutableChain.start.action
    elem.next match {
      case _: Term[P] =>
        p.supportAny.fold[RefOption[(MutableNode[P], P)]](RefNone) { k =>
          val newNode = nodeBuilder.standalone(k)
          mutableChain.insertInChain(mutableChain.mutableStartOrNode(elem), elem.next, newNode)
          RefSome(newNode -> p)
        }
      case node: Node[P] =>
        val b = node.beta <|+| p
        if (!node.inOrbit(b))
          RefSome(mutableChain.mutable(node) -> p)
        else {
          val h = p |+| node.uInv(b)
          siftAndUpdateBaseFrom(mutableChain, node, h)
        }
    }
  }

  def emptyChainWithBase(base: Seq[Int])(implicit action: PermutationAction[P]): MutableChain[P] = {
    val mutableChain = MutableChain.empty[P]
    @tailrec def rec(prev: MutableStartOrNode[P], iterator: Iterator[Int]): Unit =
      if (iterator.hasNext) {
        val beta = iterator.next
        val newNode = nodeBuilder.standalone(beta)
        mutableChain.insertInChain(prev, prev.next, newNode)
        rec(newNode, iterator)
      }
    rec(mutableChain.start, base.iterator)
    mutableChain
  }

  def mutableChainCopy(chain: Chain[P])(implicit action: PermutationAction[P]): MutableChain[P] = {
    chain.mapOrElse(node => require(node.action == action), ())
    val mutableChain = MutableChain.empty
    @tailrec def rec(after: MutableStartOrNode[P], toInsert: Chain[P]): Unit = toInsert match {
      case IsMutableNode(mutableNode) =>
        val nodeCopy = nodeBuilder.standaloneClone(mutableNode)
        mutableChain.insertInChain(after, after.next, nodeCopy)
        rec(nodeCopy, mutableNode.next)
      case node: Node[P] => // immutable
        after.next = node
      case _: Term[P] => // at the end
    }
    rec(mutableChain.start, chain)
    mutableChain
  }
}

trait AddGeneratorsAlgorithms[P] extends AppendBaseAlgorithms[P] {

  def incompleteChainWithGenerators(generators: Iterable[P], base: Seq[Int] = Seq.empty)(
    implicit action: PermutationAction[P]): MutableChain[P] = {
    val mutableChain = emptyChainWithBase(base)
    insertGenerators(mutableChain, generators)
    mutableChain
  }

  /** Adds the given generators to the BSGS chain, adding base elements
    * if necessary, and updates the transversals.
    * 
    * Not tail recursive.
    * 
    * The chain is not completed (either the randomized or the 
    * deterministic Schreier-Sims algorithm should be used for that purpose).
    * 
    * @param generators  Sequence of generators. Must not contain the identity.
    */
  def insertGenerators(mutableChain: MutableChain[P], generators: Iterable[P]): Unit = {
    implicit def action = mutableChain.start.action
    if (generators.isEmpty) return

    def badGen = sys.error("Generator must not be identity")
    def rec(mutableNode: MutableNode[P], remaining: Iterable[P]): Unit = {
      val (generatorsThere, forNext) = remaining.partition(g => (mutableNode.beta <|+| g) != mutableNode.beta)
      if (!forNext.isEmpty)
        rec(mutableChain.mutableNodeAfter(mutableNode, forNext.head.supportAny.getOrElse(badGen)), forNext)
      generatorsThere.foreach( addStrongGeneratorHere(mutableChain, mutableNode, _) )
    }
    rec(mutableChain.mutableNodeAfter(mutableChain.start, generators.head.supportAny.getOrElse(badGen)), generators)
  }

  /** Add a new strong generator `p` at the node `mutableNode`, and updates the transversal
    * of `mutableNode` and any previous node in the chain.
    */
  def addStrongGeneratorHere(mutableChain: MutableChain[P], mutableNode: MutableNode[P], generatorPair: InversePair[P]): Unit = {
    implicit def action = mutableChain.start.action
    mutableNode.addToOwnGenerators(generatorPair)
    mutableNode.nodesPrev.foreach( _.updateTransversal(generatorPair) )
  }

  /** Removes redundant strong generators in the given chain. */
  def removeRedundantGenerators(mutableChain: MutableChain[P]): Unit = {

  }
}
