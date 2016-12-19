package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.perms.Perm
import net.alasc.syntax.permutationAction._
import net.alasc.util._

// note: some tail recursive methods were moved to the RichMutableChain companion
// object, due to an elusive bug in the Scala compiler
final class RichMutableChain[G, A <: PermutationAction[G] with Singleton](val start: Start[G, A]) extends AnyVal {

  def mutableChain: MutableChain[G, A] = new MutableChain[G, A](start)

  /** Checks if there exist a base point with orbit size 1 in `mutableChain`, 
    * starting from `chain`. 
    */
  @tailrec final def existsRedundantBasePoint(chain: Chain[G, A]): Boolean = chain match {
    case node: Node[G, A] =>
      if (node.orbitSize == 1)
        true
      else
        existsRedundantBasePoint(node.next)
    case _: Term[G, A] => false
  }

  /** Returns the node with base `basePoint` in `mutableChain` starting from `chain`,
    * or `Opt.empty` if it cannot be found.  
    */
  @tailrec final def findBasePoint(chain: Chain[G, A], basePoint: Int): Opt[Node[G, A]] = chain match {
    case node: Node[G, A] =>
      if (node.beta == basePoint)
        Opt(node)
      else
        findBasePoint(node.next, basePoint)
    case _: Term[G, A] => Opt.empty[Node[G, A]]
  }

  /** Finds the last redundant node in `mutableChain`, starting from `chain`. */
  def findLastRedundant(from: Chain[G, A]): Opt[Node[G, A]] = {
    @tailrec def rec(chain: Chain[G, A], lastRedundantOption: Opt[Node[G, A]]): Opt[Node[G, A]] = chain match {
      case _: Term[G, A] => lastRedundantOption
      case node: Node[G, A] =>
        if (node.orbitSize == 1)
          rec(node.next, Opt(node))
        else
          rec(node.next, lastRedundantOption)
    }
    rec(from, Opt.empty[Node[G, A]])
  }

  /** Removes redundant nodes in `mutableChain` after `afterThis`.
    * 
    * @param afterThis    Element whose next elements are cleaned up
    * @return the number of removed elements
    */
  def cutRedundantAfter(afterThis: StartOrNode[G, A])
      (implicit classTag: ClassTag[G], group: Group[G]): Int =
    mutableChain.findLastRedundant(afterThis.next) match {
      case Opt(lastR) =>
        @tailrec def eliminateRedundantTail(prev: StartOrNode[G, A], removed: Int): Int =
          prev.next match {
            case _: Term[G, A] => sys.error("lastR should have been encountered before")
            case node: Node[G, A] =>
              if (node.orbitSize == 1) {
                val isLastR = node eq lastR
                val mutablePrev = mutableChain.mutableStartOrNode(prev)
                mutableChain.remove(mutablePrev, node, node.next)
                if (!isLastR)
                  eliminateRedundantTail(mutablePrev, removed + 1)
                else
                  removed + 1
              } else
                eliminateRedundantTail(node, removed)
          }
        eliminateRedundantTail(afterThis, 0)
      case _ => 0
    }

  /** Finds an element such that `beta` is stabilized by the subgroup after the element. */
  def findElemBeforeStabilizer(from: StartOrNode[G, A], beta: Int): StartOrNode[G, A] = {
    implicit def action = start.action
    require(beta >= 0)
    @tailrec def rec(chain: Chain[G, A], lastCandidate: StartOrNode[G, A]): StartOrNode[G, A] =
      chain match {
        case _: Term[G, A] => lastCandidate
        case node: Node[G, A] =>
          if (node.ownGenerators.exists(g => (beta <|+| g) != beta))
            rec(node.next, node)
          else
            rec(node.next, lastCandidate)
      }
    rec(from.next, from)
  }


  /** Inserts a (non-existing) base point after the element `afterThis`. */
  def insertNewBasePointAfter(afterThis: MutableStartOrNode[G, A], beta: Int)
      (implicit classTag: ClassTag[G], group: Group[G]): Unit = {
    implicit def action = start.action
    require(beta >= 0)
    val insertAfter = mutableChain.mutableStartOrNode(mutableChain.findElemBeforeStabilizer(afterThis, beta))
    val newNode = NodeBuilder[G].standalone(beta)
    mutableChain.insertInChain(insertAfter, insertAfter.next, newNode)
  }

  /** Change the base point after the element `afterThis` to `beta` and returns
    * this node with base point `beta`. */
  def changeBasePointAfter(afterThis: MutableStartOrNode[G, A], beta: Int)
    (implicit baseSwap: BaseSwap, classTag: ClassTag[G], group: Group[G]): Node[G, A] = {
    implicit def action: A = start.action
    mutableChain.putExistingBasePointAfter(afterThis, beta).getOrElse {
      mutableChain.insertNewBasePointAfter(afterThis, beta)
      mutableChain.putExistingBasePointAfter(afterThis, beta).get
    }
  }

  /** Shifts the existing `beta` at the node after `after`, if the chain already 
    * contains `basePoint`.
    * 
    * @return The shifted node with base point `beta` if the chain contains `basePoint` 
    * and the shift was performed, `Opt.empty` otherwise.
    */
  def putExistingBasePointAfter(after: MutableStartOrNode[G, A], beta: Int)
    (implicit baseSwap: BaseSwap, classTag: ClassTag[G], group: Group[G]): Opt[Node[G, A]] = {
    mutableChain.findBasePoint(after.next, beta) match {
      case Opt(toShift) =>
        val mutableToShift = mutableChain.mutable(toShift, after)
        @tailrec def shift(pos: MutableNode[G, A]): Opt[Node[G, A]] =
          if (pos.prev ne after) {
            pos.prev match {
              case prevNode: MutableNode[G, A] =>
                val MutableNodeAndNext(node1, node2) =
                  baseSwap.baseSwap(mutableChain, prevNode, pos)
                shift(node1)
              case _: Start[G, A] => sys.error("mutableHere should be before mutableToShift")
            }
          } else
            Opt(pos)
        shift(mutableToShift)
      case _ => Opt.empty[Node[G, A]]
    }
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
  def insertGenerators(generators: Iterable[G])
    (implicit classTag: ClassTag[G], group: Group[G]): Unit = {

    implicit def action: PermutationAction[G] = mutableChain.start.action

    if (generators.isEmpty) return

    def badGen = sys.error("Generator must not be identity")

    def rec(mutableNode: MutableNode[G, A], remaining: Iterable[G]): Unit = {
      val (generatorsThere, forNext) = remaining.partition(g => (mutableNode.beta <|+| g) != mutableNode.beta)
      if (!forNext.isEmpty)
        rec(mutableChain.mutableNodeAfter(mutableNode, forNext.head.findMovedPoint.getOrElse(badGen)), forNext)
      generatorsThere.foreach( g => mutableChain.addStrongGeneratorHere(mutableNode, g, g.inverse) )
    }
    rec(mutableChain.mutableNodeAfter(mutableChain.start, generators.head.findMovedPoint.getOrElse(badGen)), generators)
  }

  /** Add a new strong generator `gen` at the node `mutableNode`, and updates the transversal
    * of `mutableNode` and any previous node in the chain.
    */
  def addStrongGeneratorHere(mutableNode: MutableNode[G, A], gen: G, genInv: G)
    (implicit classTag: ClassTag[G], group: Group[G]): Unit = {
    implicit def action = mutableChain.start.action
    mutableNode.addToOwnGenerators(gen, genInv)
    updateTransversalsFrom(mutableNode, gen, genInv)
  }

  /** Update the transversals of this node and the previous nodes with the new
    * strong generator `gen`, given with its inverse `genInv`.
    */
  @tailrec def updateTransversalsFrom(mutableNode: MutableNode[G, A], gen: G, genInv: G)
                            (implicit classTag: ClassTag[G], group: Group[G]): Unit = {
    mutableNode.updateTransversal(gen, genInv)
    mutableNode.prev match {
      case IsMutableNode(prev) => updateTransversalsFrom(prev, gen, genInv)
      case _ => // at the end
    }
  }

  /** Removes redundant strong generators in the given chain. */
  def removeRedundantGenerators()
      (implicit group: Group[G]): Unit = {
    @tailrec def rec(mutableNode: MutableNode[G, A]): Unit = {
      mutableNode.removeRedundantGenerators
      mutableNode.prev match {
        case IsMutableNode(g) => rec(g)
        case _ =>
      }
    }
    mutableChain.findLastMutable() match {
      case IsMutableNode(mn) => rec(mn)
      case _ =>
    }
  }

}
