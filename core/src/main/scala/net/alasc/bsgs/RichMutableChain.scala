package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.syntax.permutationAction._
import net.alasc.util._

// note: some tail recursive methods were moved to the RichMutableChain companion
// object, due to an elusive bug in the Scala compiler
final class RichMutableChain[G, F <: PermutationAction[G] with Singleton](val start: Start[G, F]) extends AnyVal {

  def mutableChain: MutableChain[G, F] = new MutableChain[G, F](start)

  /** Checks if there exist a base point with orbit size 1 in `mutableChain`, 
    * starting from `chain`. 
    */
  @tailrec final def existsRedundantBasePoint(chain: Chain[G, F]): Boolean = chain match {
    case node: Node[G, F] =>
      if (node.orbitSize == 1)
        true
      else
        existsRedundantBasePoint(node.next)
    case _: Term[G, F] => false
  }

  /** Returns the node with base `basePoint` in `mutableChain` starting from `chain`,
    * or `Opt.empty` if it cannot be found.  
    */
  @tailrec final def findBasePoint(chain: Chain[G, F], basePoint: Int): Opt[Node[G, F]] = chain match {
    case node: Node[G, F] =>
      if (node.beta == basePoint)
        Opt(node)
      else
        findBasePoint(node.next, basePoint)
    case _: Term[G, F] => Opt.empty[Node[G, F]]
  }

  /** Finds the last redundant node in `mutableChain`, starting from `chain`. */
  def findLastRedundant(from: Chain[G, F]): Opt[Node[G, F]] = {
    @tailrec def rec(chain: Chain[G, F], lastRedundantOption: Opt[Node[G, F]]): Opt[Node[G, F]] = chain match {
      case _: Term[G, F] => lastRedundantOption
      case node: Node[G, F] =>
        if (node.orbitSize == 1)
          rec(node.next, Opt(node))
        else
          rec(node.next, lastRedundantOption)
    }
    rec(from, Opt.empty[Node[G, F]])
  }

  /** Removes redundant nodes in `mutableChain` after `afterThis`.
    * 
    * @param afterThis    Element whose next elements are cleaned up
    * @return the number of removed elements
    */
  def cutRedundantAfter(afterThis: StartOrNode[G, F])
      (implicit classTag: ClassTag[G], eq: Eq[G], group: Group[G]): Int =
    mutableChain.findLastRedundant(afterThis.next) match {
      case Opt(lastR) =>
        @tailrec def eliminateRedundantTail(prev: StartOrNode[G, F], removed: Int): Int =
          prev.next match {
            case _: Term[G, F] => sys.error("lastR should have been encountered before")
            case node: Node[G, F] =>
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
  def findElemBeforeStabilizer(from: StartOrNode[G, F], beta: Int): StartOrNode[G, F] = {
    implicit def action = start.action
    require(beta >= 0)
    @tailrec def rec(chain: Chain[G, F], lastCandidate: StartOrNode[G, F]): StartOrNode[G, F] =
      chain match {
        case _: Term[G, F] => lastCandidate
        case node: Node[G, F] =>
          if (node.ownGenerators.exists(g => (beta <|+| g) != beta))
            rec(node.next, node)
          else
            rec(node.next, lastCandidate)
      }
    rec(from.next, from)
  }


  /** Inserts a (non-existing) base point after the element `afterThis`. */
  def insertNewBasePointAfter(afterThis: MutableStartOrNode[G, F], beta: Int)
      (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Unit = {
    implicit def action = start.action
    require(beta >= 0)
    val insertAfter = mutableChain.mutableStartOrNode(mutableChain.findElemBeforeStabilizer(afterThis, beta))
    val newNode = NodeBuilder[G].standalone(beta)
    mutableChain.insertInChain(insertAfter, insertAfter.next, newNode)
  }

  /** Change the base point after the element `afterThis` to `beta` and returns
    * this node with base point `beta`. */
  def changeBasePointAfter(afterThis: MutableStartOrNode[G, F], beta: Int)
    (implicit baseSwap: BaseSwap, classTag: ClassTag[G],
      equ: Eq[G], group: Group[G]): Node[G, F] = {
    implicit def action: F = start.action
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
  def putExistingBasePointAfter(after: MutableStartOrNode[G, F], beta: Int)
    (implicit baseSwap: BaseSwap, classTag: ClassTag[G],
      equ: Eq[G], group: Group[G]): Opt[Node[G, F]] = {
    mutableChain.findBasePoint(after.next, beta) match {
      case Opt(toShift) =>
        val mutableToShift = mutableChain.mutable(toShift, after)
        @tailrec def shift(pos: MutableNode[G, F]): Opt[Node[G, F]] =
          if (pos.prev ne after) {
            pos.prev match {
              case prevNode: MutableNode[G, F] =>
                val MutableNodeAndNext(node1, node2) =
                  baseSwap.baseSwap(mutableChain, prevNode, pos)
                shift(node1)
              case _: Start[G, F] => sys.error("mutableHere should be before mutableToShift")
            }
          } else
            Opt(pos)
        shift(mutableToShift)
      case _ => Opt.empty[Node[G, F]]
    }
  }

  /** Returns an element obtained by sifting `g` through the BSGS chain starting at `elem`,
    * inserting new base points as required, returns `Opt.empty` if `p` can be sifted completely.
    * 
    * @return the sifted element `Opt((node, p1))` where `node` is where the sifting stopped,
    *         and `p1` is the remaining element, or `RefNone` if `p` can be sifted completely.
    * 
    * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
    */
  def siftAndUpdateBaseFrom(elem: StartOrNode[G, F], g: G)
    (implicit classTag: ClassTag[G], equ: Eq[G],
      group: Group[G]): Opt[(MutableNode[G, F], G)] = RichMutableChain.siftAndUpdateBaseFrom(mutableChain, elem, g)

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
    (implicit classTag: ClassTag[G], equ: Eq[G],
      group: Group[G]): Unit = {

    implicit def action: PermutationAction[G] = mutableChain.start.action

    if (generators.isEmpty) return

    def badGen = sys.error("Generator must not be identity")

    def rec(mutableNode: MutableNode[G, F], remaining: Iterable[G]): Unit = {
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
  def addStrongGeneratorHere(mutableNode: MutableNode[G, F], gen: G, genInv: G)
    (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Unit = {
    implicit def action = mutableChain.start.action
    mutableNode.addToOwnGenerators(gen, genInv)
    updateTransversalsFrom(mutableNode, gen, genInv)
  }

  /** Update the transversals of this node and the previous nodes with the new
    * strong generator `gen`, given with its inverse `genInv`.
    */
  @tailrec def updateTransversalsFrom(mutableNode: MutableNode[G, F], gen: G, genInv: G)
                            (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Unit = {
    mutableNode.updateTransversal(gen, genInv)
    mutableNode.prev match {
      case IsMutableNode(prev) => updateTransversalsFrom(prev, gen, genInv)
      case _ => // at the end
    }
  }

  /** Removes redundant strong generators in the given chain. */
  def removeRedundantGenerators()
      (implicit equ: Eq[G], group: Group[G]): Unit = {
    @tailrec def rec(mutableNode: MutableNode[G, F]): Unit = {
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

  /** Finds if a new strong generator can be found at the given `node`, assuming that
    * the chain starting at `node.tail` is complete.
    * 
    * If a new strong generator is found, returns some pair containing
    * the node and the strong generator to insert there.
    */
  def findNewStrongGeneratorAt(node: Node[G, F])
    (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Opt[(MutableNode[G, F], G)] =
    RichMutableChain.findNewStrongGeneratorAt(mutableChain, node)

  /** Completes the set of strong generators starting at `node`, assuming 
    * that `node.tail` is already completed.
    * 
    * Inspired (but rewritten) from SCHREIERSIMS, page 91 of Holt (2005).
    */
  final def completeStrongGeneratorsAt(mutableNode: MutableNode[G, F])
    (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Unit =
    RichMutableChain.completeStrongGeneratorsAt(mutableChain, mutableNode)

  /** Deterministic Schreier-Sims algorithm. */
  def completeStrongGenerators()(implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Unit = mutableChain.findLast() match {
    case _: Start[G, F] => // chain is empty, no generators to find
    case node: Node[G, F] => mutableChain.completeStrongGeneratorsAt(mutableChain.mutable(node))
  }

}

object RichMutableChain {

  @tailrec def siftAndUpdateBaseFrom[G, F <: PermutationAction[G] with Singleton](mutableChain: MutableChain[G, F], elem: StartOrNode[G, F], g: G)
    (implicit classTag: ClassTag[G], equ: Eq[G],
      group: Group[G]): Opt[(MutableNode[G, F], G)] = {
    implicit def action = mutableChain.start.action
    elem.next match {
      case _: Term[G, F] =>
        g.findMovedPoint match {
          case NNOption(k) =>
            val newNode = NodeBuilder[G].standalone(k)
            mutableChain.insertInChain(mutableChain.mutableStartOrNode(elem), elem.next, newNode)
            Opt(newNode -> g)
          case _ => Opt.empty[(MutableNode[G, F], G)]
        }
      case node: Node[G, F] =>
        val b = node.beta <|+| g
        if (!node.inOrbit(b))
          Opt(mutableChain.mutable(node) -> g)
        else {
          val h = g |+| node.uInv(b)
          siftAndUpdateBaseFrom(mutableChain, node, h)
        }
    }
  }

  def findNewStrongGeneratorAt[G, F <: PermutationAction[G] with Singleton](mutableChain: MutableChain[G, F], node: Node[G, F])
    (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Opt[(MutableNode[G, F], G)] = {
    node.foreachOrbit { b =>
      implicit def action: PermutationAction[G] = node.action
      val ub = node.u(b)
      for (x <- node.strongGeneratingSet) {
        val i = b <|+| x
        if (!node.inOrbit(i))
          return Opt(mutableChain.mutable(node) -> x)
        val ubx = ub |+| x
        if (ubx =!= node.u(i)) {
          val schreierGen = ubx |+| node.uInv(i)
          val res: Opt[(MutableNode[G, F], G)] = mutableChain.siftAndUpdateBaseFrom(node, schreierGen)
          if (!res.isEmpty) return res
        }
      }
    }
    Opt.empty[(MutableNode[G, F], G)]
  }

    /** Completes the set of strong generators starting at `node`, assuming 
    * that `node.tail` is already completed.
    * 
    * Inspired (but rewritten) from SCHREIERSIMS, page 91 of Holt (2005).
    */
  @tailrec final def completeStrongGeneratorsAt[G, F <: PermutationAction[G] with Singleton](mutableChain: MutableChain[G, F],
    mutableNode: MutableNode[G, F])
    (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G]): Unit =
    mutableChain.findNewStrongGeneratorAt(mutableNode) match {
      case Opt((where, newGenerator)) =>
        // there is a new strong generator at the node `where`, add it there and restart the search there
        implicit def action = mutableChain.start.action
        mutableChain.addStrongGeneratorHere(where, newGenerator, newGenerator.inverse)
        completeStrongGeneratorsAt(mutableChain, where)
      case _ => mutableNode.prev match {
        case IsMutableNode(mutablePrev) =>
          // current node does not have new strong generators, but node has parent that has to be completed
          completeStrongGeneratorsAt(mutableChain, mutablePrev)
        case node: Node[G, F] => sys.error("mutableNode.prev cannot be immutable")
        case start: Start[G, F] =>
          // current node does not have new strong generators, and current node starts the chain, we are finished
      }
    }

}
