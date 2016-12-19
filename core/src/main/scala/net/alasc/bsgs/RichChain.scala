package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.PermutationAction

final class RichChain[G, A <: PermutationAction[G] with Singleton](val chain: Chain[G, A]) extends AnyVal {

  /** Returns a newly created mutable copy of `chain`, cloning all mutable nodes 
    * in the given chain.
    * 
    * The provided `chain` must have action compatible with the provided `action`.
    * 
    * The provided `action` is used only if the given `chain` is a terminal.
    */
  def mutableChain(implicit action: A, classTag: ClassTag[G], equ: Eq[G], group: Group[G]): MutableChain[G, A] = {
    chain.mapOrElse(node => require(node.action == PermutationAction[G]), ())
    val mutableChain = MutableChain.empty[G, A]
    @tailrec def rec(after: MutableStartOrNode[G, A], toInsert: Chain[G, A]): Unit = toInsert match {
      case IsMutableNode(mutableNode) =>
        val nodeCopy = NodeBuilder[G].standaloneClone[A](mutableNode)
        mutableChain.insertInChain(after, after.next, nodeCopy)
        rec(nodeCopy, mutableNode.next)
      case node: Node[G, A] => // immutable
        after.next = node
      case _: Term[G, A] => // at the end
    }
    rec(mutableChain.start, chain)
    mutableChain
  }

  /** Finds the conjugate element `g` for the domain element `b` such that `b <|+| g.inverse` is a base point of the current chain,
    * meaning that the current chain, conjugated by `g` will have `b` as a base point. Returns nothing if no such `g` exists.
    */
  def findConjugateElement(b: Int): Opt[G] = {
    @tailrec def rec(current: Chain[G, A]): Opt[G] = current match {
      case node: Node[G, A] if node.inOrbit(b) => Opt(node.u(b))
      case node: Node[G, A] => rec(node.next)
      case _: Term[G, A] => Opt.empty[G]
    }
    rec(chain)
  }

  /** Returns a chain with the (possibly new) base point `b` in front. */
  def withFirstBasePoint(b: Int)
                        (implicit action: A, baseSwap: BaseSwap, classTag: ClassTag[G],
                         equ: Eq[G], group: Group[G]): Chain[G, A] = {
    val mutableChain = chain.mutableChain
    mutableChain.changeBasePointAfter(mutableChain.start, b)
    mutableChain.toChain()
  }

}
