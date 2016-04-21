package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction

final class RichChain[G, F <: FaithfulPermutationAction[G] with Singleton](val chain: Chain[G, F]) extends AnyVal {

  /** Returns a newly created mutable copy of `chain`, cloning all mutable nodes 
    * in the given chain.
    * 
    * The provided `chain` must have action compatible with the provided `action`.
    * 
    * The provided `action` is used only if the given `chain` is a terminal.
    */
  def mutableChain(implicit action: F, classTag: ClassTag[G], equ: Eq[G], group: Group[G]): MutableChain[G, F] = {
    chain.mapOrElse(node => require(node.action == FaithfulPermutationAction[G]), ())
    val mutableChain = MutableChain.empty[G, F]
    @tailrec def rec(after: MutableStartOrNode[G, F], toInsert: Chain[G, F]): Unit = toInsert match {
      case IsMutableNode(mutableNode) =>
        val nodeCopy = NodeBuilder[G].standaloneClone[F](mutableNode)
        mutableChain.insertInChain(after, after.next, nodeCopy)
        rec(nodeCopy, mutableNode.next)
      case node: Node[G, F] => // immutable
        after.next = node
      case _: Term[G, F] => // at the end
    }
    rec(mutableChain.start, chain)
    mutableChain
  }

  /** Finds the conjugate element `g` for the domain element `b` such that `b <|+| g.inverse` is a base point of the current chain,
    * meaning that the current chain, conjugated by `g` will have `b` as a base point. Returns nothing if no such `g` exists.
    */
  def findConjugateElement(b: Int): Opt[G] = {
    @tailrec def rec(current: Chain[G, F]): Opt[G] = current match {
      case node: Node[G, F] if node.inOrbit(b) => Opt(node.u(b))
      case node: Node[G, F] => rec(node.next)
      case _: Term[G, F] => Opt.empty[G]
    }
    rec(chain)
  }

  /** Returns a chain with the (possibly new) base point `b` in front. */
  def withFirstBasePoint(b: Int)
                        (implicit action: F, baseSwap: BaseSwap, classTag: ClassTag[G],
                         equ: Eq[G], group: Group[G]): Chain[G, F] = {
    val mutableChain = chain.mutableChain
    mutableChain.changeBasePointAfter(mutableChain.start, b)
    mutableChain.toChain()
  }

}
