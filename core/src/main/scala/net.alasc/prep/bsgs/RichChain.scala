package net.alasc.prep.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.syntax.permutationAction._
import net.alasc.util._

final class RichChain[G](val chain: Chain[G]) extends AnyVal {

  /** Returns a newly created mutable copy of `chain`, cloning all mutable nodes 
    * in the given chain.
    * 
    * The provided `chain` must have action compatible with the provided `action`.
    * 
    * The provided `action` is used only if the given `chain` is a terminal.
    */
  def mutableChain(implicit
    classTag: ClassTag[G],
    equ: Eq[G],
    action: FaithfulPermutationAction[G],
    group: Group[G]
  ): MutableChain[G] = {
    chain.mapOrElse(node => require(node.action == FaithfulPermutationAction[G]), ())
    val mutableChain = MutableChain.empty
    @tailrec def rec(after: MutableStartOrNode[G], toInsert: Chain[G]): Unit = toInsert match {
      case IsMutableNode(mutableNode) =>
        val nodeCopy = NodeBuilder[G].standaloneClone(mutableNode)
        mutableChain.insertInChain(after, after.next, nodeCopy)
        rec(nodeCopy, mutableNode.next)
      case node: Node[G] => // immutable
        after.next = node
      case _: Term[G] => // at the end
    }
    rec(mutableChain.start, chain)
    mutableChain
  }

}
