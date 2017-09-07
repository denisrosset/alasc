package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction

/** Data structure to store the mutable chain of the kb of a group.
  * The mutable chain is constructed lazily. */
abstract class KernelBuilder[G] {

  private[this] var _opt: Opt[MutableChain.Generic[G]] = Opt.empty[MutableChain.Generic[G]]

  def order: SafeLong = _opt match {
    case Opt(mc) => mc.start.next.order
    case _ => SafeLong.one
  }

  protected def makeMutableChain(): MutableChain.Generic[G]

  def replaceChain(chain: Chain.Generic[G]): Unit = chain match {
    case _: Term[G, _] => _opt = Opt.empty[MutableChain.Generic[G]]
    case node1: Node[G, _] =>
      val action = node1.action
      implicit def ia: action.type = action
      val node: Node[G, action.type] = node1.asInstanceOf[Node[G, action.type]]
      val mc = MutableChain.empty[G, action.type]
      mc.start.next = node
      _opt = Opt(mc)
  }

  def mutableChain: MutableChain.Generic[G] = _opt match {
    case Opt(mc) => mc
    case _ =>
      val mc = makeMutableChain()
      _opt = Opt(mc)
      mc
  }

  def toChain(): Chain.Generic[G] = _opt match {
    case Opt(mc) => mc.toChain()
    case _ => Term.generic[G]
  }

}

object KernelBuilder {

  private[this] val trivialInstance = new KernelBuilder[AnyRef] {
    override def order = SafeLong.one
    protected def makeMutableChain() = sys.error("This kb should be trivial")
    override def toChain(): Chain.Generic[AnyRef] = Term.generic[AnyRef]
  }

  def trivial[G]: KernelBuilder[G] = trivialInstance.asInstanceOf[KernelBuilder[G]]

  class Prepared[G](mc: MutableChain.Generic[G]) extends KernelBuilder[G] {
    def makeMutableChain() = mc
  }

  def fromChain[G:ClassTag:Eq:Group](chain: Chain.Generic[G]) = chain match {
    case _: Term.Generic[G] => trivial[G]
    case node: Node.Generic[G] =>
      val action: PermutationAction[G] = node.action
      implicit def ia: action.type = action
      val mc = node.asInstanceOf[Node[G, action.type]].mutableChain
      new Prepared[G](mc)
  }

}
