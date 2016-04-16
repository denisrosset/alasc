package net.alasc.perms
package chain

import spire.syntax.group._
import spire.util.Opt

import net.alasc.prep.bsgs

class PermGrpChainConjugated[G](val originalChain: bsgs.Chain[G], val g: G, val gInv: G,
                                originalGeneratorsOpt: Opt[Iterable[G]] = Opt.empty[Iterable[G]])
                               (implicit val builder: PermGrpChainBuilder[G])
  extends PermGrpChain[G] {

  def originalGenerators = originalGeneratorsOpt match {
    case Opt(g) => g
    case _ => originalChain.strongGeneratingSet
  }

  def generators = originalGenerators.map(h => gInv |+| h |+| g)

  // TODO: make protected
  var chainOpt: Opt[bsgs.Chain[G]] = Opt.empty[bsgs.Chain[G]]

  def chain = chainOpt match {
    case Opt(computed) => computed
    case _ =>
      val computed = originalChain match {
        case node: bsgs.Node[G] =>
          val mut: bsgs.MutableChain[G] = imply(node.action) { node.mutableChain }
          mut.conjugate(g, gInv)
          mut.toChain()
        case term: bsgs.Term[G] => term
      }
      chainOpt = Opt(computed)
      computed
  }

}
