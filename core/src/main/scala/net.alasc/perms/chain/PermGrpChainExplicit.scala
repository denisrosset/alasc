package net.alasc.perms
package chain

import spire.util.Opt

import scala.util.Random

final class PermGrpChainExplicit[G, I](val chain: bsgs.Chain[I], generatorsOpt: Opt[Iterable[I]] = Opt.empty[Iterable[I]])
                                   (implicit val builder: PermGrpChainBuilder[G])
  extends PermGrpChain[G] {

  def chainOpt = Opt(chain)

  def generators = generatorsOpt match {
    case Opt(g) => g
    case _ => chain.strongGeneratingSet
  }

  def iterator = chain.elementsIterator

  def contains(g: G) = chain.sifts(g)

  def order: BigInt = chain.order

  def randomElement(random: Random): G = chain.randomElement(random)

}
