package net.alasc.perms
package chain

import spire.util.Opt
import scala.util.Random

import net.alasc.prep.bsgs._

final class PermGrpChainExplicit[G](val chain: Chain[G], generatorsOpt: Opt[Iterable[G]])
                                   (implicit val builder: PermGrpChainBuilder[G])
  extends PermGrpChain[G] {

  def this(chain: Chain[G])(implicit builder: PermGrpChainBuilder[G]) = this(chain, Opt.empty[Iterable[G]])(builder)

  def chainOpt = Opt(chain)

  def generators = generatorsOpt match {
    case Opt(g) => g
    case _ => chain.strongGeneratingSet
  }

  def iterator = chain.elementsIterator

  def contains(g: G) = chain.sifts(g)

  def order: BigInt = chain.order

  def randomElement(random: Random): G = chain.randomElement(random)

  def base = chain.base

}
