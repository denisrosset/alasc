package net.alasc.perms
package chain

import spire.util.Opt
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong

import net.alasc.algebra.Permutation
import net.alasc.bsgs.Chain
import net.alasc.bsgs._

final class PermGrpChainExplicit[G](val chain: Chain[G], generatorsOpt: Opt[Iterable[G]])
                                   (implicit val equ: Eq[G],
                                    val group: Group[G],
                                    val permutation: Permutation[G])
  extends PermGrpChain[G] {

  def this(chain: Chain[G])(implicit permutation: Permutation[G]) = this(chain, Opt.empty[Iterable[G]])

  def chainOpt = Opt(chain)

  def generators = generatorsOpt match {
    case Opt(g) => g
    case _ => chain.strongGeneratingSet
  }

  def iterator = chain.elementsIterator

  def contains(g: G) = chain.sifts(g)

  def order: SafeLong = chain.order

  def randomElement(random: Random): G = chain.randomElement(random)


}
