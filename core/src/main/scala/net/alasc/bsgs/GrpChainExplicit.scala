package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction

final class GrpChainExplicit[G, F <: PermutationAction[G] with Singleton]
  (val chain: Chain[G, F], generatorsOpt: Opt[Iterable[G]])
  (implicit val classTag: ClassTag[G], val equ: Eq[G], val group: Group[G], val action: F) extends GrpChain[G, F] {

  def this(chain: Chain[G, F])(implicit action: F, classTag: ClassTag[G], equ: Eq[G], group: Group[G]) = this(chain, Opt.empty[Iterable[G]])

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
