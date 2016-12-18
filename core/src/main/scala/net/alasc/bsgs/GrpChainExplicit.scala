package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.perms.FaithfulPermRep

final class GrpChainExplicit[G, F <: PermutationAction[G] with Singleton]
  (val chain: Chain[G, F], generatorsOpt: Opt[IndexedSeq[G]], val kernelOpt: Opt[Grp[G]])
  (implicit val classTag: ClassTag[G], val equ: Eq[G], val group: Group[G], val action: F) extends GrpChain[G, F] {

  def this(chain: Chain[G, F], generatorsOpt: Opt[IndexedSeq[G]])
          (implicit classTag: ClassTag[G], equ: Eq[G], group: Group[G], action: F)
  = this(chain, generatorsOpt, Opt.empty[Grp[G]])

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
