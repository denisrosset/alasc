package net.alasc.perms
package chain

import spire.algebra.{Group, Eq}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.finite.Grp
import net.alasc.prep.bsgs

class PermGrpChainBuilder[G] extends PermGrpBuilder[G] {

  implicit def internal: Internal[G, I]

  implicit def groupI: Group[I]

  implicit def equI: Eq[I]

  implicit def permutationActionI: FaithfulPermutationAction[I]

  def trivial = new PermGrpChainExplicit[G](bsgs.Term[G], generatorsOpt = Opt.empty[G])

  def fromGenerators(generators: Iterable[G], baseGuideOpt: Opt[bsgs.BaseGuide] = Opt.empty[bsgs.BaseGuide]) = {
    val chain = bsgs.BuildChain.fromGenerators(generators, permutation, baseGuideOpt)
    new PermGrpChainExplicit(chain, generatorsOpt = Opt(generators))
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt, baseGuideOpt: Opt[bsgs.BaseGuide] = Opt.empty[bsgs.BaseGuide]) = {
    val chain = bsgs.BuildChain.fromGeneratorsAndOrder(generators, order, permutation, baseGuideOpt)
    new PermGrpChainExplicit(chain, generatorsOpt = Opt(generators))
  }

  def fromGrp(grp: Grp[G], baseGuideOpt: Opt[bsgs.BaseGuide] = Opt.empty[bsgs.BaseGuide]) = grp match {
    case cg: PermGrpChain[G] =>


    case _ => fromGeneratorsAndOrder()
  }

  def conjugatedBy(grp: Grp[G], h: G): Grp[G] = grp match {
    case expl: PermGrpChainExplicit[G] =>
      new PermGrpChainConjugated[G](expl.chain, h, h.inverse,
        originalGeneratorsOpt = Opt(expl.generators))
    case conj: PermGrpChainConjugated[G] =>
      new PermGrpChainConjugated[G](conj.originalChain, conj.g |+| h, h.inverse |+| conj.gInv,
        originalGeneratorsOpt = Opt(conj.originalGenerators))
    case _ =>
      val hInv = h.inverse
      fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
  }

}
