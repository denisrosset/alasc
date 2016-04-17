package net.alasc.perms
package chain

import net.alasc.algebra.Permutation
import net.alasc.prep.bsgs.Intersection
import spire.algebra.{Group, Eq}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.finite.Grp
import net.alasc.prep.bsgs._

class PermGrpChainBuilder[G] extends PermGrpBuilder[G] {

  implicit def permutation: Permutation[G]

  type GG = PermGrpChain[G]

  def trivial = new PermGrpChainExplicit[G](Term[G], generatorsOpt = Opt.empty[G])

  def fromGenerators(generators: Iterable[G], baseGuideOpt: Opt[bsgs.BaseGuide] = Opt.empty[BaseGuide]) = {
    val chain = BuildChain.fromGenerators(generators, permutation, baseGuideOpt)
    new PermGrpChainExplicit(chain, generatorsOpt = Opt(generators))
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt, baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide]) = {
    val chain = BuildChain.fromGeneratorsAndOrder(generators, order, permutation, baseGuideOpt)
    new PermGrpChainExplicit(chain, generatorsOpt = Opt(generators))
  }

  def fromGrp(grp: Grp[G], baseGuideOpt: Opt[BaseGuide] = Opt.empty[BaseGuide]) = grp match {
    case cg: PermGrpChain[G] => baseGuideOpt match {
      case Opt(baseGuide) => chainOpt match {
        case Opt(chain) if baseGuide.isSatisfiedBy(chain) => cg
        case _ => fromGeneratorsAndOrder(grp.generators, grp.order, baseGuideOpt)
      }
      case _ => cg
    }
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order, baseGuideOpt)
  }

  def conjugatedBy(grp: Grp[G], h: G): PermGrpChain[G] = grp match {
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

  def intersect(lhs: Grp[G], rhs: Grp[G]): PermGrpChain[G] =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) rhs
    else {
      val rhsChain = fromGrp(rhs).chain
      subgroupFor(lhs, Intersection(permutation, rhsChain))
    }

  def union(lhs: Grp[G], rhs: Grp[G]): PermGrpChain[G] =
    if (rhs.order > lhs.order) union(rhs.order, lhs.order) // ensure that lhs.order is the greatest
    else if (lhs.hasSubgroup(rhs)) lhs
    else {
      val lhsChain = fromGrp(lhs).chain
      val mutableChain = BuildMutableChain.fromChain(lhsChain, permutation)
      mutableChain.insertGenerators(generatorsToAdd)
      mutableChain.completeStrongGenerators()
      new PermGrpChainExplicit(mutableChain.toChain(), Opt(lhs.generators ++ rhs.generators))
    }

  def subgroupFor(grp: Grp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): PermGrpChain[G] =
    subgroupFor(grp, SubgroupDefinition(permutation, backtrackTest, predicate))

  def subgroupFor(grp: PermGrpChain[G], definition: SubgroupDefinition[G]): PermGrpChain[G] = {
    val guidedChain = grp match {
      case lhs: PermGrpChainConjugated[G] =>
        import lhs.{originalChain, g, gInv}
        val mut = originalChain.mutableChain
        mut.conjugate(g, gInv)
        definition.baseGuideOpt match {
          case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
          case _ =>
        }
        mut.toChain()
      case lhs: PermGrpChain[G] =>
        BuildChain.fromChain(lhs.chain, permutation, definition.baseGuideOpt)
      case _ =>
        BuildChain.fromGeneratorsAndOrder(grp.generators, grp.order, permutation, definition.baseGuideOpt)
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
    new PermGrpExplicit[G](subChain)
  }

}
