package net.alasc.prep
package chain

import scala.util.Random

import spire.syntax.group._
import spire.util.Opt

import net.alasc.finite._

import bsgs._

final class PGrpExplicit[
    Parent0 <: Grp[G] with Singleton,
    R0 <: FaithfulPRep[G] with Singleton,
    G
  ](
    val pRep: R0,
    val chain: Chain[G],
    generatorsOpt: Opt[Iterable[G]] = Opt.empty[Iterable[G]],
    val parentOrNull: Parent0 = null
  )(implicit
    val builder: PGrpChainBuilder[G]
  ) extends PGrpChain[G] { lhs =>

  type Parent = Parent0

  type R = R0

  protected[alasc] def copyWithParentOrNull(newParentOrNull: Grp[G]): Grp.SubgroupOf[newParentOrNull.type, G] =
    new PGrpExplicit[newParentOrNull.type, R0, G](pRep, chain, generatorsOpt, newParentOrNull)

  def chainOpt = Opt(chain)

  def generators = generatorsOpt match {
    case Opt(g) => g
    case _ => chain.strongGeneratingSet
  }

  def iterator = chain.elementsIterator

  def contains(g: G) = chain.sifts(g)

  def order: BigInt = chain.order

  def randomElement(random: Random): G = chain.randomElement(random)

  def subgroupFor(definition: SubgroupDefinition[G]): Grp[G] = {
    val guidedChain = BuildChain.fromChain(chain, pRep.permutationAction, definition.baseGuideOpt)
    val result = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
    new PGrpExplicit[this.type, R, G](pRep, result, Opt.empty[Iterable[G]], this)
  }

  def pointwiseStabilizer(set: Set[Int]): Grp[G] = {
    val guidedChain = BuildChain.fromChain(chain, pRep.permutationAction,
      Opt(PointwiseStabilizer.baseGuide(set)))
    new PGrpExplicit[this.type, R, G](pRep, PointwiseStabilizer.recurse(guidedChain, set), Opt.empty[Iterable[G]], this)
  }

  def stabilizerTransversal: Opt[(Grp[G], bsgs.Transversal[G])] = chain match {
    case node: Node[G] => Opt((builder.fromChainSubgroupOfIn(this)(pRep, Opt.empty[BaseGuide])(node.next), node))
    case _ => Opt.empty[(Grp[G], bsgs.Transversal[G])]
  }

  def stabilizerTransversal(b: Int): (PGrp.In[R, G], bsgs.Transversal[G]) = chain match {
    case term: Term[G] => (this, bsgs.Transversal.empty[G](b))
    case node: Node[G] if node.inOrbit(b) =>
      val u = node.u(b)
      val uInv = node.uInv(b)
      val nextGrp = new PGrpConjugated[this.type, R, G](pRep, node.next, u, uInv, Opt.empty[Iterable[G]], this)
      val trv = imply(pRep.permutationAction) { ConjugatedTransversal(node, u, uInv) }
      (nextGrp, trv)
    case node: Node[G] if node.isFixed(b) =>
      (this, bsgs.Transversal.empty(b))
    case _ =>
      val newChain = BuildChain.fromChain(chain, pRep.permutationAction, Opt(BaseGuideSeq(Seq(b))))
      val (nextChain, trv) = newChain.detach(b)
      val nextGrp = new PGrpExplicit[this.type, R, G](pRep, nextChain, Opt.empty[Iterable[G]], this)
      (nextGrp, trv)
  }

  override def conjugatedBy(h: G, hInvOpt: Opt[G] = Opt.empty): Grp[G] =
    if (pRep.represents(h)) {
      val hInv = hInvOpt match {
        case Opt(e) => e
        case _ => h.inverse
      }
      new PGrpConjugated[Null, R, G](pRep, chain, h, hInv)
    } else super.conjugatedBy(h, hInvOpt)

}
