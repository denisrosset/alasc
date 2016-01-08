package net.alasc.prep
package chain

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.Permutation
import net.alasc.domains.Partition
import net.alasc.finite._

import bsgs._

final class PGrpExplicit[R0 <: FaithfulPRep[G] with Singleton, G](val pRep: R0, val chain: Chain[G], generatorsOpt: Opt[Iterable[G]] = Opt.empty[Iterable[G]])(implicit val builder: PGrpChainBuilder[G]) extends PGrpChain[G] { lhs =>

  type R = R0

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
    new PGrpExplicit[R, G](pRep, result)
  }

  def pointwiseStabilizer(set: Set[Int]): Grp[G] = {
    val guidedChain = BuildChain.fromChain(chain, pRep.permutationAction,
      Opt(PointwiseStabilizer.baseGuide(set)))
    new PGrpExplicit[R, G](pRep, PointwiseStabilizer.recurse(guidedChain, set))
  }

  def stabilizerTransversal: Opt[(Grp[G], bsgs.Transversal[G])] = chain match {
    case node: Node[G] => Opt((builder.fromChainIn(pRep)(node.next), node))
    case _ => Opt.empty[(Grp[G], bsgs.Transversal[G])]
  }

  def stabilizerTransversal(b: Int): (PGrp.In[R0, G], bsgs.Transversal[G]) = chain match {
    case term: Term[G] => (this, bsgs.Transversal.empty[G](b))
    case node: Node[G] if node.inOrbit(b) =>
      val u = node.u(b)
      val uInv = node.uInv(b)
      val nextGrp = new PGrpConjugated(pRep, node.next, u, uInv)
      val trv = imply(pRep.permutationAction) { ConjugatedTransversal(node, u, uInv) }
      (nextGrp, trv)
    case node: Node[G] if node.isFixed(b) =>
      (this, bsgs.Transversal.empty(b))
    case _ =>
      val newChain = BuildChain.fromChain(chain, pRep.permutationAction, Opt(BaseGuideSeq(Seq(b))))
      val (nextChain, trv) = newChain.detach(b)
      val nextGrp = new PGrpExplicit(pRep, nextChain)
      (nextGrp, trv)
  }

  override def conjugatedBy(h: G, hInvOpt: Opt[G] = Opt.empty): Grp[G] =
    if (pRep.represents(h)) {
      val hInv = hInvOpt match {
        case Opt(e) => e
        case _ => h.inverse
      }
      new PGrpConjugated[R, G](pRep, chain, h, hInv)
    } else super.conjugatedBy(h, hInvOpt)

}
