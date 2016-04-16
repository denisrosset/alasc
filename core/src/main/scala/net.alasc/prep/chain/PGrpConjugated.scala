package net.alasc.prep
package chain

import scala.util.Random

import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.finite._

import bsgs._

/** Represents a conjugated group from an original group G (represented by `originalChain`)
  * conjugated by g (with gInv == g.inverse).
  * The represented group is `H = gInv G g`.
  * 
  * @note The `representation` must be able to represent `g`.
  */
class PGrpConjugated[
    R0 <: FaithfulPRep[G] with Singleton,
    G
  ](
    val pRep: R0,
    val originalChain: Chain[G],
    val g: G,
    val gInv: G,
    originalGeneratorsOpt: Opt[Iterable[G]] = Opt.empty[Iterable[G]]
  )(
    implicit val builder: PGrpChainBuilder[G]
  ) extends PGrpChain[G] { lhs =>

  type R = R0

  def originalGenerators = originalGeneratorsOpt match {
    case Opt(g) => g
    case _ => originalChain.strongGeneratingSet
  }

  def generators = originalGenerators.map(h => gInv |+| h |+| g)

  var chainOpt: Opt[Chain[G]] = Opt.empty[Chain[G]]

  def chain = chainOpt match {
    case Opt(computed) => computed
    case _ =>
      val computed = originalChain match {
        case node: Node[G] =>
          val mut = imply(node.action) { node.mutableChain }
          mut.conjugate(g, gInv)
          mut.toChain()
        case term: Term[G] => term
      }
      chainOpt = Opt(computed)
      computed
  }

  def order = originalChain.order

  // `h in gInv G g` if and only if `g h gInv in G`.
  def contains(h: G) = originalChain.sifts(g |+| h |+| gInv)

  def iterator = originalChain.elementsIterator.map(h => gInv |+| h |+| g)

  def randomElement(random: Random) = {
    val h = originalChain.randomElement(random)
    gInv |+| h |+| g
  }

  override def conjugatedBy(h: G): Grp[G] =
    if (pRep.represents(h)) {
      val hInv = h.inverse
      new PGrpConjugated[R, G](pRep, originalChain, g |+| h, hInv |+| gInv)
    } else
      super.conjugatedBy(h)

  def pointwiseStabilizer(set: Set[Int]): Grp[G] = {
    val mut = imply(pRep.permutationAction) { originalChain.mutableChain }
    mut.conjugate(g, gInv)
    baseChange.changeBase(mut, PointwiseStabilizer.baseGuide(set))
    val guidedChain = mut.toChain()
    new PGrpExplicit[R, G](pRep, PointwiseStabilizer.recurse(guidedChain, set), Opt.empty[Iterable[G]])
  }

  def stabilizerTransversal(b: Int): (Grp[G], bsgs.Transversal[G]) = originalChain match {
    case node: Node[G] =>
      imply(pRep.permutationAction) {
        val a = b <|+| gInv
        if (node.inOrbit(a)) {
          val u = node.u(a)
          val uInv = node.uInv(a)
          val newG = u |+| g
          val newGInv = gInv |+| uInv
          val nextGrp = new PGrpConjugated[R, G](pRep, node.next, newG, newGInv, Opt.empty[Iterable[G]])
          val trv = ConjugatedTransversal(node, newG, newGInv)
          (nextGrp, trv)
        } else if (node.isFixed(a))
          (this, Transversal.empty(b))
        else {
          val newChain = BuildChain.fromChain(originalChain, pRep.permutationAction, Opt(BaseGuideSeq(Seq(a))))
          val (nextOriginalChain, originalTransversal) = newChain.detach(a)
          val nextGrp = new PGrpConjugated[R, G](pRep, nextOriginalChain, g, gInv, Opt.empty[Iterable[G]])
          val trv = ConjugatedTransversal(originalTransversal, g, gInv)
          (nextGrp, trv)
        }
      }
    case term: Term[G] => (this, bsgs.Transversal.empty[G](b))
  }

  def stabilizerTransversal: Opt[(Grp[G], bsgs.Transversal[G])] = chain match {
    case node: Node[G] => imply(pRep.permutationAction) {
      Opt((builder.fromChainSubgroupOfIn(this)(pRep, Opt.empty[BaseGuide])(node.next), ConjugatedTransversal(node, g, gInv)))
    }
    case _ => Opt.empty[(Grp[G], bsgs.Transversal[G])]
  }

}
