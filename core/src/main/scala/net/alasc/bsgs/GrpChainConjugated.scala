package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.perms.FaithfulPermRep

/** Represents a conjugated group from an original group G (represented by `originalChain`)
  * conjugated by g (with gInv == g.inverse).
  * The represented group is `H = gInv G g`.
  */
final class GrpChainConjugated[G, F <: PermutationAction[G] with Singleton]
  (val originalChain: Chain[G, F], val g: G, val gInv: G,
   originalGeneratorsOpt: Opt[IndexedSeq[G]],
   val repOpt: Opt[FaithfulPermRep[G, _]])
  (implicit val classTag: ClassTag[G], val group: Group[G], val equ: Eq[G], val action: F) extends GrpChain[G, F] {

  def originalGenerators = originalGeneratorsOpt match {
    case Opt(gen) => gen
    case _ => originalChain.strongGeneratingSet
  }

  def generators = originalGenerators.map(h => gInv |+| h |+| g)

  private[this] var _chainOpt: Opt[Chain[G, F]] = Opt.empty[Chain[G, F]]

  def chainOpt = _chainOpt
  def chain = _chainOpt match {
    case Opt(computed) => computed
    case _ =>
      val computed = originalChain match {
        case node: Node[G, F] =>
          val mut: MutableChain[G, F] = node.mutableChain
          mut.conjugate(g, gInv)
          mut.toChain()
        case term: Term[G, F] => term
      }
      _chainOpt = Opt(computed)
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

}
