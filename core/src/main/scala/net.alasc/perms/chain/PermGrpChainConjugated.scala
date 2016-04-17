package net.alasc.perms
package chain

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.Permutation
import net.alasc.prep.bsgs._

/** Represents a conjugated group from an original group G (represented by `originalChain`)
  * conjugated by g (with gInv == g.inverse).
  * The represented group is `H = gInv G g`.
  */

class PermGrpChainConjugated[G](val originalChain: Chain[G], val g: G, val gInv: G,
                                originalGeneratorsOpt: Opt[Iterable[G]])
                               (implicit val classTag: ClassTag[G],
                                val group: Group[G],
                                val equ: Eq[G],
                                val permutation: Permutation[G])  extends PermGrpChain[G] {

  def this(originalChain: Chain[G], g: G, gInv: G)(implicit classTag: ClassTag[G], permutation: Permutation[G]) =
    this(originalChain, g, gInv, Opt.empty[Iterable[G]])

  def originalGenerators = originalGeneratorsOpt match {
    case Opt(gen) => gen
    case _ => originalChain.strongGeneratingSet
  }

  def generators = originalGenerators.map(h => gInv |+| h |+| g)

  // TODO: make protected
  var chainOpt: Opt[Chain[G]] = Opt.empty[Chain[G]]

  def chain = chainOpt match {
    case Opt(computed) => computed
    case _ =>
      val computed = originalChain match {
        case node: Node[G] =>
          val mut: MutableChain[G] = imply(node.action) { node.mutableChain }
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

}
