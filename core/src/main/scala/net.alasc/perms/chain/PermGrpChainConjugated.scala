package net.alasc.perms
package chain

import scala.util.Random
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.prep.bsgs._

class PermGrpChainConjugated[G](val originalChain: Chain[G], val g: G, val gInv: G,
                                originalGeneratorsOpt: Opt[Iterable[G]])
                               (implicit val builder: PermGrpChainBuilder[G])
  extends PermGrpChain[G] {

  import builder.classTag

  def this(originalChain: Chain[G], g: G, gInv: G)(implicit builder: PermGrpChainBuilder[G]) =
    this(originalChain, g, gInv, Opt.empty[Iterable[G]])(builder)

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

  def base = originalChain.base.map(_ <|+| gInv)

}
