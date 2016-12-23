package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction

/** Represents a conjugated group from an original group G (represented by `originalChain`)
  * conjugated by g (with gInv == g.inverse).
  * The represented group is `H = gInv G g`.
  */
final class GrpChainConjugated[G, A <: PermutationAction[G] with Singleton]
(val originalChain: Chain[G, A], val g: G, val gInv: G,
 originalGeneratorsOpt: Opt[IndexedSeq[G]], val kernel: Chain.Generic[G])
(implicit val classTag: ClassTag[G], val group: Group[G], val equ: Eq[G], val action: A) extends GrpChain[G, A] {

  def originalGenerators = originalGeneratorsOpt match {
    case Opt(gen) => gen
    case _ => kernel match {
      case _: Term[_, _] => originalChain.strongGeneratingSet
      case _: Node[_, _] => originalChain.strongGeneratingSet ++ kernel.strongGeneratingSet
    }
  }

  def generators = originalGenerators.map(h => gInv |+| h |+| g)

  private[this] var _chainOpt: Opt[Chain[G, A]] = Opt.empty[Chain[G, A]]

  def chainOpt = _chainOpt
  def chain = _chainOpt match {
    case Opt(computed) => computed
    case _ =>
      val computed = originalChain match {
        case node: Node[G, A] =>
          val mut: MutableChain[G, A] = node.mutableChain
          mut.conjugate(g, gInv)
          mut.toChain()
        case term: Term[G, A] => term
      }
      _chainOpt = Opt(computed)
      computed
  }

  def order = originalChain.order * kernel.order

  // `h in gInv G g` if and only if `g h gInv in G`.
  def contains(h: G) = {
    val original = g |+| h |+| gInv
    kernel match {
      case _: Term[_, _] => originalChain.sift(original) match {
        case Opt(sifted) => sifted.isId
        case _ => false
      }
      case _: Node[G, _] => originalChain.sift(original) match {
        case Opt(sifted) => kernel.sift(sifted) match {
          case Opt(kernelSifted) => kernelSifted.isId
          case _ => false
        }
        case _ => false
      }
    }
  }

  def iterator = kernel match {
    case _: Term[_, _] => originalChain.elementsIterator.map(h => gInv |+| h |+| g)
    case _: Node[G, _] => for (c <- originalChain.elementsIterator; k <- kernel.elementsIterator) yield gInv |+| c |+| k |+| g

  }

  def randomElement(random: Random): G = kernel match {
    case _: Term[_, _] => gInv |+| chain.randomElement(random) |+| g
    case _: Node[G, _] => gInv |+| chain.randomElement(random) |+| kernel.randomElement(random) |+| g
  }

  def enlargeKernel(newKernel: Chain.Generic[G]) = new GrpChainConjugated(originalChain, g, gInv, originalGeneratorsOpt, newKernel)

}
