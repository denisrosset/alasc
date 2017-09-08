package net.alasc.bsgs.internal

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{Chain, GrpChain, MutableChain, Node, Term}
import net.alasc.syntax.group._
import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.util.Opt

import scala.reflect.ClassTag
import scala.util.Random

/** Represents a conjugated group from an original group G (represented by "originalChain")
  * conjugated by g (with gInv == g.inverse).
  * The represented group is "H = gInv G g".
  *
  * The conjugating element "g" must satisfy:
  * - "g" can be represented by the permutation action "A",
  * - "g" commutes with the kernel: "g K = K g".
  *
  * For efficiency, only the chain is conjugated by g, while the kernel is left unchanged.
  */
final class GrpChainConjugated[G, A <: PermutationAction[G] with Singleton]
(val originalChain: Chain[G, A], val g: G, val gInv: G,
 val originalGeneratorsOpt: Opt[Seq[G]], val kernel: Chain.Generic[G])
(implicit val classTag: ClassTag[G], val group: Group[G], val equ: Eq[G], val action: A) extends GrpChain[G, A] {

  /** Number of group generators. */
  def nGenerators = originalGeneratorsOpt match {
    case Opt(g) => g.size
    case _ => kernel match {
      case _: Term[G, _] => originalChain.nStrongGenerators
      case _: Node[G, _] => originalChain.nStrongGenerators + kernel.nStrongGenerators
    }
  }

  /** Returns the i-th generator. */
  def generator(i: Int) = originalGeneratorsOpt match {
    case Opt(og) => gInv |+| og(i) |+| g
    case _ =>
      val nChainGenerators = originalChain.nStrongGenerators
      kernel match {
        case node: Node[G, _] if i >= nChainGenerators => kernel.kthStrongGenerator(i - nChainGenerators)
        case _ => gInv |+| originalChain.kthStrongGenerator(i) |+| g
      }
  }

  def generatorsOpt = originalGeneratorsOpt.map(_.map(h => gInv |+| h |+| g))

  def generators = new IndexedSeq[G] {
    def apply(i: Int) = generator(i)
    def length = nGenerators
  }

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

  def quotientOrder = originalChain.order

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
    case _: Term[_, _] => gInv |+| originalChain.randomElement(random) |+| g
    case _: Node[G, _] => gInv |+| originalChain.randomElement(random) |+| kernel.randomElement(random) |+| g
  }

  def enlargeKernel(newKernel: Chain.Generic[G]) = new GrpChainExplicit(chain, generatorsOpt, newKernel)

}
