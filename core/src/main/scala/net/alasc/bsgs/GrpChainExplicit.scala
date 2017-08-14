package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.rep.FaithfulPermRep
import net.alasc.syntax.group._

final class GrpChainExplicit[G, F <: PermutationAction[G] with Singleton]
  (val chain: Chain[G, F], generatorsOpt: Opt[IndexedSeq[G]], val kernel: Chain.Generic[G])
  (implicit val classTag: ClassTag[G], val equ: Eq[G], val group: Group[G], val action: F) extends GrpChain[G, F] {

  def chainOpt = Opt(chain)

  /** Number of group generators. */
  def nGenerators = generatorsOpt match {
    case Opt(g) => g.size
    case _ => kernel match {
      case _: Term[G, _] => chain.nStrongGenerators
      case _: Node[G, _] => chain.nStrongGenerators + kernel.nStrongGenerators
    }
  }

  /** Returns the i-th generator. */
  def generator(i: Int) = generatorsOpt match {
    case Opt(g) => g(i)
    case _ =>
      val nChainGenerators = chain.nStrongGenerators
      kernel match {
        case node: Node[G, _] if i >= nChainGenerators => kernel.kthStrongGenerator(i - nChainGenerators)
        case _ => chain.kthStrongGenerator(i)
      }
  }

  def generators = generatorsOpt match {
    case Opt(g) => g
    case _ => kernel match {
      case _: Term[_, _] => chain.strongGeneratingSet
      case _: Node[G, _] => chain.strongGeneratingSet ++ kernel.strongGeneratingSet
    }
  }

  def iterator = kernel match {
    case _: Term[_, _] => chain.elementsIterator
    case _: Node[G, _] =>
      for (g <- chain.elementsIterator; k <- kernel.elementsIterator) yield g |+| k
  }

  def contains(g: G) = kernel match {
    case _: Term[_, _] => chain.sift(g) match {
      case Opt(sifted) => sifted.isId
      case _ => false
    }
    case _: Node[G, _] => chain.sift(g) match {
      case Opt(sifted) => kernel.sift(sifted) match {
        case Opt(kernelSifted) => kernelSifted.isId
        case _ => false
      }
      case _ => false
    }
  }

  def order = chain.order * kernel.order

  def quotientOrder = chain.order

  def randomElement(random: Random): G = kernel match {
    case _: Term[_, _] => chain.randomElement(random)
    case _: Node[G, _] => chain.randomElement(random) |+| kernel.randomElement(random)
  }

  def enlargeKernel(newKernel: Chain.Generic[G]) = new GrpChainExplicit(chain, generatorsOpt, newKernel)

}
