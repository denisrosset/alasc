package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction

trait SchreierSims {

  def completeChainChangeAction[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (oldChain: Chain[G, F], newAction: PermutationAction[G], givenBase: Seq[Int] = Seq.empty): MutableChain[G, newAction.type] = {
    implicit def newActionValue: newAction.type = newAction
    completeChainFromGeneratorsRandomElementsAndOrder[G, newAction.type](oldChain.strongGeneratingSet, oldChain.randomElement(_), oldChain.order)
  }

  def completeChainFromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F]

  def completeChainFromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F]

  def completeChainFromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F]

  /** Deterministic Schreier-Sims algorithm. */
  def deterministicSchreierSims[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] = {
    val mutableChain = MutableChain.incompleteWithGenerators(generators, givenBase)
    mutableChain.completeStrongGenerators()
    mutableChain
  }

  def siftAndAddStrongGenerator[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (mutableChain: MutableChain[G, F], element: G)(implicit action: F): Unit = {
    val siftResult = mutableChain.siftAndUpdateBaseFrom(mutableChain.start, element)
    siftResult match {
      case Opt((nodeForGenerator, generator)) =>
        mutableChain.addStrongGeneratorHere(nodeForGenerator, generator, generator.inverse)
      case _ =>
    }
  }

}

final class SchreierSimsDeterministic extends SchreierSims {

  def completeChainFromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] =
    deterministicSchreierSims(generators, givenBase)

  def completeChainFromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] = {
    val mutableChain = deterministicSchreierSims(generators, givenBase)
    assert(mutableChain.start.next.order == order)
    mutableChain
  }

  def completeChainFromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] =
    completeChainFromGeneratorsAndOrder(generators, order, givenBase)

}

final class SchreierSimsRandomized(val random: Random) extends SchreierSims {

  def completeChainFromGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] =
    deterministicSchreierSims(generators, givenBase)

  def completeChainFromGeneratorsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], order: SafeLong, givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] = {
    import net.alasc.blackbox.RandomBag
    val bag = RandomBag(generators, random)
    randomizedSchreierSims(bag.randomElement(_), order, givenBase)
  }

  def completeChainFromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (generators: Iterable[G], randomElement: Random => G, order: SafeLong, givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] =
    randomizedSchreierSims(randomElement, order, givenBase)

  /* Randomized BSGS Schreier-Sims using the provided procedure to generate
   * random elements and the known order of the group to terminate the algorithm.
   * 
   * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
   */
  def randomizedSchreierSims[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (randomElement: Random => G, order: SafeLong, givenBase: Seq[Int] = Seq.empty)(implicit action: F): MutableChain[G, F] = {
    val mutableChain = MutableChain.emptyWithBase[G, F](givenBase)
    while (mutableChain.start.next.order < order) {
      siftAndAddStrongGenerator(mutableChain, randomElement(random))
    }
    // TODO removeRedundantGenerators(mutableChain)
    mutableChain
  }

}

object SchreierSims {

  val deterministic: SchreierSims = new SchreierSimsDeterministic

  def randomized(implicit random: Random): SchreierSims = new SchreierSimsRandomized(random)

}
