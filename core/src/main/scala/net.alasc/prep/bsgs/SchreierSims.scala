package net.alasc.prep.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction

trait SchreierSims {

  def completeChainChangeAction[G:ClassTag:Eq:Group](oldChain: Chain[G], newAction: FaithfulPermutationAction[G], givenBase: Seq[Int] = Seq.empty): MutableChain[G] =
    completeChainFromGeneratorsRandomElementsAndOrder(oldChain.strongGeneratingSet, oldChain.randomElement(_), oldChain.order)(implicitly, implicitly, newAction, implicitly)

  def completeChainFromGenerators[G:ClassTag:Eq:FaithfulPermutationAction:Group](generators: Iterable[G], givenBase: Seq[Int] = Seq.empty): MutableChain[G]

  def completeChainFromGeneratorsAndOrder[G:ClassTag:Eq:FaithfulPermutationAction:Group](generators: Iterable[G], order: BigInt, givenBase: Seq[Int] = Seq.empty): MutableChain[G]

  def completeChainFromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:FaithfulPermutationAction:Group](generators: Iterable[G], randomElement: Random => G, order: BigInt, givenBase: Seq[Int] = Seq.empty): MutableChain[G]

  /** Deterministic Schreier-Sims algorithm. */
  def deterministicSchreierSims[G:ClassTag:Eq:FaithfulPermutationAction:Group](generators: Iterable[G], givenBase: Seq[Int] = Seq.empty): MutableChain[G] = {
    val mutableChain = MutableChain.incompleteWithGenerators(generators, givenBase)
    mutableChain.completeStrongGenerators()
    mutableChain
  }

}

final class SchreierSimsDeterministic extends SchreierSims {

  def completeChainFromGenerators[G:ClassTag:Eq:FaithfulPermutationAction:Group](generators: Iterable[G], givenBase: Seq[Int] = Seq.empty): MutableChain[G] =
    deterministicSchreierSims(generators, givenBase)

  def completeChainFromGeneratorsAndOrder[G:ClassTag:Eq:FaithfulPermutationAction:Group](generators: Iterable[G], order: BigInt, givenBase: Seq[Int] = Seq.empty): MutableChain[G] = {
    val mutableChain = deterministicSchreierSims(generators, givenBase)
    assert(mutableChain.start.next.order == order)
    mutableChain
  }

  def completeChainFromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:FaithfulPermutationAction:Group](generators: Iterable[G], randomElement: Random => G, order: BigInt, givenBase: Seq[Int] = Seq.empty): MutableChain[G] =
    completeChainFromGeneratorsAndOrder(generators, order, givenBase)

}

final class SchreierSimsRandomized(val random: Random) extends SchreierSims {

  def completeChainFromGenerators[G:ClassTag:Eq:FaithfulPermutationAction:Group]
    (generators: Iterable[G], givenBase: Seq[Int] = Seq.empty): MutableChain[G] =
    deterministicSchreierSims(generators, givenBase)

  def completeChainFromGeneratorsAndOrder[G:ClassTag:Eq:FaithfulPermutationAction:Group]
    (generators: Iterable[G], order: BigInt, givenBase: Seq[Int] = Seq.empty): MutableChain[G] = {
    import net.alasc.blackbox.RandomBag
    val bag = RandomBag(generators, random)
    randomizedSchreierSims(bag.randomElement(_), order, givenBase)
  }

  def completeChainFromGeneratorsRandomElementsAndOrder[G:ClassTag:Eq:FaithfulPermutationAction:Group]
    (generators: Iterable[G], randomElement: Random => G, order: BigInt, givenBase: Seq[Int] = Seq.empty): MutableChain[G] =
    randomizedSchreierSims(randomElement, order, givenBase)

  /* Randomized BSGS Schreier-Sims using the provided procedure to generate
   * random elements and the known order of the group to terminate the algorithm.
   * 
   * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
   */
  def randomizedSchreierSims[G:ClassTag:Eq:FaithfulPermutationAction:Group]
    (randomElement: Random => G, order: BigInt, givenBase: Seq[Int] = Seq.empty): MutableChain[G] = {
    val mutableChain = MutableChain.emptyWithBase(givenBase)
    while (mutableChain.start.next.order < order) {
      val siftResult = mutableChain.siftAndUpdateBaseFrom(mutableChain.start, randomElement(random))
      siftResult match {
        case Opt((nodeForGenerator, generator)) =>
          mutableChain.addStrongGeneratorHere(nodeForGenerator, generator, generator.inverse)
        case _ =>
      }
    }
    // TODO removeRedundantGenerators(mutableChain)
    mutableChain
  }

}

object SchreierSims {

  val deterministic: SchreierSims = new SchreierSimsDeterministic

  def randomized(implicit random: Random): SchreierSims = new SchreierSimsRandomized(random)

}
