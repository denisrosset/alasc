package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.blackbox.RandomBag
import net.alasc.finite.{Grp, GrpStructure}
import net.alasc.perms.{FilterOrders, Perm}
import net.alasc.syntax.permutationAction._
import net.alasc.util.NNOption
import net.alasc.syntax.group._

/** Methods that construct a BSGS chain from generators and optional additional information. */
trait SchreierSims {

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A] =
    SchreierSims.deterministicSchreierSims(generators, kb, baseStart)

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A]

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A]

  /** Try to reduce the number of generators of a group described by a chain given a faithful action.
    *
    * @param node       Chain
    * @param generators Generators
    * @param min        Minimal number of generators
    * @tparam F         Faithful action type
    * @return a list of generators of size smaller than `generators`, otherwise Opt.empty.
    */
  def reduceGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton](node: Node[G, F], generators: IndexedSeq[G], min: Int): Opt[IndexedSeq[G]]

}

/** Common subroutines for all Schreier-Sims variants. */
object SchreierSims {

  /** Sifts the given element through the given mutable chain and kernel builder. If the element
    * cannot be completely sifted, add the sifted result as a strong generator.
    *
    * Returns whether a new strong generator has been added either to the coset chain or the kernel.
    */
  def siftAndAddStrongGenerator[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (mutableChain: MutableChain[G, A], element: G, kb: KernelBuilder[G])(implicit action: A): Boolean = {
    SchreierSims.siftAndUpdateBaseFrom(mutableChain, mutableChain.start, element) match {
      case SiftResult.Stop(generator, node) =>
        mutableChain.addStrongGeneratorHere(node, generator, generator.inverse)
        true
      case SiftResult.NotId(kernelElement) =>
        val kmc_ = kb.mutableChain // type juggling
      val faithfulAction = kmc_.start.action
        type F = faithfulAction.type
        implicit def ifa: F = faithfulAction
        val kmc: MutableChain[G, F] = kmc_.asInstanceOf[MutableChain[G, F]]
        siftAndAddStrongGenerator(kmc, kernelElement, KernelBuilder.trivial[G])
      case _: SiftResult.Id[G, A] => false // the element sifts, so no new strong generator
    }
  }

  /** Returns an element obtained by sifting `g` through the BSGS chain starting at `elem`,
    * inserting new base points as required, returns a [[SiftResult]].
    *
    * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
    */
  @tailrec def siftAndUpdateBaseFrom[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (mutableChain: MutableChain[G, A], at: StartOrNode[G, A], g: G): SiftResult[G, A] = {
    implicit def action: A = mutableChain.start.action
    at.next match {
      case term: Term[G, A] =>
        g.findMovedPoint match {
          case NNOption(k) =>
            val newNode = NodeBuilder[G].standalone(k)
            mutableChain.insertInChain(mutableChain.mutableStartOrNode(at), at.next, newNode)
            SiftResult.Stop(g, newNode)
          case _ if !g.isId => SiftResult.NotId[G, A](g)
          case _ => SiftResult.Id[G, A]
        }
      case node: Node[G, A] =>
        val b = node.beta <|+| g
        if (!node.inOrbit(b))
          SiftResult.Stop[G, A](g, mutableChain.mutable(node))
        else {
          val h = g |+| node.uInv(b)
          siftAndUpdateBaseFrom(mutableChain, node, h)
        }
    }
  }

  /** Finds if a new strong generator can be found at the given `node`, assuming that
    * the chain starting at `node.tail` is complete.
    *
    * Possible results are:
    *
    * - SiftResult.Id: the chain is complete starting from `node`
    * - SiftResult.NotId: there is a strong generator for the kb to test
    * - SiftResult.Stop: there is a new strong generator for this chain
    */
  def findNewStrongGeneratorAt[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (mutableChain: MutableChain[G, A], node: Node[G, A], kb: KernelBuilder[G]): SiftResult[G, A] = {
    implicit def action: A = node.action
    node.foreachOrbit { b =>
      val ub = node.u(b)
      node.strongGeneratingSet.foreach { x =>
        val i = b <|+| x
        if (!node.inOrbit(i))
          return SiftResult.Stop(x, mutableChain.mutable(node))
        val ubx = ub |+| x
        if (ubx =!= node.u(i)) {
          val schreierGen = ubx |+| node.uInv(i)
          siftAndUpdateBaseFrom(mutableChain, node, schreierGen) match {
            case _: SiftResult.Id[G, A] => // this element sifts, test the next one
            case stop: SiftResult.Stop[G, A] => return stop // we found a new strong generator
            case SiftResult.NotId(kg) => // we have an nontrivial element to sift through the kb
              val kmc_ = kb.mutableChain // type juggling
              val faithfulAction = kmc_.start.action
              type FA = faithfulAction.type
              implicit def ifa: FA = faithfulAction
              val kmc: MutableChain[G, FA] = kmc_.asInstanceOf[MutableChain[G, FA]]
              siftAndUpdateBaseFrom(kmc, kmc.start, kg) match {
                case _: SiftResult.NotId[G, FA] => sys.error("Elements are sifted completely by a faithful action")
                case _: SiftResult.Id[G, FA] => // do nothing, element is already contained
                case SiftResult.Stop(newKernelGenerator, where) =>
                  kmc.addStrongGeneratorHere(where, newKernelGenerator, newKernelGenerator.inverse)
                  completeStrongGeneratorsAt(kmc, where, KernelBuilder.trivial[G])
              }
          }
        }
      }
    }
    SiftResult.Id[G, A]
  }

  /** Completes the set of strong generators starting at `node`, assuming
    * that `node.tail` is already completed.
    *
    * Inspired (but rewritten) from SCHREIERSIMS, page 91 of Holt (2005).
    */
  @tailrec final def completeStrongGeneratorsAt[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (mutableChain: MutableChain[G, A], mutableNode: MutableNode[G, A], kb: KernelBuilder[G]): Unit =
  findNewStrongGeneratorAt(mutableChain, mutableNode, kb) match {
    case SiftResult.Stop(newGenerator, where) =>
      // there is a new strong generator at the node `where`, add it there and restart the search there
      implicit def action = mutableChain.start.action
      mutableChain.addStrongGeneratorHere(where, newGenerator, newGenerator.inverse)
      completeStrongGeneratorsAt(mutableChain, where, kb)
    case SiftResult.NotId(kernelGenerator) => sys.error("This case is already handled during the strong generator search")
    case _: SiftResult.Id[G, A] => mutableNode.prev match {
      case IsMutableNode(mutablePrev) =>
        // current node does not have new strong generators, but node has parent that has to be completed
        completeStrongGeneratorsAt(mutableChain, mutablePrev, kb)
      case node: Node[G, A] => sys.error("mutableNode.prev cannot be immutable")
      case start: Start[G, A] =>
      // current node does not have new strong generators, and current node starts the chain, we are finished
    }
  }

  /** Deterministic Schreier-Sims algorithm, completes the given mutable chain and the given kb as well. */
  final def completeStrongGenerators[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (mutableChain: MutableChain[G, A], kb: KernelBuilder[G]): Unit =
    mutableChain.findLast() match {
      case _: Start[G, A] => // chain is empty, no generators to find
      case node: Node[G, A] => completeStrongGeneratorsAt(mutableChain, mutableChain.mutable(node), kb)
    }

  /** Deterministic Schreier-Sims algorithm. Constructs a BSGS chain, and adds completely sifted generators to the kb,
    * if relevant.
    */
  def deterministicSchreierSims[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], kb: KernelBuilder[G], givenBase: Seq[Int] = Seq.empty)(implicit action: A): MutableChain[G, A] = {
    val mutableChain = MutableChain.incompleteWithGenerators(generators, kb, givenBase)
    completeStrongGenerators(mutableChain, kb)
    mutableChain
  }

  val deterministic: SchreierSims = new SchreierSimsDeterministic

  def randomized(implicit random: Random): SchreierSims = new SchreierSimsRandomized(random)

}

final class SchreierSimsDeterministic extends SchreierSims {

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A] = {
    val mc = mutableChain[G, A](generators, kb, baseStart)
    assert(mc.start.next.order * kb.order === order)
    mc
  }

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: (Random) => G, order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A] =
    mutableChain(generators, order, kb, baseStart)

  def reduceGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton](node: Node[G, F], generators: IndexedSeq[G], min: Int): Opt[IndexedSeq[G]] = {
    implicit def f: F = node.action
    def computeOrder(gens: IndexedSeq[G]): SafeLong = mutableChain[G, F](gens, KernelBuilder.trivial[G], Seq.empty[Int]).start.next.order
    GrpStructure.deterministicReduceGenerators(generators, node.order, computeOrder)
  }

}

final class SchreierSimsRandomized(val random: Random) extends SchreierSims {

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A] = {
    import net.alasc.blackbox.RandomBag
    val bag = RandomBag(generators, random)
    randomizedSchreierSims(bag, order, kb, baseStart)
  }

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: (Random) => G, order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A] =
    randomizedSchreierSims(randomElement, order, kb, baseStart)


  /** Randomized BSGS Schreier-Sims using the provided procedure to generate
    * random elements and the known order of the group to terminate the algorithm.
    *
    * Based on Holt (2005) RANDOMSCHREIER procedure, page 98.
    */
  def randomizedSchreierSims[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (randomElement: Random => G, order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int] = Seq.empty)(implicit action: A): MutableChain[G, A] = {
    val mutableChain = MutableChain.emptyWithBase[G, A](baseStart)
    while (mutableChain.start.next.order * kb.order < order) {
      SchreierSims.siftAndAddStrongGenerator(mutableChain, randomElement(random), kb)
    }
    mutableChain
  }

  def fastOrderCheck[G:ClassTag:Eq:Group](generators: IndexedSeq[G], order: SafeLong, faithfulAction: PermutationAction[G], numSuccTries: Int = 4): Boolean = {
    implicit def ia: faithfulAction.type = faithfulAction
    val randomBag = RandomBag(generators, random)
    val mutableChain = MutableChain.empty[G, faithfulAction.type]
    var succTries = 0
    while (mutableChain.start.next.order < order && succTries < numSuccTries) {
      if (SchreierSims.siftAndAddStrongGenerator[G, faithfulAction.type](mutableChain, randomBag(random), KernelBuilder.trivial[G]))
        succTries = 0
      else
        succTries += 1
    }
    val c = mutableChain.start.next.order.compare(order)
    require(c <= 0)
    c == 0
  }

  def reduceGenerators[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton](node: Node[G, F], generators: IndexedSeq[G], min: Int): Opt[IndexedSeq[G]] = {
    // taken from GAP SmallGeneratingSet, the part after the order filtering
    val order = node.order
    var gens = generators
    var i: Int = 0
    if (gens.length > 2) {
      i = spire.math.max(2, min)
      while (i <= min + 1 && i < gens.length) {
        var j = 1
        while (j <= 5 && i < gens.length) {
          val testGens = IndexedSeq.fill(i)(node.randomElement(random)).filterNot(_.isId)
          if (fastOrderCheck(testGens, order, node.action))
            gens = testGens
          j += 1
        }
        i += 1
      }
    }
    i = if (GrpStructure.isCommutativeFromGenerators(gens)) 1 else 2
    while (i <= gens.length && gens.length > min) {
      // random did not improve much, try subsets
      val testGens = gens.patch(i, Nil, 1)
      if (fastOrderCheck(testGens, order, node.action))
        gens = testGens
      else
        i += 1
    }
    if (gens.length < generators.length) Opt(gens) else Opt.empty[IndexedSeq[G]]
  }

}
