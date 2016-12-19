package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.group._
import spire.syntax.eq._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.syntax.permutationAction._
import net.alasc.util.NNOption

/** Data structure to store the mutable chain of the kb of a group.
  * The mutable chain is constructed lazily. */
abstract class KernelBuilder[G] {

  private[this] var _opt: Opt[MutableChain.Generic[G]] = Opt.empty[MutableChain.Generic[G]]

  def order: SafeLong = _opt match {
    case Opt(mc) => mc.start.next.order
    case _ => SafeLong.one
  }

  protected def makeMutableChain(): MutableChain.Generic[G]

  def replaceChain(chain: Chain.Generic[G]): Unit = chain match {
    case _: Term[G, _] => _opt = Opt.empty[MutableChain.Generic[G]]
    case node1: Node[G, _] =>
      val action = node1.action
      implicit def ia: action.type = action
      val node: Node[G, action.type] = node1.asInstanceOf[Node[G, action.type]]
      val mc = MutableChain.empty[G, action.type]
      mc.start.next = node
      _opt = Opt(mc)
  }

  def mutableChain: MutableChain.Generic[G] = _opt match {
    case Opt(mc) => mc
    case _ =>
      val mc = makeMutableChain()
      _opt = Opt(mc)
      mc
  }

  def toChain(): Chain.Generic[G] = _opt match {
    case Opt(mc) => mc.toChain()
    case _ => Term.generic[G]
  }

}

object KernelBuilder {

  private[this] val trivialInstance = new KernelBuilder[AnyRef] {
    override def order = SafeLong.one
    protected def makeMutableChain() = sys.error("This kb should be trivial")
    override def toChain(): Chain.Generic[AnyRef] = Term.generic[AnyRef]
  }

  def trivial[G]: KernelBuilder[G] = trivialInstance.asInstanceOf[KernelBuilder[G]]

  class Prepared[G](mc: MutableChain.Generic[G]) extends KernelBuilder[G] {
    def makeMutableChain() = mc
  }

  def fromChain[G:ClassTag:Eq:Group](chain: Chain.Generic[G]) = chain match {
    case _: Term.Generic[G] => trivial[G]
    case node: Node.Generic[G] =>
      val action: PermutationAction[G] = node.action
      implicit def ia: action.type = action
      val mc = node.asInstanceOf[Node[G, action.type]].mutableChain
      new Prepared[G](mc)
  }

}

/** Result of a sifting operation. */
sealed abstract class SiftResult[G, A <: PermutationAction[G] with Singleton]

object SiftResult {

  /** Represents the result of a completely sifted element equal to the identity. */
  final class Id[G, A <: PermutationAction[G] with Singleton] private[SiftResult]() extends SiftResult[G, A]

  object Id {

    private[this] val instance: Id[Perm, Perm.algebra.type] = new Id[Perm, Perm.algebra.type]

    def apply[G, A <: PermutationAction[G] with Singleton] = instance.asInstanceOf[Id[G, A]]

  }

  /** Represents the result of a completely sifted element not equal to the identity, thus
    * part of the kb of the action. */
  case class NotId[G, A <: PermutationAction[G] with Singleton](remainder: G) extends SiftResult[G, A]

  /** Represents the result of an incomplete sift, with
    * implicitly[A].movesAnyPoint(remainder) true. The returned mutableNode `stop` is where
    * the remainder should be inserted as a strong generator.
    */
  case class Stop[G, A <:PermutationAction[G] with Singleton](remainder: G, node: MutableNode[G, A]) extends SiftResult[G, A]

}

/** Methods that construct a BSGS chain from generators and optional additional information. */
trait SchreierSims {

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A] =
    SchreierSims.deterministicSchreierSims(generators, kb, baseStart)

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A]

  def mutableChain[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (generators: Iterable[G], randomElement: Random => G, order: SafeLong, kb: KernelBuilder[G], baseStart: Seq[Int])(implicit action: A): MutableChain[G, A]

}

/** Common subroutines for all Schreier-Sims variants. */
object SchreierSims {

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
    val mutableChain = MutableChain.incompleteWithGenerators(generators, givenBase)
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

}

final class SchreierSimsRandomized(val random: Random) extends SchreierSims {

  def siftAndAddStrongGenerator[G:ClassTag:Eq:Group, A <: PermutationAction[G] with Singleton]
  (mutableChain: MutableChain[G, A], element: G, kb: KernelBuilder[G])(implicit action: A): Unit = {
    SchreierSims.siftAndUpdateBaseFrom(mutableChain, mutableChain.start, element) match {
      case SiftResult.Stop(generator, node) =>
        mutableChain.addStrongGeneratorHere(node, generator, generator.inverse)
      case SiftResult.NotId(kernelElement) =>
        val kmc_ = kb.mutableChain // type juggling
      val faithfulAction = kmc_.start.action
        type F = faithfulAction.type
        implicit def ifa: F = faithfulAction
        val kmc: MutableChain[G, F] = kmc_.asInstanceOf[MutableChain[G, F]]
        siftAndAddStrongGenerator(kmc, kernelElement, KernelBuilder.trivial[G])
      case _: SiftResult.Id[G, A] => // do nothing
    }
  }

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
      siftAndAddStrongGenerator(mutableChain, randomElement(random), kb)
    }
    mutableChain
  }

}
