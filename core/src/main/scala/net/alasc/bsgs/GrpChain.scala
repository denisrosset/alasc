package net.alasc.bsgs

import scala.reflect.ClassTag
import scala.annotation.tailrec

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt
import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs.internal.{GrpChainConjugated, GrpChainExplicit}
import net.alasc.partitions.Partition
import net.alasc.finite._
import net.alasc.perms.{MutableOrbit, orbits}
import net.alasc.syntax.group._

/** Group described a BSGS chain of elements of type G using the permutation action F.
  *
  * If the action is faithful for the current group:
  * - The group is fully described by the chain.
  * - kernelOpt is empty.
  *
  * If the action is not faithful:
  * - kb contains the nontrivial kb normal subgroup K,
  * - The chain describes the quotient group.
  * - The group is described by all c |+| k where c in chain and k in kernel.
  */
abstract class GrpChain[G, A <: PermutationAction[G] with Singleton] extends Grp[G] { lhs =>

  implicit val action: A

  implicit def classTag: ClassTag[G]

  def chain: Chain[G, A]

  def chainOpt: Opt[Chain[G, A]]

  def kernel: Chain.Generic[G]

  /** Order of the quotient group G/K where G is this group and K is its kernel for its action. */
  def quotientOrder: SafeLong

  /** Returns a new GrpChain[G, A] with the kernel substituted.
    *
    * @param newKernel Kernel to substitute, of which the current kernel must be a subgroup.
    * @return The new group.
    */
  def enlargeKernel(newKernel: Chain.Generic[G]): GrpChain[G, A]

}

object GrpChain {

  type Generic[G] = GrpChain[G, _ <: PermutationAction[G] with Singleton]

  /** Returns a `GrpChain[G, action.type]` if it can be shown that the provided Grp[G] is of that type.
    *
    * @param grp    Group to check the type of.
    * @param action Action to match
    * @return       Opt(grpChain) if the group is a GrpChain for the given action, otherwise Opt.empty.
    */
  @inline final def extractGrpChain[G](grp: Grp[G], action: PermutationAction[G]): Opt[GrpChain[G, action.type]] =
    grp match {
      case gc: GrpChain[G, _] if gc.action eq action => Opt(gc.asInstanceOf[GrpChain[G, action.type]])
      case _ => Opt.empty[GrpChain[G, action.type]]
    }

  @inline final def extractAction[G](grp: Grp[G]): Opt[PermutationAction[G]] =
    grp match {
      case grp: GrpChain[G, _] => Opt(grp.action)
      case _ => Opt.empty[PermutationAction[G]]
    }

  /** Returns the action that the two given groups have in common, if any.
    *
    * @param lhs First group to test
    * @param rhs Second group to test
    * @return    Opt(action) if both groups are of type GrpChain[G, action.type], otherwise Opt.empty.
    */
  @inline final def commonAction[G](lhs: Grp[G], rhs: Grp[G]): Opt[PermutationAction[G]] =
    lhs match {
      case lhs1: GrpChain[G, _] => rhs match {
        case rhs1: GrpChain[G, _] if lhs1.action eq rhs1.action => Opt(lhs1.action)
        case _ => Opt.empty[PermutationAction[G]]
      }
      case _ => Opt.empty[PermutationAction[G]]
    }

  /** Casts the provided Grp[G] into GrpChain[G, action.type] for the provided action. The safety of the operation
    * is not verified. */
  @inline final def inActionUnsafe[G](lhs: Grp[G], action: PermutationAction[G]): GrpChain[G, action.type] = lhs.asInstanceOf[GrpChain[G, action.type]]

  /** Returns the union of the group `lhs` with the group defined by the generators `rhsGenerators`.
    *
    * @param lhs           First group.
    * @param rhsGenerators Generators of the second group.
    * @param kernel        Builder to build the kernel of the union group during the Schreier-Sims algorithm.
    * @tparam A            Action able to describe the generators of the first AND second group
    * @return a GrpChain describing the group union.
    */
  def unionComputeKernel[G, A <: PermutationAction[G] with Singleton]
  (lhs: GrpChain[G, A], rhsGenerators: Iterable[G], kernel: KernelBuilder[G]): GrpChain[G, A] = {
    import lhs.{action, classTag, equ, group}
    val mutableChain = lhs.chain.mutableChain
    val newGenerators = rhsGenerators.filterNot(lhs.contains)
    mutableChain.insertGenerators(newGenerators)
    SchreierSims.completeStrongGenerators(mutableChain, kernel)
    val newChain = mutableChain.toChain()
    val generatorsOpt = if (newChain.strongGeneratingSet.size >= lhs.generators.size + newGenerators.size)
        Opt(lhs.generators ++ newGenerators) else Opt.empty[Seq[G]]
    new GrpChainExplicit(mutableChain.toChain(), generatorsOpt, kernel.toChain())
  }

  /** Returns the union of the group `lhs` with the group defined by the generators `rhsGenerators`.
    *
    * @param lhs           Non empty BSGS chain describing the first group.
    * @param rhsGenerators Generators of the second group.
    * @tparam F            Faithful action able to describe the generators of the first AND second group
    * @return a BSGS chain describing the group union.
    */
  def unionFaithful[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton](lhs: Node[G, F], rhsGenerators: Iterable[G]): Chain[G, F] = {
    val action = lhs.action
    implicit def ia: action.type = action
    val mutableChain = lhs.mutableChain
    val newGenerators = rhsGenerators.filterNot(lhs.siftsFaithful)
    mutableChain.insertGenerators(newGenerators)
    SchreierSims.completeStrongGenerators(mutableChain, KernelBuilder.trivial[G])
    mutableChain.toChain()
  }

  /** Returns the union of the group `lhs` with the group defined by the generators `rhs`. It is assumed
    * that the kernel of the union is the same as `lhs.kernel`.
    *
    * @param lhs           First group.
    * @param rhsGenerators Generators of the second group.
    * @tparam A            Action able to describe the generators of the first AND second group.
    * @return a GrpChain describing the group union.
    */
  def unionGivenKernel[G, A <: PermutationAction[G] with Singleton]
  (lhs: GrpChain[G, A], rhsGenerators: Iterable[G]): GrpChain[G, A] = {
    import lhs.{action, classTag, equ, group}
    val mutableChain = lhs.chain.mutableChain
    val newGenerators = rhsGenerators.filterNot(lhs.contains)
    mutableChain.insertGenerators(newGenerators)
    SchreierSims.completeStrongGenerators(mutableChain, KernelBuilder.fromChain(lhs.kernel))
    val newChain = mutableChain.toChain()
    val generatorsOpt = if (newChain.strongGeneratingSet.size >= lhs.generators.size + newGenerators.size)
      Opt(lhs.generators ++ newGenerators) else Opt.empty[Seq[G]]
    new GrpChainExplicit(mutableChain.toChain(), generatorsOpt, lhs.kernel)
  }

  /** Returns the intersection of two groups with the same action and kernel.
    *
    * @param lhs First group
    * @param rhs Coset chain of the second group, whose kernel is given by lhs.kernel.
    * @return the intersection
    */
  def intersect[G, A <: PermutationAction[G] with Singleton](lhs: GrpChain[G, A], rhs: Chain[G, A])
  (implicit baseChange: BaseChange): GrpChain[G, A] = {
    import lhs.{action, classTag, equ, group}
    subgroupFor(lhs, Intersection[G, A](rhs, lhs.kernel))
  }

  /** Returns the subgroup of `grp` that obeys the subgroup definition `definition`.
    *
    * Warning: the subgroup definition has to pass all elements of the same coset if the
    * action is not faithful.
    */
  def subgroupFor[G, A <: PermutationAction[G] with Singleton](grp: GrpChain[G, A], definition: SubgroupDefinition[G, A])
    (implicit baseChange: BaseChange): GrpChain[G, A] = {
    import grp.{action, classTag, equ, group}
    val guidedChain = grp match {
      case lhs: GrpChainConjugated[G, A] =>
        import lhs.{g, gInv, originalChain}
        val mut = originalChain.mutableChain
        mut.conjugate(g, gInv)
        definition.baseGuideOpt match {
          case Opt(baseGuide) => baseChange.changeBase(mut, grp.kernel, baseGuide)
          case _ =>
        }
        mut.toChain()
      case _ =>
        val chain = grp.chain
        definition.baseGuideOpt match {
          case Opt(baseGuide) if !baseGuide.isSatisfiedBy(chain) =>
            val mut = chain.mutableChain
            baseChange.changeBase(mut, grp.kernel, baseGuide)
            mut.toChain()
          case _ => chain
        }
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain, grp.kernel).toChain()
    new GrpChainExplicit[G, A](subChain, Opt.empty[Seq[G]], grp.kernel)
  }

  /** Returns the subgroup of `grp` that fixes the partition `partition`. */
  def fixingPartition[G, A <: PermutationAction[G] with Singleton](grp: GrpChain[G, A], partition: Partition)
    (implicit baseChange: BaseChange): GrpChain[G, A] = {
    import grp.{action, group}
    subgroupFor(grp, OrderedPartitionStabilizer[G, A](partition))
  }

  def someStabilizerTransversal[G, F <: PermutationAction[G] with Singleton](grp: GrpChain[G, F]): Opt[(GrpChain[G, F], Transversal[G, F])] = {
    import grp.{action, classTag, equ, group}
    grp match {
      case conj: GrpChainConjugated[G, F] => conj.originalChain match {
        case node: Node[G, F] =>
          val nextGrp = new GrpChainConjugated(node.next, conj.g, conj.gInv, Opt.empty[Seq[G]], grp.kernel)
          val trv = ConjugatedTransversal(node, conj.g, conj.gInv)
          Opt((nextGrp, trv))
        case _ => Opt.empty[(GrpChain[G, F], Transversal[G, F])]
      }
      case _ => grp.chain match {
        case node: Node[G, F] => Opt((new GrpChainExplicit(node.next, Opt.empty[Seq[G]], grp.kernel), node))
        case _ => Opt.empty[(GrpChain[G, F], Transversal[G, F])]
      }
    }
  }

  def stabilizerTransversal[G, F <: PermutationAction[G] with Singleton](grp: GrpChain[G, F], b: Int)
    (implicit baseSwap: BaseSwap): (GrpChain[G, F], Transversal[G, F]) = {
    // duplicated code with stabilizer below
    import grp.{action, classTag, equ, group}
    grp match {
      case conj: GrpChainConjugated[G, F] =>
        import conj.{g, gInv, originalChain}
        originalChain match {
          case node: Node[G, F] =>
            val a = b <|+| gInv
            node.findConjugateElement(a) match {
              case Opt(h) =>
                val hInv = h.inverse
                val c = a <|+| hInv
                val (nextOriginalChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
                val newG = h |+| g
                if (newG.isId) {
                  val nextGrp = new GrpChainExplicit[G, F](nextOriginalChain, Opt.empty[Seq[G]], grp.kernel)
                  (nextGrp, originalTrv)
                } else {
                  val newGInv = gInv |+| hInv
                  val nextGrp = new GrpChainConjugated[G, F](nextOriginalChain, newG, newGInv, Opt.empty[Seq[G]], grp.kernel)
                  val trv = ConjugatedTransversal[G, F](originalTrv, newG, newGInv)
                  (nextGrp, trv)
                }
              case _ =>
                if (node.isFixed(a))
                  (conj, Transversal.empty[G, F](b))
                else {
                  val mutableChain = originalChain.mutableChain
                  mutableChain.conjugate(g, gInv)
                  mutableChain.changeBasePointAfter(mutableChain.start, b)
                  val (nextChain, trv) = mutableChain.toChain.detach(b)
                  val nextGrp = new GrpChainExplicit[G, F](nextChain, Opt.empty[Seq[G]], grp.kernel)
                  (nextGrp, trv)
                }
            }
          case term: Term[G, F] => (conj, Transversal.empty[G, F](b))
        }
      case _ => grp.chain match {
        case term: Term[G, F] => (grp, Transversal.empty[G, F](b))
        case node: Node[G, F] => node.findConjugateElement(b) match {
          case Opt(g) =>
            if (g.isId) {
              val (nextChain, trv) = node.withFirstBasePoint(b).detach(b)
              (new GrpChainExplicit[G, F](nextChain, Opt.empty[Seq[G]], grp.kernel), trv)
            } else {
              val gInv = g.inverse
              val c = b <|+| gInv
              val (nextChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
              val nextGrp = new GrpChainConjugated[G, F](nextChain, g, gInv, Opt.empty[Seq[G]], grp.kernel)
              val trv = ConjugatedTransversal[G, F](originalTrv, g, gInv)
              (nextGrp, trv)
            }
          case _ =>
            if (node.isFixed(b))
              (grp, Transversal.empty[G, F](b))
            else {
              val (nextChain, trv) = node.withFirstBasePoint(b).detach(b)
              (new GrpChainExplicit[G, F](nextChain, Opt.empty[Seq[G]], grp.kernel), trv)
            }
        }
      }
    }
  }

  def stabilizer[G, F <: PermutationAction[G] with Singleton](grp: GrpChain[G, F], b: Int)
  (implicit baseSwap: BaseSwap): GrpChain[G, F] = {
    // duplicated code with stabilizerTransversal above
    import grp.{action, classTag, equ, group}
    grp match {
      case conj: GrpChainConjugated[G, F] =>
        import conj.{g, gInv, originalChain}
        originalChain match {
          case node: Node[G, F] =>
            val a = b <|+| gInv
            node.findConjugateElement(a) match {
              case Opt(h) =>
                val hInv = h.inverse
                val c = a <|+| hInv
                val (nextOriginalChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
                val newG = h |+| g
                if (newG.isId)
                  new GrpChainExplicit[G, F](nextOriginalChain, Opt.empty[Seq[G]], grp.kernel)
                else {
                  val newGInv = gInv |+| hInv
                  new GrpChainConjugated[G, F](nextOriginalChain, newG, newGInv, Opt.empty[Seq[G]], grp.kernel)
                }
              case _ =>
                if (node.isFixed(a)) conj
                else {
                  val mutableChain = originalChain.mutableChain
                  mutableChain.conjugate(g, gInv)
                  mutableChain.changeBasePointAfter(mutableChain.start, b)
                  val (nextChain, _) = mutableChain.toChain.detach(b)
                  new GrpChainExplicit[G, F](nextChain, Opt.empty[Seq[G]], grp.kernel)
                }
            }
          case term: Term[G, F] => conj
        }
      case _ => grp.chain match {
        case term: Term[G, F] => grp
        case node: Node[G, F] => node.findConjugateElement(b) match {
          case Opt(g) =>
            if (g.isId) {
              val (nextChain, _) = node.withFirstBasePoint(b).detach(b)
              new GrpChainExplicit[G, F](nextChain, Opt.empty[Seq[G]], grp.kernel)
            } else {
              val gInv = g.inverse
              val c = b <|+| gInv
              val (nextChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
              new GrpChainConjugated[G, F](nextChain, g, gInv, Opt.empty[Seq[G]], grp.kernel)
            }
          case _ =>
            if (node.isFixed(b)) grp
            else {
              val (nextChain, _) = node.withFirstBasePoint(b).detach(b)
              new GrpChainExplicit[G, F](nextChain, Opt.empty[Seq[G]], grp.kernel)
            }
        }
      }
    }
  }

  def pointwiseStabilizer[G, A <: PermutationAction[G] with Singleton](grp: GrpChain[G, A], set: Set[Int])
                                                                      (implicit baseChange: BaseChange): GrpChain[G, A] = {
    import grp.{action, classTag, equ, group}
    val guide = PointwiseStabilizer.baseGuide(set)
    val guidedChain = grp match {
      case conj: GrpChainConjugated[G, A] =>
        import conj.{g, gInv}
        val mut = conj.originalChain.mutableChain
        mut.conjugate(g, gInv)
        baseChange.changeBase(mut, grp.kernel, guide)
        mut.toChain()
      case _ =>
        val mut = grp.chain.mutableChain
        baseChange.changeBase(mut, grp.kernel, guide)
        mut.toChain()
    }
    new GrpChainExplicit[G, A](PointwiseStabilizer.recurse(guidedChain, set), Opt.empty[Seq[G]], grp.kernel)
  }

  def leftCosetsByFaithfulAction[G, F <: PermutationAction[G] with Singleton](grp0: GrpChain[G, F], subgrp0: Grp[G], chainSubgrp0: GrpChain[G, F])
    (implicit baseSwap: BaseSwap): LeftCosets[G, subgrp0.type] = {
    require(grp0.hasSubgroup(subgrp0)) // TODO: add NC variant
    new LeftCosetsImpl[G, subgrp0.type] {

      val grp = grp0

      val subgrp: subgrp0.type = subgrp0

      val chainSubgrp = chainSubgrp0

      def iterator: Iterator[LeftCoset[G, subgrp0.type]] = {
        import grp.{action, group}
        //val myBase = grp.chain.base
        //val bo = BaseOrder[G, F](myBase) TODO: unused?
        //val bordering = bo.toOrdering
        val orbit = MutableOrbit.empty
        import spire.std.int.IntAlgebra
        def rec(g: G, chain: Chain[G, F], subSubgrp: GrpChain[G, F]): Iterator[LeftCoset[G, subgrp0.type]] = chain match {
          case node: Node[G, F] =>
            for {
              b <- node.orbit.iterator
              bg = b <|+| g if orbits.Points.isSmallestInOrbit(bg, subSubgrp.generators, Opt(orbit))
              nextSubSubgrp = stabilizer[G, F](subSubgrp, bg)
              nextG = node.u(b) |+| g
              element <- rec(nextG, node.next, nextSubSubgrp)
            } yield element
          case _: Term[G, F] =>
            Iterator(new LeftCoset(g, subgrp))
        }
        rec(Group[G].id, grp.chain, chainSubgrp)
      }

    }

  }

  /** Lexicographic enumeration of group elements by their images for the given action (which must be faithful). */
  final class LexElements[G, F <: PermutationAction[G] with Singleton]
  (grp: GrpChain[G, F])(implicit baseChange: BaseChange) extends BigIndexedSeq[G] {

    import grp.{action, classTag, equ, group}

    val lexChain: Chain[G, F] = if (grp.chain.hasLexicographicBase) grp.chain else {
      val n = action.largestMovedPoint(grp.generators).getOrElse(0) + 1
      BuildChain.withBase[G, F](grp.chain, BaseGuideLex(n), Term[G, F])
    }

    def length = lexChain.order

    def contains(g: G) = lexChain.siftsFaithful(g)

    def apply(idx: SafeLong): G = {
      @tailrec def rec(current: Chain[G, F], curIdx: SafeLong, curOrder: SafeLong, curG: G): G = current match {
        case node: Node[G, F] =>
          val sortedOrbit = node.orbit.toSeq.sortBy(k => k <|+| curG)
          val nextOrder = curOrder / node.orbitSize
          val nextIdx = curIdx % nextOrder
          val orbitIndex = ((curIdx - nextIdx) / nextOrder).toInt
          val nextG = node.u(sortedOrbit(orbitIndex)) |+| curG
          rec(node.next, nextIdx, nextOrder, nextG)
        case _: Term[G, F] =>
          assert(curIdx == 0)
          curG
      }
      rec(lexChain, idx, lexChain.order, Group[G].id)
    }

    def iterator: Iterator[G] = {
      def rec(current: Chain[G, F], curG: G): Iterator[G] = current match {
        case node: Node[G, F] =>
          val sortedOrbit = node.orbit.toSeq.sortBy(k => k <|+| curG)
          for {
            b <- sortedOrbit.iterator
            nextG = node.u(b) |+| curG
            rest <- rec(node.next, nextG)
          } yield rest
        case _: Term[G, F] => Iterator(curG)
      }
      rec(lexChain, Group[G].id)
    }

  }

}
