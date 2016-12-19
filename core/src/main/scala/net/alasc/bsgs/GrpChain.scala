package net.alasc.bsgs

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Group, Order}
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, PermutationAction}
import net.alasc.bsgs
import net.alasc.domains.Partition
import net.alasc.finite._
import net.alasc.perms.orbits
import net.alasc.perms.{FaithfulPermRep, MutableOrbit}

/** Group described a BSGS chain of elements of type G using the permutation action F.
  *
  * If the action is faithful for the current group, then the group is fully described by the chain and kernelOpt is empty.
  * If the action is not faithful, then kernel contains the nontrivial kernel normal subgroup K,
  * and the chain describes the quotient Group/K.
  *
  */
abstract class GrpChain[G, A <: PermutationAction[G] with Singleton] extends Grp[G] { lhs =>

  implicit val action: A

  implicit def classTag: ClassTag[G]

  def chain: Chain[G, A]

  def chainOpt: Opt[Chain[G, A]]

  def kernel: Chain.Generic[G]

}

object GrpChain {

  type Generic[G] = GrpChain[G, _ <: PermutationAction[G] with Singleton]

  @inline final def extractGrpChain[G](grp: Grp[G], action: PermutationAction[G]): Opt[GrpChain[G, action.type]] =
    grp match {
      case gc: GrpChain[G, _] if gc.action eq action => Opt(gc.asInstanceOf[GrpChain[G, action.type]])
      case _ => Opt.empty[GrpChain[G, action.type]]
    }

  @inline final def commonAction[G](lhs: Grp[G], rhs: Grp[G]): Opt[PermutationAction[G]] =
    lhs match {
      case lhs1: GrpChain[G, _] => rhs match {
        case rhs1: GrpChain[G, _] if lhs1.action eq rhs1.action => Opt(lhs1.action)
        case _ => Opt.empty[PermutationAction[G]]
      }
      case _ => Opt.empty[PermutationAction[G]]
    }

  @inline final def forceAction[G](lhs: Grp[G], action: PermutationAction[G]): GrpChain[G, action.type] = lhs.asInstanceOf[GrpChain[G, action.type]]

  /*
  /** Returns the union of the group `lhs` with the group defined by the generators `rhs`.
    * It is required that the permutation action `F` can describe the `rhs` generators.
    *
    * The kernel is also computed using Schreier-Sims algorithm.
    */
  def unionComputeKernel[G, F <: PermutationAction[G] with Singleton]
  (lhs: GrpChain[G, F], rhs: Iterable[G], kernel: KernelBuilder[G])(implicit schreierSims: SchreierSims): GrpChain[G, F] = {
    import lhs.{action, classTag, equ, group}
    val mutableChain = lhs.chain.mutableChain
    val newGenerators = rhs.filterNot(mutableChain.start.next.sifts)
    mutableChain.insertGenerators(newGenerators)
    schreierSims.completeStrongGenerators(mutableChain, kernel)
    val newChain = mutableChain.toChain()
    val generatorsOpt = if (newChain.strongGeneratingSet.size >= lhs.generators.size + newGenerators.size)
        Opt(lhs.generators ++ newGenerators) else Opt.empty[IndexedSeq[G]]
    new GrpChainExplicit(mutableChain.toChain(), generatorsOpt, kernel.result(completeChain = true))
  }

  /** Returns the union of the group `lhs` with the group defined by the generators `rhs`.
    * It is required that the permutation action `F` can describe the `rhs` generators.
    *
    * The kernel is provided in argument.
    */
  def unionGivenKernel[G, F <: PermutationAction[G] with Singleton]
  (lhs: GrpChain[G, F], rhs: Iterable[G], kernelOpt: Opt[Grp[G]])(implicit schreierSims: SchreierSims): GrpChain[G, F] = {
    import lhs.{action, classTag, equ, group}
    val mutableChain = lhs.chain.mutableChain
    val newGenerators = rhs.filterNot(mutableChain.start.next.sifts)
    mutableChain.insertGenerators(newGenerators)
    schreierSims.completeStrongGenerators(mutableChain, KernelBuilder.BlackHole[G, F])
    val newChain = mutableChain.toChain()
    val generatorsOpt = if (newChain.strongGeneratingSet.size >= lhs.generators.size + newGenerators.size)
      Opt(lhs.generators ++ newGenerators) else Opt.empty[IndexedSeq[G]]
    new GrpChainExplicit(mutableChain.toChain(), generatorsOpt, kernelOpt)
  }

  def intersect[G, F <: PermutationAction[G] with Singleton](lhs: GrpChain[G, F], rhs: GrpChain[G, F])
                                                            (implicit baseChange: BaseChange, schreierSims: SchreierSims): GrpChain[G, F] = {
    import lhs.{action, classTag, equ, group}
    subgroupFor(lhs, Intersection[G, F](rhs.chain))
  }
*/
  /*
  /** Returns the subgroup of `grp` that obeys the subgroup definition `definition`. */
  def subgroupFor[G, F <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, F], definition: SubgroupDefinition[G, F])
    (implicit baseChange: BaseChange, schreierSims: SchreierSims): GrpChain[G, F] = {
    import grp.{action, classTag, equ, group}
    val guidedChain = grp match {
      case lhs: GrpChainConjugated[G, F] =>
        import lhs.{g, gInv, originalChain}
        val mut = originalChain.mutableChain
        mut.conjugate(g, gInv)
        definition.baseGuideOpt match {
          case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
          case _ =>
        }
        mut.toChain()
      case _ =>
        val chain = grp.chain
        definition.baseGuideOpt match {
          case Opt(baseGuide) if !baseGuide.isSatisfiedBy(chain) =>
            val mut = chain.mutableChain
            baseChange.changeBase(mut, baseGuide)
            mut.toChain()
          case _ => chain
        }
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
    new GrpChainExplicit[G, F](subChain, Opt.empty[IndexedSeq[G]], grp.kernelOpt)
  }


  /** Returns the subgroup of `grp` that fixes the partition `partition`. */
  def fixingPartition[G, F <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, F], partition: Partition)
    (implicit baseChange: BaseChange, schreierSims: SchreierSims): GrpChain[G, F] = {
    import grp.{action, group}
    subgroupFor(grp, net.alasc.bsgs.FixingPartition[G, F](partition))
  }

  def someStabilizerTransversal[G, F <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, F]): Opt[(GrpChain[G, F], Transversal[G, F])] = {
    import grp.{action, classTag, equ, group}
    grp match {
      case conj: GrpChainConjugated[G, F] => conj.originalChain match {
        case node: Node[G, F] =>
          val nextGrp = new GrpChainConjugated(node.next, conj.g, conj.gInv, Opt.empty[IndexedSeq[G]])
          val trv = ConjugatedTransversal(node, conj.g, conj.gInv)
          Opt((nextGrp, trv))
        case _ => Opt.empty[(GrpChain[G, F], Transversal[G, F])]
      }
      case _ => grp.chain match {
        case node: Node[G, F] => Opt((new GrpChainExplicit(node.next, Opt.empty[IndexedSeq[G]]), node))
        case _ => Opt.empty[(GrpChain[G, F], Transversal[G, F])]
      }
    }
  }

  def stabilizerTransversal[G, F <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, F], b: Int)
    (implicit baseChange: BaseChange, baseSwap: BaseSwap, schreierSims: SchreierSims): (GrpChain[G, F], Transversal[G, F]) = {
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
                  val nextGrp = new GrpChainExplicit[G, F](nextOriginalChain, Opt.empty[IndexedSeq[G]])
                  (nextGrp, originalTrv)
                } else {
                  val newGInv = gInv |+| hInv
                  val nextGrp = new GrpChainConjugated[G, F](nextOriginalChain, newG, newGInv,
                    Opt.empty[IndexedSeq[G]])
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
                  val nextGrp = new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]])
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
              (new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]]), trv)
            } else {
              val gInv = g.inverse
              val c = b <|+| gInv
              val (nextChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
              val nextGrp = new GrpChainConjugated[G, F](nextChain, g, gInv, Opt.empty[IndexedSeq[G]])
              val trv = ConjugatedTransversal[G, F](originalTrv, g, gInv)
              (nextGrp, trv)
            }
          case _ =>
            if (node.isFixed(b))
              (grp, Transversal.empty[G, F](b))
            else {
              val (nextChain, trv) = node.withFirstBasePoint(b).detach(b)
              (new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]]), trv)
            }
        }
      }
    }
  }

  def stabilizer[G, F <: PermutationAction[G] with Singleton]
  (grp: GrpChain[G, F], b: Int)
  (implicit baseChange: BaseChange, baseSwap: BaseSwap, schreierSims: SchreierSims): GrpChain[G, F] = {
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
                  new GrpChainExplicit[G, F](nextOriginalChain, Opt.empty[IndexedSeq[G]])
                else {
                  val newGInv = gInv |+| hInv
                  new GrpChainConjugated[G, F](nextOriginalChain, newG, newGInv, Opt.empty[IndexedSeq[G]])
                }
              case _ =>
                if (node.isFixed(a)) conj
                else {
                  val mutableChain = originalChain.mutableChain
                  mutableChain.conjugate(g, gInv)
                  mutableChain.changeBasePointAfter(mutableChain.start, b)
                  val (nextChain, _) = mutableChain.toChain.detach(b)
                  new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]])
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
              new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]])
            } else {
              val gInv = g.inverse
              val c = b <|+| gInv
              val (nextChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
              new GrpChainConjugated[G, F](nextChain, g, gInv, Opt.empty[IndexedSeq[G]])
            }
          case _ =>
            if (node.isFixed(b)) grp
            else {
              val (nextChain, _) = node.withFirstBasePoint(b).detach(b)
              new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]])
            }
        }
      }
    }
  }

  def pointwiseStabilizer[G, F <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, F], set: Set[Int])(implicit baseChange: BaseChange): GrpChain[G, F] = {
    import grp.{action, classTag, equ, group}
    val guide = PointwiseStabilizer.baseGuide(set)
    val guidedChain = grp match {
      case conj: GrpChainConjugated[G, F] =>
        import conj.{g, gInv}
        val mut = conj.originalChain.mutableChain
        mut.conjugate(g, gInv)
        baseChange.changeBase(mut, guide)
        mut.toChain()
      case _ =>
        val mut = grp.chain.mutableChain
        baseChange.changeBase(mut, guide)
        mut.toChain()
    }
    new GrpChainExplicit[G, F](PointwiseStabilizer.recurse(guidedChain, set), Opt.empty[IndexedSeq[G]])
  }

  def leftCosetsBy[G, F <: PermutationAction[G] with Singleton]
    (grp0: GrpChain[G, F], subgrp0: Grp[G], chainSubgrp0: GrpChain[G, F])
    (implicit baseChange: BaseChange, baseSwap: BaseSwap, schreierSims: SchreierSims): LeftCosets[G, subgrp0.type] = {
    import grp0.{action, group}
    require(grp0.hasSubgroup(subgrp0)) // TODO: add NC variant
    new LeftCosetsImpl[G, subgrp0.type] {

      val grp = grp0

      val subgrp: subgrp0.type = subgrp0

      val chainSubgrp = chainSubgrp0

      def iterator: Iterator[LeftCoset[G, subgrp0.type]] = {
        import grp.{action, classTag, group}
        val myBase = grp.chain.base
        val bo = BaseOrder[G, F](myBase)
        val bordering = Order.ordering(bo)
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
        rec(Group[G].id, grp.chain, chainSubgrp0)
      }

    }

  }

  final class LexElements[G, F <: PermutationAction[G] with Singleton]
    (grp: GrpChain[G, F])(implicit baseChange: BaseChange, schreierSims: SchreierSims) extends BigIndexedSeq[G] {

    import grp.{action, classTag, equ, group}

    val lexChain: Chain[G, F] = if (grp.chain.hasLexicographicBase) grp.chain else {
      val n = action.largestMovedPoint(grp.generators).getOrElse(0) + 1
      BuildChain.fromChain(grp.chain, Opt(BaseGuideLex(n)))
    }

    def length = lexChain.order

    def contains(g: G) = lexChain.sifts(g)

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

  }*/

}