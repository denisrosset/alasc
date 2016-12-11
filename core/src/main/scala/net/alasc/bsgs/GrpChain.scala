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
import net.alasc.finite.{Grp, LeftCoset, LeftCosets, LeftCosetsImpl}
import net.alasc.perms.orbits
import net.alasc.perms.{FaithfulPermRep, MutableOrbit}

/** Group described a BSGS chain of elements `G` using the faithful permutation action `F` */
abstract class GrpChain[G, F <: PermutationAction[G] with Singleton] extends Grp[G] { lhs =>

  implicit val action: F

  implicit def classTag: ClassTag[G]

  def chain: bsgs.Chain[G, F]

  def chainOpt: Opt[bsgs.Chain[G, F]]

  def repOpt: Opt[FaithfulPermRep[G, _]]

}

object GrpChain {

  trait In[G, F <: PermutationAction[G] with Singleton] {
    def unapply(grp: Grp[G]): Option[GrpChain[G, F]]
  }

  /** Matches a `GrpChain[G, F]` with the requested action. */
  object In {
    def apply[G, PA[X] <: PermutationAction[X], F <: PermutationAction[G] with Singleton](action: F with PA[G]): In[G, F] =
      new In[G, F] {
        def unapply(grp: Grp[G]): Option[GrpChain[G, F]] = grp match {
          case gc: GrpChain[G, _] if gc.action eq action => Some(gc.asInstanceOf[GrpChain[G, F]])
          case _ => None // Opt.empty[GrpChain[G, F]]
        }
      }
  }

  trait Extracted[G] { self =>
    def isEmpty = action eq null
    type Action <: PermutationAction[G] with Singleton
    implicit val action: Action
    val grp: GrpChain[G, Action]
    def get = self
  }

  object ExtractedEmpty extends Extracted[Nothing] {
    val action = null
    val grp = null
    type Action = Null
  }

  object AndAction {
    def unapply[G](lhs: Grp[G]): Extracted[G] = lhs match {
      case gc: GrpChain[G, _] =>
        new Extracted[G] {
          type Action = gc.action.type
          val action: Action = gc.action
          val grp = gc.asInstanceOf[GrpChain[G, Action]]
        }
      case _ => ExtractedEmpty.asInstanceOf[Extracted[G]]
    }
  }

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
        BuildChain.fromChain[G, F, F](grp.chain, definition.baseGuideOpt)
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
    new GrpChainExplicit[G, F](subChain, Opt.empty[IndexedSeq[G]], grp.repOpt)
  }

  /** Returns the union of the group `lhs` with the group defined by the generators `rhs`.
    * It is required that the permutation action `F` can describe the `rhs` generators.
    */
  def union[G, F <: PermutationAction[G] with Singleton]
    (lhs: GrpChain[G, F], rhs: Iterable[G]): GrpChain[G, F] = {
    import lhs.{action, classTag, equ, group}
    val mutableChain = lhs.chain.mutableChain
    val newGenerators = rhs.filterNot(mutableChain.start.next.sifts)
    mutableChain.insertGenerators(newGenerators)
    mutableChain.completeStrongGenerators()
    val newChain = mutableChain.toChain()
    val generatorsOpt =
      if (newChain.strongGeneratingSet.size >= lhs.generators.size + newGenerators.size)
        Opt(lhs.generators ++ newGenerators)
      else
        Opt.empty[IndexedSeq[G]]
    new GrpChainExplicit(mutableChain.toChain(), generatorsOpt, lhs.repOpt)
  }

  def intersect[G, F <: PermutationAction[G] with Singleton](lhs: GrpChain[G, F], rhs: GrpChain[G, F])
    (implicit baseChange: BaseChange, schreierSims: SchreierSims): GrpChain[G, F] = {
    import lhs.{action, classTag, equ, group}
    subgroupFor(lhs, Intersection[G, F](rhs.chain))
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
          val nextGrp = new GrpChainConjugated(node.next, conj.g, conj.gInv, Opt.empty[IndexedSeq[G]], grp.repOpt)
          val trv = ConjugatedTransversal(node, conj.g, conj.gInv)
          Opt((nextGrp, trv))
        case _ => Opt.empty[(GrpChain[G, F], Transversal[G, F])]
      }
      case _ => grp.chain match {
        case node: Node[G, F] => Opt((new GrpChainExplicit(node.next, Opt.empty[IndexedSeq[G]], grp.repOpt), node))
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
                  val nextGrp = new GrpChainExplicit[G, F](nextOriginalChain, Opt.empty[IndexedSeq[G]], grp.repOpt)
                  (nextGrp, originalTrv)
                } else {
                  val newGInv = gInv |+| hInv
                  val nextGrp = new GrpChainConjugated[G, F](nextOriginalChain, newG, newGInv,
                    Opt.empty[IndexedSeq[G]], grp.repOpt)
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
                  val nextGrp = new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]], grp.repOpt)
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
              (new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]], grp.repOpt), trv)
            } else {
              val gInv = g.inverse
              val c = b <|+| gInv
              val (nextChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
              val nextGrp = new GrpChainConjugated[G, F](nextChain, g, gInv, Opt.empty[IndexedSeq[G]], grp.repOpt)
              val trv = ConjugatedTransversal[G, F](originalTrv, g, gInv)
              (nextGrp, trv)
            }
          case _ =>
            if (node.isFixed(b))
              (grp, Transversal.empty[G, F](b))
            else {
              val (nextChain, trv) = node.withFirstBasePoint(b).detach(b)
              (new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]], grp.repOpt), trv)
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
                  new GrpChainExplicit[G, F](nextOriginalChain, Opt.empty[IndexedSeq[G]], grp.repOpt)
                else {
                  val newGInv = gInv |+| hInv
                  new GrpChainConjugated[G, F](nextOriginalChain, newG, newGInv, Opt.empty[IndexedSeq[G]], grp.repOpt)
                }
              case _ =>
                if (node.isFixed(a)) conj
                else {
                  val mutableChain = originalChain.mutableChain
                  mutableChain.conjugate(g, gInv)
                  mutableChain.changeBasePointAfter(mutableChain.start, b)
                  val (nextChain, _) = mutableChain.toChain.detach(b)
                  new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]], grp.repOpt)
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
              new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]], grp.repOpt)
            } else {
              val gInv = g.inverse
              val c = b <|+| gInv
              val (nextChain, originalTrv) = node.withFirstBasePoint(c).detach(c)
              new GrpChainConjugated[G, F](nextChain, g, gInv, Opt.empty[IndexedSeq[G]], grp.repOpt)
            }
          case _ =>
            if (node.isFixed(b)) grp
            else {
              val (nextChain, _) = node.withFirstBasePoint(b).detach(b)
              new GrpChainExplicit[G, F](nextChain, Opt.empty[IndexedSeq[G]], grp.repOpt)
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
    new GrpChainExplicit[G, F](PointwiseStabilizer.recurse(guidedChain, set), Opt.empty[IndexedSeq[G]], grp.repOpt)
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

  }

}