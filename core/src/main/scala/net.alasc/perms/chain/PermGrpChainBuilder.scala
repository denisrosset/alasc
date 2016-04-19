package net.alasc.perms
package chain

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.{Group, Order}
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.{BigIndexedSeq, Permutation}
import net.alasc.domains.Partition
import net.alasc.finite.{Grp, LeftCoset, LeftCosets, RightCosets}
import net.alasc.prep.bsgs._

class PermGrpChainBuilder[G](implicit
                             val baseChange: BaseChange,
                             val baseSwap: BaseSwap,
                             val classTag: ClassTag[G],
                             val permutation: Permutation[G],
                             val schreierSims: SchreierSims
                            ) extends PermGrpBuilder[G] {

  type GG = PermGrpChain[G]

  // builder methods

  def trivial = new PermGrpChainExplicit[G](Term[G], generatorsOpt = Opt(Iterable.empty[G]))

  def fromGenerators(generators: Iterable[G]): GG =
    fromGenerators(generators, Opt.empty[BaseGuide])

  def fromGenerators(generators: Iterable[G], baseGuideOpt: Opt[BaseGuide]): GG = {
    val chain = BuildChain.fromGenerators(generators, permutation, baseGuideOpt)
    new PermGrpChainExplicit(chain, generatorsOpt = Opt(generators))
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt): GG =
    fromGeneratorsAndOrder(generators, order, Opt.empty[BaseGuide])

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt, baseGuideOpt: Opt[BaseGuide]): GG = {
    val chain = BuildChain.fromGeneratorsAndOrder(generators, order, permutation, baseGuideOpt)
    new PermGrpChainExplicit(chain, generatorsOpt = Opt(generators))
  }

  def fromGrp(grp: Grp[G]): GG = fromGrp(grp, Opt.empty[BaseGuide])

  def fromGrp(grp: Grp[G], baseGuideOpt: Opt[BaseGuide]): GG = grp match {
    case cg: PermGrpChain[G] => baseGuideOpt match {
      case Opt(baseGuide) => cg.chainOpt match {
        case Opt(chain) if baseGuide.isSatisfiedBy(chain) => cg
        case _ => fromGeneratorsAndOrder(grp.generators, grp.order, baseGuideOpt)
      }
      case _ => cg
    }
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order, baseGuideOpt)
  }

  // with a single Grp[G] argument

  def someStabilizer(grp: Grp[G]): Opt[GG] = someStabilizerTransversal(grp) match {
    case Opt((subgrp, _)) => Opt(subgrp)
    case _ => Opt.empty[GG]
  }

  def someStabilizerTransversal(grp: Grp[G]): Opt[(GG, Transversal[G])] = grp match {
    case conj: PermGrpChainConjugated[G] => conj.originalChain match {
      case node: Node[G] =>
        val nextGrp = new PermGrpChainConjugated(node.next, conj.g, conj.gInv)
        val trv = ConjugatedTransversal(node, conj.g, conj.gInv)
        Opt((nextGrp, trv))
      case _ => Opt.empty[(GG, Transversal[G])]
    }
    case cg: PermGrpChain[G] => cg.chain match {
      case node: Node[G] => Opt((new PermGrpChainExplicit(node.next), node))
      case _ => Opt.empty[(GG, Transversal[G])]
    }
    case _ => someStabilizerTransversal(fromGrp(grp): PermGrpChain[G])
  }

  def stabilizer(grp: Grp[G], b: Int): GG = stabilizerTransversal(grp, b)._1

  def stabilizerTransversal(grp: Grp[G], b: Int): (GG, Transversal[G]) = grp match {
    case conj: PermGrpChainConjugated[G] =>
      import conj.{g, gInv, originalChain}
      originalChain match {
        case node: Node[G] =>
          val a = b <|+| gInv
          if (node.inOrbit(a)) {
            val u = node.u(a)
            val uInv = node.uInv(a)
            val newG = u |+| g
            val newGInv = gInv |+| uInv
            val nextGrp = new PermGrpChainConjugated[G](node.next, newG, newGInv)
            val trv = ConjugatedTransversal(node, newG, newGInv)
            (nextGrp, trv)
          }
          else if (node.isFixed(a))
            (conj, Transversal.empty(b))
          else {
            val newChain = BuildChain.fromChain(originalChain, permutation, Opt(BaseGuideSeq(Seq(a))))
            val (nextOriginalChain, originalTransversal) = newChain.detach(a)
            val nextGrp = new PermGrpChainConjugated[G](nextOriginalChain, g, gInv)
            val trv = ConjugatedTransversal(originalTransversal, g, gInv)
            (nextGrp, trv)
          }
        case term: Term[G] => (conj, Transversal.empty[G](b))
      }
    case cg: PermGrpChain[G] => cg.chain match {
      case term: Term[G] => (cg, Transversal.empty[G](b))
      case node: Node[G] if node.inOrbit(b) =>
        val u = node.u(b)
        val uInv = node.uInv(b)
        val nextGrp = new PermGrpChainConjugated[G](node.next, u, uInv)
        val trv: Transversal[G] = ConjugatedTransversal(node, u, uInv)
        (nextGrp, trv)
      case node: Node[G] if node.isFixed(b) =>
        (cg, Transversal.empty(b))
      case _ =>
        val newChain = BuildChain.fromChain(cg.chain, permutation, Opt(BaseGuideSeq(Seq(b))))
        val (nextChain, trv) = newChain.detach(b)
        val nextGrp = new PermGrpChainExplicit[G](nextChain)
        (nextGrp, trv)
    }
    case _ =>
      val chain = BuildChain.fromGeneratorsAndOrder(grp.generators, grp.order, permutation, Opt(BaseGuideSeq(Seq(b))))
      val (nextChain, trv) = chain.detach(b)
      val nextGrp = new PermGrpChainExplicit[G](nextChain)
      (nextGrp, trv)
  }

  def pointwiseStabilizer(grp: Grp[G], set: Set[Int]): GG = {
    val guide = PointwiseStabilizer.baseGuide(set)
    val guidedChain = grp match {
      case conj: PermGrpChainConjugated[G] =>
        import conj.{g, gInv}
        val mut = conj.originalChain.mutableChain
        mut.conjugate(g, gInv)
        baseChange.changeBase(mut, guide)
        mut.toChain()
      case cg: PermGrpChain[G] =>
        val mut = cg.chain.mutableChain
        baseChange.changeBase(mut, guide)
        mut.toChain()
      case _ =>
        BuildChain.fromGeneratorsAndOrder(grp.generators, grp.order, permutation, Opt(guide))
    }
    new PermGrpChainExplicit[G](PointwiseStabilizer.recurse(guidedChain, set))
  }

  override def conjugatedBy(grp: Grp[G], h: G): PermGrpChain[G] = grp match {
    case conj: PermGrpChainConjugated[G] =>
      new PermGrpChainConjugated[G](conj.originalChain, conj.g |+| h, h.inverse |+| conj.gInv,
        originalGeneratorsOpt = Opt(conj.originalGenerators))
    case cg: PermGrpChain[G] =>
      new PermGrpChainConjugated[G](cg.chain, h, h.inverse,
        originalGeneratorsOpt = Opt(cg.generators))
    case _ =>
      val hInv = h.inverse
      fromGeneratorsAndOrder(grp.generators.map(g => hInv |+| g |+| h), grp.order)
  }

  def intersect(lhs: Grp[G], rhs: Grp[G]): PermGrpChain[G] =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(rhs)
    else {
      val rhsChain = fromGrp(rhs).chain
      subgroupFor(lhs, Intersection(permutation, rhsChain))
    }

  def union(lhs: Grp[G], rhs: Grp[G]): PermGrpChain[G] =
    if (rhs.order > lhs.order) union(rhs, lhs) // ensure that lhs.order is the greatest
    else if (lhs.hasSubgroup(rhs)) fromGrp(lhs)
    else {
      val lhsChain = fromGrp(lhs).chain
      val mutableChain = BuildMutableChain.fromChain(lhsChain, permutation)
      val newGenerators = rhs.generators.filterNot(mutableChain.start.next.sifts)
      mutableChain.insertGenerators(newGenerators)
      mutableChain.completeStrongGenerators()
      val newChain = mutableChain.toChain()
      val generatorsOpt =
        if (newChain.strongGeneratingSet.size >= lhs.generators.size + newGenerators.size)
          Opt(lhs.generators ++ newGenerators)
        else
          Opt.empty[Iterable[G]]
      new PermGrpChainExplicit(mutableChain.toChain(), generatorsOpt)
    }

  def subgroupFor(grp: Grp[G], backtrackTest: (Int, Int) => Boolean, predicate: G => Boolean): PermGrpChain[G] =
    subgroupFor(grp, SubgroupDefinition(permutation, backtrackTest, predicate))

  def subgroupFor(grp: Grp[G], definition: SubgroupDefinition[G]): PermGrpChain[G] = {
    val guidedChain = grp match {
      case lhs: PermGrpChainConjugated[G] =>
        import lhs.{g, gInv, originalChain}
        val mut = originalChain.mutableChain
        mut.conjugate(g, gInv)
        definition.baseGuideOpt match {
          case Opt(baseGuide) => baseChange.changeBase(mut, baseGuide)
          case _ =>
        }
        mut.toChain()
      case lhs: PermGrpChain[G] =>
        BuildChain.fromChain(lhs.chain, permutation, definition.baseGuideOpt)
      case _ =>
        BuildChain.fromGeneratorsAndOrder(grp.generators, grp.order, permutation, definition.baseGuideOpt)
    }
    val subChain = SubgroupSearch.subgroupSearch(definition, guidedChain).toChain()
    new PermGrpChainExplicit[G](subChain)
  }

  def fixingPartition(grp: Grp[G], partition: Partition): GG =
    subgroupFor(grp, FixingPartition(permutation, partition))

  def setwiseStabilizer(grp: Grp[G], set: Set[Int]): GG =
    subgroupFor(grp, SetwiseStabilizer(permutation, set))

  def leftCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): LeftCosets[G] =
    new LeftCosets[G] {

      val grp: GG = grp0 match {
        case cg: PermGrpChain[G] => cg
        case _ => fromGrp(grp0)
      }
      val subgrp: Grp[G] = subgrp0
      require(grp.hasSubgroup(subgrp))

      def iterator: Iterator[LeftCoset[G]] = {
        val myBase = base(grp)
        val bo = BaseOrder(permutation, base(grp))
        val bordering = Order.ordering(bo)
        def rec(g: G, chain: Chain[G], subSubgrp: Grp[G]): Iterator[LeftCoset[G]] = chain match {
          case node: Node[G] =>
            for {
              b <- node.orbit.iterator
              bg = b <|+| g
              (nextSubSubGrp, trv) = stabilizerTransversal(subSubgrp, bg) if trv.orbit.min(bordering) == bg
              nextG = node.u(b) |+| g
              element <- rec(nextG, node.next, nextSubSubGrp)
            } yield element
          case _: Term[G] =>
            assert(pointwiseStabilizer(subgrp, myBase.map(_ <|+| g).toSet).isTrivial)
            assert(subSubgrp.order == 1)
            Iterator(new LeftCoset(g, subgrp))
        }
        rec(Group[G].id, grp.chain, subgrp)
      }

    }

  def rightCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): RightCosets[G] =
    new RightCosets[G] {
      val grp = grp0
      val subgrp = subgrp0
      def iterator = leftCosetsBy(grp0, subgrp0).iterator.map(_.inverse)
    }

  // enumeration of subgroup elements
  def lexElements(grp: Grp[G]): BigIndexedSeq[G] = {
    val cg = fromGrp(grp)
    val sizeOfDomain = cg.largestMovedPoint.getOrElseFast(-1) + 1
    val lexChain = BuildChain.fromChain(cg.chain, permutation, Opt(BaseGuideLex(sizeOfDomain)))
    new PermGrpChainBuilder.LexElements[G](lexChain)
  }

  def base(grp: Grp[G]): Seq[Int] = grp match {
    case cj: PermGrpChainConjugated[G] => cj.originalChain.base.map(_ <|+| cj.g)
    case cg: PermGrpChain[G] => cg.chain.base
    case _ => base(fromGrp(grp): PermGrpChain[G])
  }

  def find[Q:Permutation](grp: Grp[G], q: Q): Opt[G] = grp match {
    case cg: PermGrpChain[G] => cg.chain.siftOther(q)
    case _ => find(fromGrp(grp): PermGrpChain[G], q)
  }

}

object PermGrpChainBuilder {

  type Aux[G, GG0 <: PermGrpChain[G]] = PermGrpChainBuilder[G] { type GG = GG0 }

  final class LexElements[G:Permutation](val lexChain: Chain[G]) extends BigIndexedSeq[G] {

    def length = lexChain.order

    def contains(g: G) = lexChain.sifts(g)

    def apply(idx: BigInt): G = {
      @tailrec def rec(current: Chain[G], curIdx: BigInt, curOrder: BigInt, curG: G): G = current match {
        case node: Node[G] =>
          val sortedOrbit = node.orbit.toSeq.sortBy(k => k <|+| curG)
          val nextOrder = curOrder / node.orbitSize
          val nextIdx = curIdx % nextOrder
          val orbitIndex = ((curIdx - nextIdx) / nextOrder).toInt
          val nextG = node.u(sortedOrbit(orbitIndex)) |+| curG
          rec(node.next, nextIdx, nextOrder, nextG)
        case _: Term[G] =>
          assert(curIdx == 0)
          curG
      }
      rec(lexChain, idx, lexChain.order, Group[G].id)
    }

    def iterator: Iterator[G] = {
      def rec(current: Chain[G], curG: G): Iterator[G] = current match {
        case node: Node[G] =>
          val sortedOrbit = node.orbit.toSeq.sortBy(k => k <|+| curG)
          for {
            b <- sortedOrbit.iterator
            nextG = node.u(b) |+| curG
            rest <- rec(node.next, nextG)
          } yield rest
        case _: Term[G] => Iterator(curG)
      }
      rec(lexChain, Group[G].id)
    }

  }

}
