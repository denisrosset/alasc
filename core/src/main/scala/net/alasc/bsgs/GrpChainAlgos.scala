package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.finite._
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder}

final class GrpChainAlgos[G]
  (implicit val baseChange: BaseChange, val baseSwap: BaseSwap, val classTag: ClassTag[G],
  val equ: Eq[G], val group: Group[G], val repBuilder: FaithfulPermRepBuilder[G],
  val schreierSims: SchreierSims) extends GrpAlgosImpl[G] {

  type GG = GrpChain[G, F] forSome { type F <: PermutationAction[G] with Singleton }

  def trivial: GG = {
    val rep: FaithfulPermRep[G, SafeLong] = repBuilder.build[SafeLong](Nil)
    type F = rep.F
    implicit def F: F = rep.permutationAction
    new GrpChainExplicit[G, F](Term[G, F], Opt(IndexedSeq.empty[G]), Opt(rep))
  }

  def fromGenerators(generators: IndexedSeq[G]): GG = {
    val rep: FaithfulPermRep[G, SafeLong] = repBuilder.build[SafeLong](generators)
    type F = rep.F
    implicit def F: F = rep.permutationAction
    new GrpChainExplicit[G, F](BuildChain.fromGenerators[G, F](generators), Opt(generators), Opt(rep))
  }

  def fromGeneratorsAndOrder(generators: IndexedSeq[G], order: SafeLong): GG = {
    val rep: FaithfulPermRep[G, SafeLong] = repBuilder.build[SafeLong](generators)
    type F = rep.F
    implicit def F: F = rep.permutationAction
    new GrpChainExplicit[G, F](BuildChain.fromGeneratorsAndOrder[G, F](generators, order), Opt(generators), Opt(rep))
  }

  def fromGrp(grp: Grp[G]): GG = grp match {
    case gc: GrpChain[G, _] => gc
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  def convertGrp[F <: PermutationAction[G] with Singleton](grp: Grp[G], repOpt: Opt[FaithfulPermRep[G, _]])
                                                          (implicit F: F): GrpChain[G, F] = {
    val GF = GrpChain.In(F)
    grp match {
      case GF(gc) => gc
      case _ =>
        val chain = BuildChain.fromGeneratorsAndOrder[G, F](grp.generators, grp.order)
        new GrpChainExplicit[G, F](chain, Opt(grp.generators), repOpt)
    }
  }

  def union(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) union(rhs, lhs) // ensure that lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(lhs)
    else {
      def fallback: GG = {
        val rep = repBuilder.build[SafeLong](lhs.generators ++ rhs.generators)
        import rep.permutationAction
        GrpChain.union(convertGrp[rep.F](lhs, Opt(rep)), rhs.generators)
      }
      lhs match {
        case GrpChain.AndAction(pair) => pair.grp.repOpt match {
          case Opt(rep) if rhs.generators.forall(rep.represents) =>
            GrpChain.union[G, pair.Action](pair.grp, rhs.generators)
          case _ => fallback
        }
        case _ => fallback
      }
    }

  def intersect(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure that lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(rhs)
    else {
      def fallback: GG = {
        val rep = repBuilder.build[SafeLong](lhs.generators ++ rhs.generators)
        import rep.permutationAction
        GrpChain.intersect(convertGrp[rep.F](lhs, Opt(rep)), convertGrp[rep.F](rhs, Opt(rep)))
      }
      lhs match {
        case GrpChain.AndAction(pair) =>
          val GF = GrpChain.In(pair.action)
          rhs match {
            case GF(rhs1) => GrpChain.intersect[G, pair.Action](pair.grp, rhs1)
            case _ => pair.grp.repOpt match {
              case Opt(rep) if rhs.generators.forall(rep.represents) =>
                GrpChain.intersect(pair.grp, convertGrp[pair.Action](rhs, pair.grp.repOpt)(pair.action))
              case _ => fallback
            }
          }
          case _ => fallback
      }
    }

  def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G, subgrp.type] = grp match {
    case GrpChain.AndAction(pair) =>
      GrpChain.leftCosetsBy[G, pair.Action](pair.grp, subgrp, convertGrp[pair.Action](subgrp, pair.grp.repOpt)(pair.action))
    case _ =>
      val rep = repBuilder.build[SafeLong](grp.generators)
      import rep.permutationAction
      GrpChain.leftCosetsBy(convertGrp[rep.F](grp, Opt(rep)), subgrp, convertGrp[rep.F](subgrp, Opt(rep)))
  }

  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G, subgrp.type] = leftCosetsBy(grp, subgrp).inverse

}
