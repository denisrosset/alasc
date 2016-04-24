package net.alasc.finite

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseChange, BaseSwap, BuildChain, GrpChain, GrpChainExplicit, Intersection, SchreierSims, Term}
import net.alasc.perms.FaithfulPermRepBuilder

final class GrpChainBuilder[G]
  (implicit val baseChange: BaseChange, val baseSwap: BaseSwap, val classTag: ClassTag[G],
  val equ: Eq[G], val group: Group[G], val repBuilder: FaithfulPermRepBuilder[G],
  val schreierSims: SchreierSims) extends GrpBuilder[G] {

  type GG = GrpChain[G, F] forSome { type F <: PermutationAction[G] with Singleton }

  def trivial: GG = {
    val rep = repBuilder.build(Nil)
    type F = rep.F
    implicit def F: F = rep.permutationAction
    new GrpChainExplicit[G, F](Term[G, F], generatorsOpt = Opt(Iterable.empty[G]))
  }

  def fromGenerators(generators: Iterable[G]): GG = {
    val rep = repBuilder.build(generators)
    type F = rep.F
    implicit def F: F = rep.permutationAction
    new GrpChainExplicit[G, F](BuildChain.fromGenerators[G, F](generators), generatorsOpt = Opt(generators))
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: SafeLong): GG = {
    val rep = repBuilder.build(generators)
    type F = rep.F
    implicit def F: F = rep.permutationAction
    new GrpChainExplicit[G, F](BuildChain.fromGeneratorsAndOrder[G, F](generators, order), generatorsOpt = Opt(generators))
  }

  def fromGrp(grp: Grp[G]): GG = grp match {
    case gc: GrpChain[G, _] => gc
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  protected def convertGrp[F <: PermutationAction[G] with Singleton](grp: Grp[G])(implicit F: F): GrpChain[G, F] =
    grp match {
      case gc: GrpChain[G, _] if gc.action eq F => gc.asInstanceOf[GrpChain[G, F]]
      case _ =>
        new GrpChainExplicit[G, F](BuildChain.fromGeneratorsAndOrder[G, F](grp.generators, grp.order),
          Opt(grp.generators))
    }

  def union(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) union(rhs, lhs) // ensure that lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(lhs)
    else {
      val rep = repBuilder.build(lhs.generators ++ rhs.generators)
      import rep.permutationAction
      //implicit def F: F = rep.permutationAction
      GrpChain.union(convertGrp[rep.F](lhs), rhs)
    }

  def intersect(lhs: Grp[G], rhs: Grp[G]): GG =
    if (rhs.order > lhs.order) intersect(rhs, lhs) // ensure that lhs.order >= rhs.order
    else if (lhs.hasSubgroup(rhs)) fromGrp(rhs)
    else {
      val rep = repBuilder.build(lhs.generators ++ rhs.generators)
      import rep.permutationAction
      GrpChain.subgroupFor(convertGrp[rep.F](lhs), Intersection[G, rep.F](convertGrp[rep.F](rhs).chain))
    }

  override def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G, subgrp.type] = {
    val rep = repBuilder.build(grp.generators)
    import rep.permutationAction
    GrpChain.leftCosetsBy(convertGrp[rep.F](grp), subgrp, convertGrp[rep.F](subgrp))
  }

  def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G, subgrp.type] = leftCosetsBy(grp, subgrp).inverse

}
