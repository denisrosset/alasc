package net.alasc.perms

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseChange, BaseSwap, BuildChain, GrpChain, GrpChainExplicit, SchreierSims, Term}
import net.alasc.finite._

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

  def union(x: Grp[G], y: Grp[G]): GG = ???/*
    val rep = repBuilder.build(x.generators ++ y.generators)
    implicit val b = getBuilder(rep)
    new WrapGrp[G, rep.type](rep, rep.wrap(x) union rep.wrap(y))
  }*/

  def intersect(x: Grp[G], y: Grp[G]): GG = ??? /*{
    val rep = repBuilder.build(x.generators ++ y.generators)
    implicit val b = getBuilder(rep)
    new WrapGrp[G, rep.type](rep, rep.wrap(x) intersect rep.wrap(y))
  }*/

  override def leftCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): LeftCosets[G] = ??? /*{
    implicit val rep = repBuilder.build(grp0.generators)
    implicit val builder = getBuilder(rep)
    val grp1 = rep.wrap(grp0)
    val subgrp1 = rep.wrap(subgrp0)
    val cosets1 = grp1.leftCosetsBy(subgrp1)
    new LeftCosetsImpl[G] {
      def iterator: Iterator[LeftCoset[G]] = cosets1.iterator.map { coset1 => new LeftCoset[G](coset1.g.underlying, subgrp) }
      val subgrp: Grp[G] = subgrp0
      val grp: Grp[G] = grp0
    }

  }*/

  def rightCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): RightCosets[G] = leftCosetsBy(grp0, subgrp0).inverse

}
