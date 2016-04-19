package net.alasc.perms

package wrap

import spire.algebra.{Eq, Group}

import net.alasc.finite._



abstract class WrapGrpBuilder[G](implicit val equ: Eq[G],
                                 val group: Group[G],
                                 val repBuilder: FaithfulPermRepBuilder[G]) extends GrpBuilder[G] {

  def getBuilder(rep: FaithfulPermRep[G]): GrpBuilder[rep.Wrap]

  type GG = WrapGrp[G, R] forSome { type R <: FaithfulPermRep[G] with Singleton }

  def trivial: GG = {
    val rep = repBuilder.build(Nil)
    new WrapGrp[G, rep.type](rep, getBuilder(rep).trivial)
  }

  def fromGenerators(generators: Iterable[G]): GG = {
    implicit val rep = repBuilder.build(generators)
    val wg = getBuilder(rep).fromGenerators(generators.map(rep.Wrap))
    new WrapGrp[G, rep.type](rep, wg)
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt): GG = {
    implicit val rep = repBuilder.build(generators)
    val wg = getBuilder(rep).fromGeneratorsAndOrder(generators.map(rep.Wrap), order)
    new WrapGrp[G, rep.type](rep, wg)
  }

  def fromGrp(grp: Grp[G]): GG = grp match {
    case wg: WrapGrp[G, _] => wg
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

  def union(x: Grp[G], y: Grp[G]): GG = {
    val rep = repBuilder.build(x.generators ++ y.generators)
    implicit val b = getBuilder(rep)
    new WrapGrp[G, rep.type](rep, rep.wrap(x) union rep.wrap(y))
  }

  def intersect(x: Grp[G], y: Grp[G]): GG = {
    val rep = repBuilder.build(x.generators ++ y.generators)
    implicit val b = getBuilder(rep)
    new WrapGrp[G, rep.type](rep, rep.wrap(x) intersect rep.wrap(y))
  }

  override def leftCosetsBy(grp: Grp[G], subgrp: Grp[G]): LeftCosets[G] = ???

  override def rightCosetsBy(grp: Grp[G], subgrp: Grp[G]): RightCosets[G] = ???

}
