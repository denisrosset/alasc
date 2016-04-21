package net.alasc.perms

package wrap

import spire.algebra.{Eq, Group}
import spire.math.SafeLong

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

  def fromGeneratorsAndOrder(generators: Iterable[G], order: SafeLong): GG = {
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

  override def leftCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): LeftCosets[G] = {
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

  }

  def rightCosetsBy(grp0: Grp[G], subgrp0: Grp[G]): RightCosets[G] = leftCosetsBy(grp0, subgrp0).inverse

}
