package net.alasc.blackbox

import scala.annotation.tailrec
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._

import net.alasc.finite._

class BBGrpBuilder[G](implicit
    val group: Group[G],
    val equ: Eq[G]
  ) extends GrpBuilder[G] {

  type GG = Grp[G]

  def trivial: GG = new BBGrp(Iterable.empty[G], Set(group.id))(this)

  def fromGenerators(generators: Iterable[G]): Grp[G] = {
    @tailrec def rec(elements: Set[G]): Set[G] = {
      val newElements = generators
        .flatMap(g1 => elements.map(g2 => g1 |+| g2))
        .filterNot(elements.contains(_))
      if (newElements.isEmpty) elements else rec(elements ++ newElements)
    }
    new BBGrp(generators, rec(generators.toSet), null)(this)
  }

  def fromGeneratorsAndOrder(generators: Iterable[G], order: BigInt): GG =
    fromGenerators(generators)

  def fromGeneratorsRandomElementsAndOrder(generators: Iterable[G], randomElement: Random => G, order: BigInt): GG =
    fromGenerators(generators)

  def fromGrp(grp: Grp[G]): GG =
    new BBGrp(grp.generators, grp.iterator.toSet, null)(this)

}
