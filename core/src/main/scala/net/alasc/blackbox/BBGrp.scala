package net.alasc.blackbox

import scala.util.Random
import spire.algebra.{Eq, Group}
import net.alasc.finite._
import spire.syntax.cfor.cforRange
import net.alasc.syntax.group._

import scala.collection.mutable.ArrayBuffer

class BBGrp[G](
    val generators: IndexedSeq[G],
    val elements: Set[G]
  )(implicit
    val equ: Eq[G],
    val group: Group[G]
) extends Grp[G] {

  def nGenerators = generators.size

  def generator(i: Int) = generators(i)

  def iterator = elements.iterator

  def contains(g: G) = elements.contains(g)

  def order = elements.size

  def randomElement(random: Random): G = elements.iterator.drop(random.nextInt(elements.size)).next

}

object BBGrp {

  def fromGrp[G:Eq:Group](grp: Grp[G]): BBGrp[G] = grp match {
    case bb: BBGrp[G] => bb
    case _ => new BBGrp(grp.generators, grp.iterator.toSet)
  }

  def fromElements[G:Eq:Group](elements: Set[G]): BBGrp[G] = {
    val remaining = elements.to[collection.mutable.HashSet]
    val generators = ArrayBuffer.empty[G]
    val reconstructed = ArrayBuffer(Group[G].id)
    remaining -= Group[G].id
    while (remaining.nonEmpty) {
      val startRemove = reconstructed.length
      val g = remaining.head
      generators += g
      Dimino.runInduction(reconstructed, generators, generators.length - 1)
      cforRange(startRemove until reconstructed.length) { i => remaining -= reconstructed(i) }
    }
    new BBGrp[G](generators, elements)
  }

  def filter[G:Eq:Group](grp: Grp[G], predicate: G => Boolean): BBGrp[G] =
    BBGrp.fromElements(BBGrp.fromGrp(grp).elements.filter(predicate))


}