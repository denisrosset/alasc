package net.alasc.blackbox

import scala.util.Random

import spire.algebra.{Eq, Group}

import net.alasc.finite._

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
