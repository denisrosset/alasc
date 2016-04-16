package net.alasc.blackbox

import scala.util.Random

import net.alasc.finite._

class BBGrp[G](
    val generators: Iterable[G],
    val elements: Set[G]
  )(implicit
    val builder: BBGrpBuilder[G]
) extends Grp[G] {

  def iterator = elements.iterator

  def contains(g: G) = elements.contains(g)

  def order = elements.size

  def randomElement(random: Random): G = elements.iterator.drop(random.nextInt(elements.size)).next

}
