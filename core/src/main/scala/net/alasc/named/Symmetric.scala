package net.alasc.named

import scala.annotation.tailrec
import scala.util.Random

import spire.math.SafeLong

import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}

object Symmetric {

  @tailrec def order(degree: Int, mul: SafeLong = 1): SafeLong =
    if (degree > 1) order(degree - 1, mul * degree) else mul

  def transposition[G:PermutationBuilder](i: Int, j: Int): G =
    PermutationBuilder[G].fromSupportAndImageFun(Set(i, j), x => if (x == i) j else (if (x == j) i else x))
  
  def domainArray(degree: Int): Array[Int] = Array.tabulate[Int](degree)(identity)

  def randomElement[G:PermutationBuilder](degree: Int, random: Random) = PermutationBuilder[G].fromImages(random.shuffle(domainArray(degree)))

  def apply[G:PermutationBuilder:GrpBuilder](degree: Int): Grp[G] =
    if (degree < 2) Grp.trivial[G] else
    Grp.fromGeneratorsAndOrder(
      IndexedSeq(Cyclic.shift[G](degree), transposition[G](0, 1)),
      order(degree)
    )

}
