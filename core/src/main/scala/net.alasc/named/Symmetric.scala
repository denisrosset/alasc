package net.alasc.named

import scala.annotation.tailrec
import scala.util.Random

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.prep._
import net.alasc.syntax.all._

object Symmetric {

  @tailrec def order(degree: Int, mul: BigInt = 1): BigInt =
    if (degree > 1) order(degree - 1, mul * degree) else mul

  def transposition[G:Permutation](i: Int, j: Int): G =
    Permutation[G].fromSupportAndImageFun(Set(i, j), x => if (x == i) j else (if (x == j) i else x))
  
  def domainArray(degree: Int): Array[Int] = Array.tabulate[Int](degree)(identity)

  def randomElement[G:Permutation](degree: Int, random: Random) = Permutation[G].fromImages(random.shuffle(domainArray(degree)))

  def apply[G:Permutation:PGrpBuilder](degree: Int): PGrp[G] =
    if (degree < 2) PGrpBuilder[G].trivial else
    PGrpBuilder[G].fromGeneratorsAndOrder(
      Seq(Cyclic.shift[G](degree), transposition[G](0, 1)),
      order(degree)
    )

}
