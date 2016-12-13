package net.alasc.named

import scala.annotation.tailrec
import scala.util.Random
import spire.math.SafeLong
import spire.syntax.cfor._
import net.alasc.algebra._
import net.alasc.finite.{Grp, GrpBuilder}
import net.alasc.perms.Perm

object Symmetric {

  @tailrec def order(degree: Int, mul: SafeLong = 1): SafeLong =
    if (degree > 1) order(degree - 1, mul * degree) else mul

  def transposition(i: Int, j: Int): Perm =
    Perm.fromSupportAndImageFun(Set(i, j), x => if (x == i) j else (if (x == j) i else x))
  
  def domainArray(degree: Int): Array[Int] = Array.tabulate[Int](degree)(identity)

  def randomElement(degree: Int, random: Random): Perm = {
    // uses the Fisher-Yates shuffle, inside out variant
    val array = new Array[Int](degree)
    cforRange(0 until degree) { i =>
      val j = random.nextInt(i + 1)
      array(i) = array(j)
      array(j) = i
    }
    Perm.fromImages(array)
  }

  def apply(degree: Int)(implicit gb: GrpBuilder[Perm]): Grp[Perm] =
    if (degree < 2) Grp.trivial[Perm] else
    Grp.fromGeneratorsAndOrder(
      IndexedSeq(Cyclic.shift(degree), transposition(0, 1)),
      order(degree)
    )

}
