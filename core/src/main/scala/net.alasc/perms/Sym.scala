package net.alasc.perms

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.prep._
import net.alasc.syntax.permutationAction._

object Sym {

  def generators[G:Permutation](degree: Int) = {
    val starts = 0 to (degree - 2)
    starts.map(k => Permutation[G].swapping(k, k + 1))
  }

  def domainArray(degree: Int): Array[Int] = Array.tabulate[Int](degree)(identity)

  def randomElement[G:Permutation](degree: Int, random: Random) = Permutation[G].fromImages(random.shuffle(domainArray(degree)))

  @tailrec def order(degree: Int, mul: BigInt = 1): BigInt =
    if (degree > 1) order(degree - 1, mul * degree) else mul

  def apply[G:ClassTag:Eq:PGrpBuilder:Permutation](degree: Int) =
    implicitly[PGrpBuilder[G]].fromGeneratorsRandomElementsAndOrder(
      generators(degree), (random: Random) => randomElement(degree, random), order(degree)
    )

}
