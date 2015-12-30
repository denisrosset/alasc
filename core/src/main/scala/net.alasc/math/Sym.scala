package net.alasc.math

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._
/* TODO: subclass Grp
/** Symmetric group of given degree. */
class Sym[P:Eq:Permutation](val degree: Int) extends GrpChain[P]( {

  override def toString = "S" + degree


  override def elementsIterator: Iterator[P] = domainArray(degree).permutations.map(images => Permutation[P].fromImages(images))


  override def contains(p: P) = p.supportMax.fold(true)(_ < degree)

}*/

object Sym {

  def generators[P:Permutation](degree: Int) = {
    val starts = 0 to (degree - 2)
    starts.map(k => Permutation[P].swapping(k, k + 1))
  }

  def domainArray(degree: Int): Array[Int] = Array.tabulate[Int](degree)(identity)

  def randomElement[P:Permutation](degree: Int, random: Random) = Permutation[P].fromImages(random.shuffle(domainArray(degree)))

  @tailrec def order(degree: Int, mul: BigInt = 1): BigInt =
    if (degree > 1) order(degree - 1, mul * degree) else mul

  def apply[P:ClassTag:Eq:Permutation](degree: Int) = {
    import Grp.defaultAlgorithms
    implicit val reps = new PermutationRepresentations[P]
    new GrpLazy(generators(degree), Opt(order(degree)), Opt((random: Random) => randomElement(degree, random)))
  }

}
