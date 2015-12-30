package net.alasc.math

import scala.util.Random
import scala.collection.immutable.BitSet

import spire.algebra.{Eq, Group}

import net.alasc.algebra._
import net.alasc.syntax.permutationAction._

/** Symmetric group of given degree. */
class Sym[P](val degree: Int) {
  override def toString = "S" + degree
}

class SymPermutationSubgroup[P:Eq:Permutation] extends Subgroup[Sym[P], P] {

  def group: Group[P] = Permutation[P]
  def equ: Eq[P] = Eq[P]

  protected def domainSequence(degree: Int): Seq[Int] = 0 until degree
  def order(s: Sym[P]) = (BigInt(1) /: (1 to s.degree))(_*_)
  override def contains(s: Sym[P], p: P) = p.supportMax.fold(true)(_ < s.degree)
  def randomElement(s: Sym[P], gen: Random) = Permutation[P].fromImages(gen.shuffle(domainSequence(s.degree)))
  def swapFun(i: Int, j: Int): (Int => Int) = (k => if (k == i) j else if (k == j) i else k)
  def generators(s: Sym[P]) =
    (0 to s.degree - 2).map(k => Permutation[P].fromSupportAndImageFun(BitSet(k, k + 1), swapFun(k, k + 1)))
  def iterator(s: Sym[P]) = domainSequence(s.degree).permutations.map(images => Permutation[P].fromImages(images))

}

object Sym {

  implicit def SymPermutationSubgroup[P:Eq:Permutation] =
    new SymPermutationSubgroup[P]

  def apply[P: Permutation](degree: Int) =
    new Sym[P](degree)

}
