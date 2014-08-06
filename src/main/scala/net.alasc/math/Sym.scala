package net.alasc.algebra

import net.alasc.syntax.permutation._
import scala.util.Random
import scala.collection.immutable.BitSet

/** Symmetric group of given degree. */
class Sym[P](val degree: Int) {
  override def toString = "S" + degree
}

class SymPermutationSubgroup[P](implicit val scalar: Permutation[P], builder: PermutationBuilder[P]) extends PermutationSubgroup[Sym[P], P] {
  protected def domainSequence(degree: Int): Seq[Int] = (0 until degree).toSeq
  def order(s: Sym[P]) = (BigInt(1) /: (1 to s.degree))(_*_)
  override def contains(s: Sym[P], p: P) = p.supportMax < s.degree
  def random(s: Sym[P], gen: Random) = builder.fromImages(gen.shuffle(domainSequence(s.degree)))
  def generators(s: Sym[P]) = (0 to s.degree - 2).map(k => builder.fromSupportAndImages(BitSet(k, k + 1),
    i => if (i == k) k + 1
    else if (i == k + 1) k
    else i))
  def elements(s: Sym[P]) = new Iterable[P] {
    override def stringPrefix = "Elements"
    def iterator = domainSequence(s.degree).permutations.map(images => builder.fromImages(images))
  }
}

object Sym {
  implicit def SymPermutationSubgroup[P](implicit scalar: Permutation[P], builder: PermutationBuilder[P]) =
    new SymPermutationSubgroup[P]
  def apply[P: Permutation](degree: Int)(implicit ev: Permutation[P], builder: PermutationBuilder[P]) =
    new Sym[P](degree)
}
