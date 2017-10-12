package net.alasc.perms.internal

import spire.algebra.Eq
import spire.syntax.cfor.cforRange
import net.alasc.algebra.PermutationAction
import net.alasc.util.NNOption

import syntax._

object GenPrm {

  @inline final def seed = 0xf444c3b3

  import java.lang.Integer.rotateRight

  @inline final def pairHash(preimage: Int, image: Int) = preimage ^ rotateRight(image, 16)

  def hash(lhs: GenPrm): Int = {
    // The hash algorithm is specified as follows.
    // We consider the permutation as a list of all the (preimage, image) pairs
    // which have preimage != image; the list is ordered according to the preimage.
    // The hashcode of each pair is given by preimage + (image rot 16), and
    // the pairs are hashed using the same strategy as MurmurHash3.orderedHash
    import scala.util.hashing.MurmurHash3.{mix, finalizeHash}
    var n = 0
    var h = seed
    cforRange(0 until lhs.length) { preimage =>
      val image = lhs(preimage)
      if (preimage != image) {
        h = mix(h, pairHash(preimage, image))
        n += 1
      }
    }
    finalizeHash(h, n)
  }

  object equ extends Eq[GenPrm] {
    def eqv(lhs: GenPrm, rhs: GenPrm): Boolean = java.util.Arrays.equals(lhs, rhs)
  }

  object permutationAction extends PermutationAction[GenPrm] {
    def isFaithful: Boolean = true
    override def movesPoint(g: GenPrm, i: Int): Boolean = g.image(i) != i
    override def nMovedPoints(g: GenPrm): Int = g.nMovedPoints
    override def movedPoints(g: GenPrm): Set[Int] = g.movedPoints
    override def largestMovedPoint(g: GenPrm): NNOption = new NNOption(g.largestMovedPoint)
    override def smallestMovedPoint(g: GenPrm): NNOption = new NNOption(g.smallestMovedPoint)
    // TODO override def signPerm(g: GenPrm): Int = super.signPerm(g)
    // TODO override def cycleStructure(g: GenPrm): Map[Int, Int] = super.cycleStructure(g)
    // TODO override def permutationOrder(g: GenPrm): SafeLong = super.permutationOrder(g)
    // TODO override def orbit(g: GenPrm, i: Int): Set[Int] = super.orbit(g, i)
    // TODO override def images(g: GenPrm, n: Int): Seq[Int] = super.images(g, n)
    // TODO override def toPerm(g: GenPrm): Perm = super.toPerm(g)
    // TODO override def hasSameAction[Q](g: GenPrm, q: Q)(implicit Q: PermutationAction[Q]): Boolean = super.hasSameAction(g, q)
    // TODO override def smallestMovedPoint(generators: Iterable[GenPrm]): NNOption = super.smallestMovedPoint(generators)
    // TODO override def largestMovedPoint(generators: Iterable[GenPrm]): NNOption = super.largestMovedPoint(generators)
    def findMovedPoint(g: GenPrm): NNOption = new NNOption(g.smallestMovedPoint)
    def movedPointsUpperBound(g: GenPrm): NNOption = new NNOption(g.length - 1)
    def actr(p: Int, g: GenPrm): Int = g.image(p)
    override def movesAnyPoint(g: GenPrm): Boolean = {
      cforRange(0 until g.length) { i =>
        if (g(i) != i) return true
      }
      false
    }

    def actl(g: GenPrm, p: Int): Int = g.invImage(p)
  }

}
