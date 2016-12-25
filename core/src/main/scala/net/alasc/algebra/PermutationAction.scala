package net.alasc.algebra

import scala.annotation.tailrec

import spire.algebra._
import spire.math.SafeLong
import spire.std.int._

import metal.syntax._

import net.alasc.perms.Perm
import net.alasc.util._

/** Permutation action: a group action on non-negative integers.
  *
  * The domain on which the permutation acts starts at 0, negative integers should be left invariant.
  *
  * The standard action, whose speed should be optimized, is the right action.
  *
  */
trait PermutationAction[G] extends Action[Int, G] { self =>

  /** Returns true if the action is known to be faithful, false if it is not
    * the case or the status is unknown.
    */
  def isFaithful: Boolean

  /** Returns an arbitrary element in the support of `g` or NNNone if support empty.
    *
    * Widely used in the BSGS algorithms, needs to have a fast implementation.
    */
  def findMovedPoint(g: G): NNOption

  /** Returns a fast-to-compute upper bound on the maximal element in the support of `g`. */
  def movedPointsUpperBound(g: G): NNOption

  /** Tests if `g` moves any point. */
  def movesAnyPoint(g: G): Boolean = findMovedPoint(g).nonEmpty

  /** Tests if the point `i` is in the support of `g`. */
  def movesPoint(g: G, i: Int): Boolean = actr(i, g) != i

  /** Number of non-negative integers moved by the permutation. */
  def nMovedPoints(g: G): Int = {
    val n = movedPointsUpperBound(g).getOrElseFast(-1) + 1
    var nMoved: Int = 0
    var k: Int = 0
    while (k < n) {
      if (movesPoint(g, k))
        nMoved += 1
      k += 1
    }
    nMoved
  }

  /** Returns a bit set of all non-negative integers k that are changed by the action of the permutation,
    * i.e. `S = { k | k <|+| g != k }`.
    */
  def movedPoints(g: G): Set[Int] = {
    val n = movedPointsUpperBound(g).getOrElseFast(-1) + 1
    val bitset = metal.mutable.FixedBitSet.reservedSize(largestMovedPoint(g).getOrElse(-1) + 1)
    var k: Int = 0
    while (k < n) {
      if (movesPoint(g, k))
        bitset += k
      k += 1
    }
    bitset.toScala
  }

  /** Returns the maximal element in the support of ` g`, or NNNone if the support is empty. */
  def largestMovedPoint(g: G): NNOption = {
    var k: Int = movedPointsUpperBound(g).getOrElseFast(-1)
    while (k > -1) {
      if (movesPoint(g, k)) return NNSome(k)
      k -= 1
    }
    NNNone
  }

  /** Returns the minimal element in the support of `g`, or NNNone if the support is empty. */
  def smallestMovedPoint(g: G): NNOption = {
    var k: Int = 0
    val n: Int = movedPointsUpperBound(g).getOrElseFast(-1) + 1
    while (k < n) {
      if (movesPoint(g, k)) return NNSome(k)
      k += 1
    }
    NNNone
  }

  /** Returns the sign of the permutation `g`. */
  def signPerm(g: G): Int = {
    val rest = metal.mutable.FixedBitSet.fromIterable(movedPoints(g))
    var sign = 1
    while (rest.nonEmpty) {
      val h = rest.min
      var iter = h
      var n = 0
      while (n == 0 || iter != h) {
        rest -= iter
        n += 1
        iter = actr(iter, g)
      }
      if (n % 2 == 0)
        sign = -sign
    }
    sign
  }

  /** Returns the cycle structure of the permutation `g`. */
  def cycleStructure(g: G): Map[Int, Int] = {
    val rest = metal.mutable.FixedBitSet.fromIterable(movedPoints(g))
    val cs = metal.mutable.HashMap.empty[Int, Int]
    while (rest.nonEmpty) {
      val h = rest.min
      var iter = h
      var n = 0
      while (n == 0 || iter != h) {
        rest -= iter
        n += 1
        iter = actr(iter, g)
      }
      cs(n) = cs.getOrElse(n, 0) + 1
    }
    cs.toScala
  }

  /** Returns the order of the permutation representation of this element. */
  def permutationOrder(g: G): SafeLong = {
    val cs = cycleStructure(g)
    cs.keys.foldLeft(SafeLong(1)) { case (acc, len) => spire.math.lcm(acc, SafeLong(len)) }
  }

  /** Returns the orbit of the domain element `i` under the action of `g`. */
  def orbit(g: G, i: Int): Set[Int] = {
    val mut = metal.mutable.ResizableBitSet(i)
    @tailrec def rec(k: Int): Unit =
      if (k != i) {
        mut += k
        rec(actr(k, g))
      }
    rec(actr(i, g))
    mut.toScala
  }

  /** Returns the images of `g` on the domain (0 until n).
    *
    * Requires that `n > supportMax(g)`.*/
  def images(g: G, n: Int): IndexedSeq[Int] = new IndexedSeq[Int] {
    require(largestMovedPoint(g).getOrElseFast(-1) < n)
    def length = n
    def apply(idx: Int) = actr(idx, g)
  }

  /** Returns a permutation with the same images as the action of `g`. */
  def toPerm(g: G): Perm =
    Perm.fromSupportAndImageFun(movedPoints(g), k => actr(k, g))

  /** Return the smallest element of the domain moved by the given generators, or [[NNNone]]. */
  def smallestMovedPoint(generators: Iterable[G]): NNOption = {
    var mn = Int.MaxValue
    var moved = false
    val it = generators.iterator
    while (it.hasNext) {
      val g = it.next()
      smallestMovedPoint(g) match {
        case NNOption(i) =>
          mn = spire.math.min(mn, i)
          moved = true
        case _ =>
      }
    }
    if (moved) NNOption(mn) else NNNone
  }

  /** Return the largest element of the domain moved by the given generators, or [[NNNone]]. */
  def largestMovedPoint(generators: Iterable[G]): NNOption = {
    var mx = -1
    val it = generators.iterator
    while (it.hasNext) {
      val g = it.next()
      mx = spire.math.max(largestMovedPoint(g).getOrElseFast(-1), mx)
    }
    if (mx >= 0) NNOption(mx) else NNNone
  }

}

object PermutationAction {

  def apply[G](implicit G: PermutationAction[G]): PermutationAction[G] = G

  // TODO: move to contravariant functor (cats)
  def contramap[A, B](fa: PermutationAction[A])(f: B => A): PermutationAction[B] = new PermutationAction[B] {
    def isFaithful: Boolean = false
    def findMovedPoint(b: B): NNOption = fa.findMovedPoint(f(b))
    def movedPointsUpperBound(b: B): NNOption = fa.movedPointsUpperBound(f(b))
    def actr(p: Int, b: B): Int = fa.actr(p, f(b))
    def actl(b: B, p: Int): Int = fa.actl(f(b), p)
    override def movesAnyPoint(b: B): Boolean = fa.movesAnyPoint(f(b))
    override def movesPoint(b: B, i: Int): Boolean = fa.movesPoint(f(b), i)
    override def nMovedPoints(b: B): Int = fa.nMovedPoints(f(b))
    override def movedPoints(b: B): Set[Int] = fa.movedPoints(f(b))
    override def largestMovedPoint(b: B): NNOption = fa.largestMovedPoint(f(b))
    override def smallestMovedPoint(b: B): NNOption = fa.smallestMovedPoint(f(b))
    override def signPerm(b: B): Int = fa.signPerm(f(b))
    override def cycleStructure(b: B): Map[Int, Int] = fa.cycleStructure(f(b))
    override def permutationOrder(b: B): SafeLong = fa.permutationOrder(f(b))
    override def orbit(b: B, i: Int): Set[Int] = fa.orbit(f(b), i)
    override def images(b: B, n: Int): IndexedSeq[Int] = fa.images(f(b), n)
    override def toPerm(b: B): Perm = fa.toPerm(f(b))
    override def smallestMovedPoint(generators: Iterable[B]): NNOption = super.smallestMovedPoint(generators)
    override def largestMovedPoint(generators: Iterable[B]): NNOption = super.largestMovedPoint(generators)
  }

  def trivial[G]: PermutationAction[G] = new PermutationAction[G] {
    def isFaithful: Boolean = false
    def findMovedPoint(g: G): NNOption = NNNone
    def movedPointsUpperBound(g: G): NNOption = NNNone
    def actr(p: Int, g: G): Int = p
    def actl(g: G, p: Int): Int = p
  }

  def sign[G](G: PermutationAction[G]): PermutationAction[G] = new PermutationAction[G] {
    def isFaithful: Boolean = false
    def findMovedPoint(g: G): NNOption =
      if (G.signPerm(g) == -1) NNOption(0) else NNNone
    def movedPointsUpperBound(g: G): NNOption = NNOption(1)
    def actr(p: Int, g: G): Int =
      if (G.signPerm(g) == -1 && (p == 0 || p == 1)) 1 - p else p
    def actl(g: G, p: Int): Int =
      if (G.signPerm(g) == -1 && (p == 0 || p == 1)) 1 - p else p
    override def movesAnyPoint(g: G): Boolean = G.signPerm(g) == -1
    override def movesPoint(g: G, p: Int): Boolean = G.signPerm(g) == -1 && (p == 0 || p == 1)
    override def nMovedPoints(g: G): Int =
      if (G.signPerm(g) == -1) 2 else 0
    override def movedPoints(g: G): Set[Int] =
      if (G.signPerm(g) == -1) Set(0, 1) else Set.empty[Int]
    override def largestMovedPoint(g: G): NNOption =
      if (G.signPerm(g) == -1) NNOption(1) else NNNone
    override def smallestMovedPoint(g: G): NNOption =
      if (G.signPerm(g) == -1) NNOption(0) else NNNone
    override def signPerm(g: G): Int = G.signPerm(g)
    override def permutationOrder(g: G): SafeLong =
    if (G.signPerm(g) == -1) 2 else 1
    override def orbit(g: G, p: Int): Set[Int] =
      if (G.signPerm(g) == -1 && (p == 0 || p == 1)) Set(0, 1) else Set(p)
    /** Returns a permutation with the same images as the action of `g`. */
    override def toPerm(g: G): Perm =
    if (G.signPerm(g) == -1) Perm(0, 1) else Perm.id
  }

}
