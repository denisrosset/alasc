package com.faacets.perm
import scala.annotation.elidable
import scala.annotation.elidable._

abstract class Permutation extends PermutationHelpers with PartialFunction[Domain, Domain] with Iterable[(Domain, Domain)] with Ordered[Permutation] {
  def inverse: Permutation
  def *(other: Permutation): Permutation
  def isIdentity: Boolean
  override def isDefinedAt(k: Domain) = (k >= 0) && (k <= size)
  override def apply(k: Domain) = arr(k)
  def image(k: Domain) = apply(k)
  def images: Array[Domain] = arr.clone
  def iterator = arr.view.zipWithIndex.map(_.swap).iterator
  private[perm] def arr: Array[Domain]
  private[perm] def toArrayPermutation = ArrayPermutation(arr)
  def domainSize = size
  def domain = (0 until domainSize)
  override def toString = arr.mkString("Permutation(", ",", ")")

  override def equals(other: Any): Boolean =
    other match {
      case that: Permutation =>
        (that canEqual this) &&
        (size == this.size) &&
        arr.sameElements(that.arr)
      case _ => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[Permutation]
  
  override def hashCode: Int = scala.util.hashing.MurmurHash3.seqHash(arr)

  @elidable(ASSERTION) def assertValid {
    val notInside = scala.collection.mutable.BitSet(domain: _*)
    domain.map(image(_)).foreach(i => {
      assert(notInside(i)) // should not be already inside, thus a duplicate element
      notInside -= i
    })
    assert(notInside.isEmpty)
  }
  def compare(that: Permutation) = {
    require(domainSize == that.domainSize)
    val firstNotEqual = (0 until domainSize).find(i => image(i) != that.image(i))
    firstNotEqual match {
      case None => 0
      case Some(i) if image(i) <= that.image(i) => -1
      case _ => 1
    }
  }
}

trait Identity extends Permutation {
  def isIdentity = true
  override def apply(k: Domain) = k
  def inverse = this
  def *(other: Permutation) = {
    if (size != other.size)
      throw new IllegalArgumentException("Permutation size mismatch")
    other
  }
}

sealed case class P0() extends Identity {
  private[perm] def arr = Array(0)
  override def size = 1
  override def equals(other: Any): Boolean = other match {
    case P0() => true
    case _ => super.equals(other)
  }
}

sealed case class P01() extends Identity {
  private[perm] def arr = Array(0, 1)
  override def size = 2
  override def equals(other: Any): Boolean = other match {
    case P01() => true
    case P10() => false
    case _ => super.equals(other)
  }
}

sealed case class P10() extends Permutation {
  private[perm] def arr = Array(1,0)
  override def size = 2
  def isIdentity = false

  override def apply(k: Domain) = 1 - k

  def inverse = this
  def *(other: Permutation) = other match {
    case P01() => this
    case P10() => P01()
    case x: ArrayPermutation if x.size == 2 => this.toArrayPermutation*other
    case _ => throw new IllegalArgumentException("Permutation size mismatch")
  }
  override def equals(other: Any): Boolean = other match {
    case P10() => true
    case P01() => false
    case _ => super.equals(other)
  }
}

private[perm] sealed case class ArrayPermutation (val arr: Array[Domain]) extends Permutation
{
  // note that the image of b under the product g*h is given by:
  // b^(g*h) = (b^g)^h
  def *(that: Permutation): Permutation = {
    if (size != that.size)
      throw new IllegalArgumentException("Permutation size mismatch")
    val a = new Array[Domain](size)
    for (i <- 0 until size) a(i) = that.arr(arr(i))
    Permutation(a)
  }
  def isIdentity: Boolean = iterator.forall( x => x._1 == x._2 )
  def inverse: Permutation = {
    val a = new Array[Domain](size)
    for (i <- 0 until size) a(arr(i)) = i
    Permutation(a)
  }
}

object Permutation {
  def all(n: Int): Iterator[Permutation] = (0 until n).toArray.permutations.map(ArrayPermutation(_))
  def identity(n: Int) = n match {
    case 1 => P0()
    case 2 => P01()
    case _ => ArrayPermutation((0 until n).toArray)
  }
  def identifySmallPermutation(images: Array[Domain]) = images match {
    case Array(0) => P0()
    case Array(0,1) => P01()
    case Array(1,0) => P10()
    case _ => throw new IllegalArgumentException("Small permutation cannot be instantiated")
  }
  def fromUniqueArray(images: Array[Domain]): Permutation =
    if (images.size <= 2)
      identifySmallPermutation(images)
    else
      ArrayPermutation(images)
  def apply(images: Array[Domain]): Permutation =
    if (images.size <= 2)
      identifySmallPermutation(images)
    else
      ArrayPermutation(images.clone)
  def apply(images: Domain*): Permutation = apply(images.toArray)
  def random(n: Int)(implicit gen: scala.util.Random = scala.util.Random) =
    Permutation(gen.shuffle( (0 until n).toBuffer ).toArray)
}
