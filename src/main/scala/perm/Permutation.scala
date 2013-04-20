package com.faacets.perm
import scala.annotation.elidable
import scala.annotation.elidable._

class Permutation private (private val imgs: Array[Domain]) extends Map[Domain, Domain] //with Iterable[(Domain, Domain)]
{
  override def size = imgs.size
  def get(x: Domain) = if (x >= 0 && x < imgs.size) Some(imgs(x)) else None
  def iterator = ((0 until imgs.size) zip imgs).iterator
  // we do not implement the addition or removal of elements
  def +[B1 >: Domain](x: (Domain, B1)) = ???
  def -(x: Domain) = ???
  override def keysIterator = (0 until size).iterator
  override def keys = 0 until size
  def inverse: Permutation = {
    val a = new Array[Domain](size)
    for (i <- keys) a(imgs(i)) = i
    new Permutation(a)
  }
  def *(that: Permutation): Permutation = {
    @elidable(ASSERTION) def sanityCheck = { assert(size == that.size) }
    sanityCheck
    val a = new Array[Domain](size)
    for (i <- keys) a(i) = imgs(that.imgs(i))
    new Permutation(a)
  }
  def isIdentity: Boolean = iterator.forall( x => x._1 == x._2 )
  @elidable(ASSERTION) def assertValid {
    val notInside = scala.collection.mutable.BitSet(keys: _*)
      keys.map(apply(_)).foreach(i => {
        assert(notInside(i)) // should not be already inside, thus a duplicate element
        notInside -= i
      })
    assert(notInside.isEmpty)
  }
}

object Permutation {
  def apply(images: Array[Domain]) = new Permutation(images.clone)
  def apply(images: scala.collection.immutable.Seq[Domain]) = new Permutation(images.toArray)
}
