package net.alasc

import scala.util.parsing.combinator._
import scala.util.Random
import scala.util.hashing.MurmurHash3

/*
## `Perm`

These permutations are backed by an image array, and describe a right action on elements
of the domain \\( \beta \\) :

\\[ \beta^(g h) = (\beta^g)^h \\].

The implementation is organized as follows:

- the interface trait `Perm` without implementation methods,
- an implementation trait `PermLike` with generic implementations of
  non-speed critical methods,
- a specification for `PermBuilder` to create `Perm` instances from
  images or pre-images,
- a RichPerm extension with several helper methods.

`Perm` has several optimized implementations for speed. Small sizes are implemented
using table lookups, while bigger permutations are implemented using `Byte`, `Short`
or `Int` primitive `Array`s.
*/

/** Describes a permutation of n elements. */
trait Perm {
  /** Builder associated with the permutation implementation. */
  def builder: PermBuilder
  /** Hashes this perm using a MurmurHash3 ordered hash with seed Perm.hashSeed. */
  def hash: Int
  /** Tests for equality with another Perm. */
  def ===(that: Perm): Boolean
  /** Size of the current permutation. */
  def size: Int
  /** Image of a domain element. */
  def image(k: Dom): Dom
  /** Tests if this permutation is the identity. */
  def isIdentity: Boolean
  /** Returns the inverse of this permutation. */
  def inverse: Perm
  /** Returns the product of this permutation with another permutation. */
  def *(that: Perm): Perm
}

/** Specification of a factory for permutations. */
trait PermBuilder {
  /** Builds a permutation from images.
    * 
    * Constructs a permutation of given size from a 
    * function computing images.
    * 
    * @param  size The size of the new permutation
    * @param  f    function returning for k the image of k
    *              f: k --> k^g
    * @return the new permutation built
    */
  def fromImages(size: Int, f: Dom => Dom): Perm
  /** Builds a permutation from preimages.
    * 
    * Constructs a permutation of given size from a
    * function computing preimages.
    * 
    * @param  size The size of the new permutation
    * @param  f    function returning for k the image of k
    *              f: k --> k^(g^-1)
    * @return the new permutation built
    */
  def fromPreimages(size: Int, f: Dom => Dom): Perm
}

object Perm extends PermBuilder {
  final val hashSeed = "Perm".hashCode
  def identity(size: Int) = fromImages(size, k => k)
  def fromImages(size: Int, f: Dom => Dom) = IntPerm.fromImages(size, f) // TODO dispatch using size
  def fromPreimages(size: Int, f: Dom => Dom) = IntPerm.fromPreimages(size, f)
}

trait PermApply extends Perm {
  def apply(cycle: Dom*): Perm = {
    import Dom.ZeroBased._
    def rotateRight[T](seq: Seq[T]) = cycle.last +: cycle.tail
    val map = Map(rotateRight(cycle) zip cycle: _*)
    builder.fromImages(size, k => image(map.getOrElse(k, k)))
  }
}

trait PermLike extends PermApply {
  def builder: PermBuilder
  def size: Int
  def image(k: Dom): Dom

  def hash = MurmurHash3.orderedHash(Dom.domain(size).map(i => image(i)._0), Perm.hashSeed)
  def *(that: Perm) = builder.fromImages(size, k => that.image(image(k)))
  def ===(that: Perm) = Dom.domain(size).forall( k => image(k) === that.image(k) )
  def isIdentity = Dom.domain(size).forall( k => k === image(k) )
  def inverse = builder.fromPreimages(size, k => image(k))
}

/*
Implementation of `Perm` written for code clarity. Used in tests to check correctness of
the optimized implementations.
*/
class GenericPerm(images: Seq[Dom]) extends PermLike {
  import Dom.ZeroBased._
  def builder = GenericPerm
  def size = images.size
  def image(k: Dom) = images(k)
}

object GenericPerm extends PermBuilder {
  def fromImages(size: Int, f: Dom => Dom) = new GenericPerm(Dom.domain(size).map(f).toIndexedSeq)
  def fromPreimages(size: Int, f: Dom => Dom) = {
    import Dom.ZeroBased._
    val images = scala.collection.mutable.ArrayBuffer.fill(size)(0)
    images.indices.foreach ( i => images(f(i)) = i )
    fromImages(size, images(_))
  }
}

final class IntPerm private[alasc](val images: Array[Int]) extends PermApply {
  import Dom.ZeroBased._
  def builder = IntPerm
  final def size = images.size
  final def image(k: Dom) = images(k)

  final def hash = MurmurHash3.orderedHash(images, Perm.hashSeed)
  final def *(perm: Perm): Perm = perm match {
    case that: IntPerm =>
      val n = size
      val newArray = new Array[Int](n)
      var i = 0
      while (i < n) {
        newArray(i) = that.images(images(i))
        i += 1
      }
      new IntPerm(newArray)
    case  _ => builder.fromImages(size, k => perm.image(image(k)))
  }
  final def inverse: Perm = {
    val n = size
    val newArray = new Array[Int](n)
    var i = 0
    while (i < n) {
      newArray(images(i)) = i
      i += 1
    }
    new IntPerm(newArray)
  }
  def ===(perm: Perm): Boolean = perm match {
    case that: IntPerm =>
      val n = size
      var i = 0
      while (i < n) {
        if (images(i) != that.images(i))
          return false
      i += 1
      }
      true
    case _ => (0 until size).forall( k => image(k) == perm.image(k) )
  }
  def isIdentity: Boolean = {
    val n = size
    var i = 0
    while (i < n) {
      if (images(i) != i)
        return false
      i += 1
    }
    true
  }
}

object IntPerm extends PermBuilder {
  def fromImages(size: Int, f: Dom => Dom) = {
    import Dom.ZeroBased._
    new IntPerm(Array.tabulate(size)(k => f(k)))
  }
  def fromPreimages(size: Int, f: Dom => Dom) = {
    import Dom.ZeroBased._
    val array = new Array[Int](size)
    array.indices.foreach( i => array(f(i)) = i )
    new IntPerm(array)
  }
}

/*
implicit class RichPerm(perm: Perm) {
}
trait PermParserLike extends RegexParsers {
  def cycle = "(" ~> (repsep(oneBasedDom, ",") <~ ")")
  def oneBasedDom: Parser[Dom] = """\d+""".r ^^ { i => Dom._1(i.toInt) }
  def size: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def cycles(sz: Int) = rep(cycle) ^^ { cycles => Perm.fromCycles(sz, cycles) }
}

sealed abstract class Perm extends PermElement[Perm] {
  def toByteArray = {
    require_(size <= Byte.MaxValue)
    Array.tabulate[Byte](size)(i => image(Dom._0(i))._0.toByte)
  }
  def toShortArray = {
    require_(size <= Short.MaxValue)
    Array.tabulate[Short](size)(i => image(Dom._0(i))._0.toShort)
  }
  def toIntArray =
    Array.tabulate[Int](size)(i => image(Dom._0(i))._0)
  def optimized: Perm = size match {
    // small cases
//    case x if x <= Byte.MaxValue => new BytePerm(toByteArray)
//    case x if x <= Short.MaxValue => new ShortPerm(toShortArray)
    case _ => new GenericPerm(toIntArray)
  }
  def unoptimized: Perm = new GenericPerm(toIntArray)
  //  def toTeX = TeX("{}^"+arr.size)+TeX(cycles.filter(_.size>1).map(_.mkString("(",",",")")).mkString(""))
  def isIdentity: Boolean = {
    val n = size
    var i = 0
    while (i < n) {
      if (i != image(Dom._0(i))._0)
        return false
      i += 1
    }
    return true
  }
  override def toString = "Perm("+size+")"+cyclesToText
  def apply(s: String): Perm = {
    val points = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val list = s.map( c => Dom._0(points.indexOf(c)) )
    apply(list:_*)
  }
  def apply(cycle: Dom*): Perm
  def withSwap(i: Dom, j: Dom): Perm
  final def compatible(that: Perm) = size == that.size
  def toExplicit = this
  def ++(that: Perm): Perm = {
    val n = size
    Perm(DomArray._0(images._0 ++ that.images._0.map(_ + n)))
  }
  final def invImage(k: Dom): Dom = {
    val n = size
    var i = 0
    while (i < n) {
      val d = Dom._0(i)
      if (image(d) == k)
        return d
      i += 1
    }
    throw new IllegalArgumentException("Permutation should contain the image")
  }
  final def ===(that: Perm): Boolean = (this eq that) || {
    require_(compatible(that))
    val n = size
    var i = 0
    while (i < n) {
      val k = Dom._0(i)
      if (image(k) != that.image(k))
        return false
      i += 1
    }
    true
  }
  final def images: DomArray = DomArray((0 until size).map(i => image(Dom._0(i))):_*)
}

trait PermCompanion[P <: Perm] {
}


object Perm {
  def addCycle(p: Perm, c: Seq[Dom]) = p.apply(c:_*)
  def apply(n: Int): Perm = n match {
//    case 2 => Perm12
//    case x if x < byteMaxSize => new ByteArrayPerm((0 until n).map(_.toByte).toArray)
    case _ => new ShortArrayPerm((0 until n).map(_.toShort).toArray)
  }
  def fromCycles(n: Int, cycles: Seq[Seq[Dom]]): Perm = (Perm(n) /: cycles)( Perm.addCycle )
  def apply(images: DomArray): Perm = images.size match {
//    case 2 if images(0)._0 == 0 => Perm12
//    case 2 => Perm21
//    case x if x < byteMaxSize => new ByteArrayPerm(images.array.map(_.toByte))
    case _ => new ShortArrayPerm(images.array.map(_.toShort))
  }
  def fromImages(imgs: Dom*) = imgs.size match {
//    case 2 if imgs(0)._0 == 0 => Perm12
//    case 2 => Perm21
//    case x if x < byteMaxSize => new ByteArrayPerm(imgs.map(_._0.toByte).toArray)
    case _ => new ShortArrayPerm(imgs.map(_._0.toShort).toArray)
  }
}

abstract class GenericPerm extends Perm {
  def companion: MutablePerm
}

class MutablePerm private[alasc](val array: Array[Int]) extends GenericPerm {
  def this(newSize: Int) = this(new Array[Int](newSize))
  def make = new MutablePerm(size)
  def image(e: Dom): Dom = Dom._0(array(e._0))
  def image_=(e: Dom, i: Dom) {
    array(e._0) = i._0
  }
}

trait OptimizedPerm extends Perm {
  final def apply(cycle: Dom*): Perm = unoptimized.apply(cycle:_*).optimized
  final def withSwap(i: Dom, j: Dom): Perm = unoptimized.withSwap(i, j).optimized
}

class IdentityPerm(val size: Int) extends OptimizedPerm {

}
/*
object Perm12 extends Perm {
  final def size = 2
  final def image(k: Dom) = k
  final def *(that: Perm) = that
  final def inverse = this
  val cachedHashCode = scala.util.hashing.MurmurHash3.arrayHash(Array(0, 1))
  final override def hashCode() = cachedHashCode
  final def withSwap(i: Dom, j: Dom) = (if(i == j) this else Perm21)
  final def apply(cycle: Dom*): Perm = cycle.size match {
    case 1 => this
    case 2 => Perm21
    case _ => sys.error("Illegal cycle size")
  }
}

object Perm21 extends Perm {
  final def size = 2
  final def image(k: Dom) = Dom._0(1 - k._0)
  final def *(that: Perm) = {
    if (that eq Perm21)
      Perm12
    else
      Perm21
  }
  final def inverse = this
  val cachedHashCode = scala.util.hashing.MurmurHash3.arrayHash(Array(1, 0))
  final override def hashCode() = cachedHashCode
  final def withSwap(i: Dom, j: Dom) = (if(i == j) this else Perm12)
  final def apply(cycle: Dom*): Perm = cycle.size match {
    case 1 => this
    case 2 => Perm12
    case _ => sys.error("Illegal cycle size")
  }
}

object PermUnsafe {
// everything is zero-based, arrays are not cloned but must be non-modifiable
  def fromArray(array: Array[Short]): Perm = array.length match {
    case 1 => Perm(1)
    case 2 if array(0) == 0 => Perm12
    case 2 => Perm21
    case n if n <= Perm.byteMaxSize => fromArray(array.map(_.toByte))
    case _ =>
      new ShortArrayPerm(array)
  }
  def fromArray(array: Array[Byte]): Perm = array.length match {
    case 1 => Perm(1)
    case 2 if array(0) == 0 => Perm12
    case 2 => Perm21
    case n if n > Perm.byteMaxSize => 
      throw new IllegalArgumentException(s"Cannot have array of Byte greater than ${Perm.byteMaxSize}")
    case _ =>
      new ByteArrayPerm(array)
  }
  def toArray(p: ByteArrayPerm): Array[Byte] = p.arr
}
 */
sealed abstract class ArrayPerm[T] extends Perm {
  val array: Array[T]
  final def size = array.length
  final override def hashCode() = MurmurHash3.arrayHash(array)
}

final class ShortArrayPerm private[alasc](val array: Array[Short]) extends ArrayPerm[Short] with OptimizedPerm {
  final def image(k: Dom) = Dom._0(array(k._0))
  // note that the image of b under the product g*h is given by:
  // b^(g*h) = (b^g)^h
  final def *(perm: Perm): ShortArrayPerm = perm match {
    case that: ShortArrayPerm =>
      require_(compatible(that))
      val newArray = new Array[Short](size)
      var i = 0
      val n = size
      while (i < n) {
        newArray(i) = that.array(array(i))
          i += 1
      }
      new ShortArrayPerm(newArray)
    case _ => sys.error("")
  }
  final def inverse: Perm = {
    val newArray = new Array[Short](size)
    val n = size
    var i = 0
    while (i < n) {
      newArray(array(i)) = i.toShort
      i += 1
    }
    new ShortArrayPerm(newArray)
  }
}
/*
final class ByteArrayPerm private[alasc](val array: Array[Byte]) extends ArrayPerm {
  require_(arr.length > 2 && arr.length <= Perm.byteMaxSize)
  final override def hashCode() = scala.util.hashing.MurmurHash3.arrayHash(arr)
  final def size = arr.length
  final def image(k: Dom) = Dom._0(arr(k._0))
  // note that the image of b under the product g*h is given by:
  // b^(g*h) = (b^g)^h
  final def *(thatgen: Perm): Perm = {
    val that = thatgen.asInstanceOf[ByteArrayPerm]
    require_(compatible(that))
    val a = new Array[Byte](size)
    var i = 0
    val n = arr.length
    while (i < n) {
      a(i) = that.arr(arr(i))
      i += 1
    }
    new ByteArrayPerm(a)
  }
  final def inverse: Perm = {
    val a = new Array[Byte](size)
    val n = arr.length
    var i = 0
    while (i < n) {
      a(arr(i)) = i.toByte
      i += 1
    }
    new ByteArrayPerm(a)
  }
  final def withSwap(i: Dom, j: Dom): Perm = {
    require_(isDefinedAt(i) && isDefinedAt(j))
    val a = arr.clone
    val k = a(i._0)
    a(i._0) = a(j._0)
    a(j._0) = k
    new ByteArrayPerm(a)
  }
  final def apply(cycle: Dom*): Perm = {
    val a = arr.clone
    val a0 = a(cycle(0)._0)
    for (i <- 0 until cycle.size - 1)
      a(cycle(i)._0) = a(cycle(i + 1)._0)
    a(cycle(cycle.size-1)._0) = a0
    new ByteArrayPerm(a)
  }
}
*/
 */
