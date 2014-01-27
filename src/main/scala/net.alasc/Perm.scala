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
trait Perm extends Permuting[Perm] {
  // TODO: override equals and hashCode
  /** Builder associated with the permutation implementation. */
  def builder: PermBuilder
  /** Returns a String representation of this permutation as a product of cycles. */
  def toString: String
  /** Returns the product of this permutation with a cycle. */
  def apply(cycle: Dom*): Perm
  /** Returns the product of this permutation with a cycle specified by a string. */
  def apply(s: String): Perm
  /** Returns this permutation as a specialized IndexPerm. */
  def toIndexPerm: IndexPerm
  /** Returns this permutation as a specialized CompactPerm. */
  def toCompactPerm: CompactPerm
  /** Returns this permutation as a specialized BytePerm. */
  def toBytePerm: BytePerm
  /** Returns this permutation as a specialized ShortPerm. */
  def toShortPerm: ShortPerm
  /** Returns this permutation as a specialized IntPerm. */
  def toIntPerm: IntPerm
  /** Returns this permutation as a GenericPerm (for test purposes). */ 
  def toGeneric: GenericPerm
  /** Returns the fastest specialization of this permutation. */
  def toOptimized: Perm
  /** Returns the concatenation of this permutation and another permutation.
    * 
    * For example Perm(3)(1,2) ++ Perm(2)(1,2) === Perm(5)(1,2)(4,5)
    */
  def ++(that: Perm): Perm
}

/** Specification of a factory for permutations. */
trait PermBuilder {
  /** Builds the identity permutation of given size.
    * 
    * @param  n    The size of the new permutation
    * 
    * @return the identity permutation of size n
    */
  def apply(n: Int): Perm
  /** Builds a permutation from images.
    * 
    * Constructs a permutation of given size from a 
    * function computing images.
    * 
    * @param  n    The size of the new permutation
    * @param  f    function returning for k the image of k
    *              f: k --> k^g
    * @return the new permutation built
    */
  def fromImages(n: Int)(f: Dom => Dom): Perm
  /** Builds a permutation from images.
    * 
    * Constructs a permutation from a sequence of images.
    * 
    * @param  seq  Sequence of images
    *
    * @return the new permutation built
    */
  def fromImages(seq: Seq[Dom]): Perm
  /** Builds a permutation from images.
    * 
    * Constructs a permutation from a sequence of images.
    * 
    * @param  seq  Sequence of images
    * @param  base Base of conversion (zero- or one-based)
    *
    * @return the new permutation built
    */
  def fromImages(seq: Seq[Int])(implicit base: Int => Dom): Perm
  /** Builds a permutation from preimages.
    * 
    * Constructs a permutation of given size from a
    * function computing preimages.
    * 
    * @param  n    The size of the new permutation
    * @param  f    function returning for k the image of k
    *              f: k --> k^(g^-1)
    * @return the new permutation built
    */
  def fromPreimages(n: Int)(f: Dom => Dom): Perm
  /** Builds a permutation from images.
    * 
    * Constructs a permutation from a sequence of preimages.
    * 
    * @param  seq  Sequence of preimages
    *
    * @return the new permutation built
    */
  def fromPreimages(seq: Seq[Dom]): Perm
  /** Builds a permutation from images.
    * 
    * Constructs a permutation from a sequence of preimages.
    * 
    * @param  seq  Sequence of preimages
    * @param  base Base of conversion (zero- or one-based)
    *
    * @return the new permutation built
    */
  def fromPreimages(seq: Seq[Int])(implicit base: Int => Dom): Perm
  /** Builds a permutation from cycles.
    * 
    * Constructs a permutation of given size from a
    * product of cycles.
    * 
    * @param  n       The size of the new permutation
    * @param  cycles  Sequence of cycles
    * 
    * @return the new permutation built
    */
  def fromCycles(n: Int, cycles: Seq[Dom]*): Perm
}

trait PermBuilderLike extends PermBuilder {
  def fromCycles(n: Int, cycles: Seq[Dom]*) = cycles.foldLeft(apply(n))(_.apply(_:_*))
  def fromImages(seq: Seq[Dom]) = {
    import Dom.ZeroBased._
    fromImages(seq.size)( seq(_) )
  }
  def fromImages(seq: Seq[Int])(implicit base: Int => Dom) = 
    fromImages(seq.size)( k => seq(k._0) )
  def fromPreimages(seq: Seq[Dom]) = {
    import Dom.ZeroBased._
    fromPreimages(seq.size)( seq(_) )
  }
  def fromPreimages(seq: Seq[Int])(implicit base: Int => Dom) =
    fromPreimages(seq.size)( k => seq(k._0) )
}

object Perm extends PermBuilder with PermBuilderLike {
  final val hashSeed = "Perm".hashCode
  def apply(n: Int) = fromImages(n)(k => k)
  def fromImages(n: Int)(f: Dom => Dom) = n match {
    case x if x <= IndexPerm.maxSize => IndexPerm.fromImages(n)(f)
    case x if x <= CompactPerm.maxSize => CompactPerm.fromImages(n)(f)
    case x if x <= BytePerm.maxSize => BytePerm.fromImages(n)(f)
    case x if x <= ShortPerm.maxSize => ShortPerm.fromImages(n)(f)
    case _ => IntPerm.fromImages(n)(f)
  }
  def fromPreimages(n: Int)(f: Dom => Dom) = n match {
    case x if x <= IndexPerm.maxSize => IndexPerm.fromPreimages(n)(f)
    case x if x <= CompactPerm.maxSize => CompactPerm.fromPreimages(n)(f)
    case x if x <= BytePerm.maxSize => BytePerm.fromPreimages(n)(f)
    case x if x <= ShortPerm.maxSize => ShortPerm.fromPreimages(n)(f)
    case _ => IntPerm.fromPreimages(n)(f)
  }
}

trait PermLike extends AnyRef with Perm with GenPermutingLike with FiniteLike[Perm] with PermutingLike[Perm] {
  override def toString = "Perm("+size+")"+this.cyclesToText
  
  def hashSpec = MurmurHash3.orderedHash(Dom.domain(size).map(i => image(i)._0), Perm.hashSeed)

  def hash = {
    var h = Perm.hashSeed
    val n = size
    var i = 0
    while (i < n) {
      h = MurmurHash3.mix(h, image(Dom._0(i))._0)
      i += 1
    }
    MurmurHash3.finalizeHash(h, n)
  }

  def apply(cycle: Dom*): Perm = {
    import Dom.ZeroBased._
    def rotateLeft[T](seq: Seq[T]) = cycle.tail :+ cycle.head
    val map = Map(cycle zip rotateLeft(cycle): _*)
    builder.fromImages(size)(k => image(map.getOrElse(k, k)))
  }

  def apply(s: String): Perm = {
    val points = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val list: Seq[Dom] = s.map( c => Dom._0(points.indexOf(c)) )
    apply(list:_*)
  }

  def toIndexPerm =
    IndexPerm.fromImages(size)(image)
  def toCompactPerm =
    CompactPerm.fromImages(size)(image)
  def toBytePerm =
    BytePerm.fromImages(size)(image)
  def toShortPerm =
    ShortPerm.fromImages(size)(image)
  def toIntPerm = IntPerm.fromImages(size)(image)
  def toGeneric = GenericPerm.fromImages(size)(image)
  def toOptimized = size match {
    case x if x <= IndexPerm.maxSize => toIndexPerm
    case x if x <= CompactPerm.maxSize => toCompactPerm
    case x if x <= BytePerm.maxSize => toBytePerm
    case x if x <= ShortPerm.maxSize => toShortPerm
    case _ => toIntPerm
  }
  def toPerm = toOptimized


  def dontForgetToOverrideHashCodeAndEquals = true // this trait extends AnyRef so that these will be used
  override def hashCode = hash
  override def equals(any: Any) = any match {
    case that: Perm => this === that
    case _ => false
  }

  def ++(that: Perm) = {
    val n = size
    Perm.fromImages(n + that.size) {
      case k if k._0 < n => image(k)
      case k => that.image(k + (-n)) + n
    }
  }
}

trait PermLikeExtended extends PermLike {
  def *(that: Perm) = builder.fromImages(size)(k => that.image(image(k)))
  def ===(that: Perm) = Dom.domain(size).forall( k => image(k) === that.image(k) )
  def isIdentity = Dom.domain(size).forall( k => k === image(k) )
  def inverse = builder.fromPreimages(size)(k => image(k))
}

/*
Implementation of `Perm` written for code clarity. Used in tests to check correctness of
the optimized implementations.
*/
class GenericPerm(images: Seq[Dom]) extends PermLike with PermLikeExtended {
  import Dom.ZeroBased._

  def builder = GenericPerm
  def size = images.size
  def image(k: Dom) = images(k)
}

object GenericPerm extends PermBuilder with PermBuilderLike {
  def fromImages(n: Int)(f: Dom => Dom) = 
    new GenericPerm(Dom.domain(n).map(f).toIndexedSeq)
  def fromPreimages(n: Int)(f: Dom => Dom) = {
    import Dom.ZeroBased._
    val images = scala.collection.mutable.ArrayBuffer.fill(n)(0)
    images.indices.foreach ( i => images(f(i)) = i )
    fromImages(n)(images(_))
  }
  def apply(n: Int) = fromImages(n)(k => k)
}

trait PermParserLike extends RegexParsers {
  def cycle = "(" ~> (repsep(oneBasedDom, ",") <~ ")")
  def oneBasedDom: Parser[Dom] = """\d+""".r ^^ { i => Dom._1(i.toInt) }
  def size: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def cycles(sz: Int) = rep(cycle) ^^ { cycles => Perm.fromCycles(sz, cycles:_*) }
}
