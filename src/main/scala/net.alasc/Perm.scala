package net.alasc

import scala.util.parsing.combinator._
import scala.util.Random

trait PermParserLike extends RegexParsers {
  def cycle = "(" ~> (repsep(oneBasedDom, ",") <~ ")")
  def oneBasedDom: Parser[Dom] = """\d+""".r ^^ { i => Dom._1(i.toInt) }
  def size: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def cycles(sz: Int) = rep(cycle) ^^ { cycles => Perm(sz, cycles) }
}

sealed abstract class Perm extends PermElement[Perm] {
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

object Perm {
  val byteMaxSize = 128 
  def addCycle(p: Perm, c: Seq[Dom]) = p.apply(c:_*)
  def apply(n: Int): Perm = n match {
    case 2 => Perm12
    case x if x < byteMaxSize => new ByteArrayPerm((0 until n).map(_.toByte).toArray)
    case _ => new ShortArrayPerm((0 until n).map(_.toShort).toArray)
  }
  def apply(n: Int, cycles: List[List[Dom]]): Perm = (Perm(n) /: cycles)( Perm.addCycle )
  def apply(images: DomArray): Perm = images.size match {
    case 2 if images(0)._0 == 0 => Perm12
    case 2 => Perm21
    case x if x < byteMaxSize => new ByteArrayPerm(images.array.map(_.toByte))
    case _ => new ShortArrayPerm(images.array.map(_.toShort))
  }
  def fromImages(imgs: Dom*) = imgs.size match {
    case 2 if imgs(0)._0 == 0 => Perm12
    case 2 => Perm21
    case x if x < byteMaxSize => new ByteArrayPerm(imgs.map(_._0.toByte).toArray)
    case _ => new ShortArrayPerm(imgs.map(_._0.toShort).toArray)
  }
}

final class ShortArrayPerm private[alasc](val arr: Array[Short]) extends Perm {
  require_(arr.length > Perm.byteMaxSize)
  final override def hashCode() = scala.util.hashing.MurmurHash3.arrayHash(arr)
  final def size = arr.length
  final def image(k: Dom) = Dom._0(arr(k._0))
  // note that the image of b under the product g*h is given by:
  // b^(g*h) = (b^g)^h
  final def *(thatgen: Perm): Perm = {
    val that = thatgen.asInstanceOf[ShortArrayPerm]
    require_(compatible(that))
    val a = new Array[Short](size)
    var i = 0
    val n = arr.length
    while (i < n) {
      a(i) = that.arr(arr(i))
      i += 1
    }
    new ShortArrayPerm(a)
  }
  final def inverse: Perm = {
    val a = new Array[Short](size)
    val n = arr.length
    var i = 0
    while (i < n) {
      a(arr(i)) = i.toShort
      i += 1
    }
    new ShortArrayPerm(a)
  }
  final def withSwap(i: Dom, j: Dom): Perm = {
    require_(isDefinedAt(i) && isDefinedAt(j))
    val a = arr.clone
    val k = a(i._0)
    a(i._0) = a(j._0)
    a(j._0) = k
    new ShortArrayPerm(a)
  }
  final def apply(cycle: Dom*): Perm = {
    val a = arr.clone
    val a0 = a(cycle(0)._0)
    for (i <- 0 until cycle.size - 1)
      a(cycle(i)._0) = a(cycle(i + 1)._0)
    a(cycle(cycle.size-1)._0) = a0
    new ShortArrayPerm(a)
  }
}

final class ByteArrayPerm private[alasc](val arr: Array[Byte]) extends Perm {
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
