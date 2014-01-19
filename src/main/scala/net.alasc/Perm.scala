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
  override def toString = "Perm("+size+")"+cyclesToText
  def apply(s: String): Perm = {
    val points = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val list = s.map( c => Dom._0(points.indexOf(c)) )
    apply(list:_*)
  }
  def apply(cycle: Dom*): Perm
  def withSwap(i: Dom, j: Dom): Perm
  def compatible(that: Perm) = size == that.size
  def toExplicit = this
  def ++(that: Perm): Perm = {
    val n = size
    Perm(DomArray._0(images._0 ++ that.images._0.map(_ + n)))
  }
}

object Perm12 extends Perm {
  final def isIdentity = true
  final def size = 2
  final def invImage(k: Dom) = k
  final def image(k: Dom) = k
  final def images = DomArray.fromZeroBasedArray(Array(0, 1))
  final def *(that: Perm) = that
  final def inverse = this
  val cachedHashCode = scala.util.hashing.MurmurHash3.arrayHash(Array(0, 1))
  final override def hashCode() = cachedHashCode
  final def ===(that: Perm) = this eq that
  final def withSwap(i: Dom, j: Dom) = Perm21
  final def apply(cycle: Dom*): Perm = cycle.size match {
    case 1 => this
    case 2 => Perm21
    case _ => sys.error("Illegal cycle size")
  }
}

object Perm21 extends Perm {
  final def isIdentity = false
  final def size = 2
  final def invImage(k: Dom) = Dom._0(1 - k._0)
  final def image(k: Dom) = Dom._0(1 - k._0)
  final def images = DomArray.fromZeroBasedArray(Array(1, 0))
  final def *(that: Perm) = {
    if (that eq Perm21)
      Perm12
    else
      Perm21
  }
  final def inverse = this
  val cachedHashCode = scala.util.hashing.MurmurHash3.arrayHash(Array(1, 0))
  final override def hashCode() = cachedHashCode
  final def ===(that: Perm) = this eq that
  final def withSwap(i: Dom, j: Dom) = Perm12
  final def apply(cycle: Dom*): Perm = cycle.size match {
    case 1 => this
    case 2 => Perm12
    case _ => sys.error("Illegal cycle size")
  }
}

object Perm {
  def addCycle(p: Perm, c: Seq[Dom]) = p.apply(c:_*)
  def apply(n: Int): Perm = n match {
    case 2 => Perm12
    case _ => new ArrayPerm((0 until n).map(_.toShort).toArray)
  }
  def apply(n: Int, cycles: List[List[Dom]]): Perm = (Perm(n) /: cycles)( Perm.addCycle )
  def apply(images: DomArray): Perm = images.size match {
    case 2 if images(0)._0 == 0 => Perm12
    case 2 => Perm21
    case _ => new ArrayPerm(images.array.map(_.toShort))
  }
  def fromImages(imgs: Dom*) = imgs.size match {
    case 2 if imgs(0)._0 == 0 => Perm12
    case 2 => Perm21
    case _ => new ArrayPerm(imgs.map(_._0.toShort).toArray)
  }
}

final class ArrayPerm private[alasc](val arr: Array[Short]) extends Perm {
  final def isIdentity: Boolean = {
    val n = arr.length
    var i = 0
    while (i < n) {
      if (Dom._0(i) != image(Dom._0(i)))
        return false
      i += 1
    }
    return true
  }
  final def size = arr.length
//  def toTeX = TeX("{}^"+arr.size)+TeX(cycles.filter(_.size>1).map(_.mkString("(",",",")")).mkString(""))
  final def invImage(k: Dom): Dom = {
    var i = 0
    val n = arr.length
    while (i < n) {
      if (arr(i) == k._0)
        return Dom._0(i)
      i += 1
    }
    throw new IllegalArgumentException("Permutation should contain the image")
  }
  final def image(k: Dom) = Dom._0(arr(k._0))
  final def images: DomArray = DomArray.fromZeroBasedArray(arr.map(_.toInt))
  // note that the image of b under the product g*h is given by:
  // b^(g*h) = (b^g)^h
  final def *(thatgen: Perm): Perm = {
    val that = thatgen.asInstanceOf[ArrayPerm]
    require_(compatible(that))
    val a = new Array[Short](size)
    var i = 0
    val n = arr.length
    while (i < n) {
      a(i) = that.arr(arr(i))
      i += 1
    }
    new ArrayPerm(a)
  }
  final def inverse: Perm = {
    val a = new Array[Short](size)
    val n = arr.length
    var i = 0
    while (i < n) {
      a(arr(i)) = i.toShort
      i += 1
    }
    new ArrayPerm(a)
  }
  final override def hashCode() = scala.util.hashing.MurmurHash3.arrayHash(arr)
  final def ===(that: Perm) = {
    require_(compatible(that))
    arr.sameElements(that.asInstanceOf[ArrayPerm].arr)
  }
  final def withSwap(i: Dom, j: Dom): Perm = {
    require_(isDefinedAt(i) && isDefinedAt(j))
    val a = arr.clone
    val k = a(i._0)
    a(i._0) = a(j._0)
    a(j._0) = k
    new ArrayPerm(a)
  }
  final def apply(cycle: Dom*): Perm = {
    val a = arr.clone
    val a0 = a(cycle(0)._0)
    for (i <- 0 until cycle.size - 1)
      a(cycle(i)._0) = a(cycle(i + 1)._0)
    a(cycle(cycle.size-1)._0) = a0
    new ArrayPerm(a)
  }
}
