package net.alasc

import scala.util.parsing.combinator._
import scala.util.Random

trait PermParserLike extends RegexParsers {
  def cycle = "(" ~> (repsep(oneBasedDom, ",") <~ ")")
  def oneBasedDom: Parser[Dom] = """\d+""".r ^^ { i => Dom._1(i.toInt) }
  def size: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def cycles(sz: Int) = rep(cycle) ^^ { cycles => Perm(sz, cycles) }
}

object Perm extends DumpableCompanion[Perm] {
  val parser = new DumpParser with PermParserLike {
    def dump = (("Perm(" ~> size) <~ ")") >> ( sz => cycles(sz) )
  }

  def addCycle(p: Perm, c: Seq[Dom]) = p.apply(c:_*)
  def apply(n: Int): Perm = new Perm((0 until n).toArray)
  def apply(n: Int, cycles: List[List[Dom]]): Perm = (Perm(n) /: cycles)( Perm.addCycle )
  def apply(images: DomArray): Perm = new Perm(images.array.asInstanceOf[Array[Int]])
  def fromImages(imgs: Dom*) = new Perm(imgs.map(_._0).toArray)
}

class Perm(val arr: Array[Int]) extends PermElement[Perm] with Dumpable {
  def toText = "Perm("+size+")"+cyclesToText
  override def toString = toText
  def cyclesToTextUsingSymbols(symbols: Seq[String]) = cycles.filter(_.length > 1).map(_.map( d => symbols(d._0) ).mkString("(",",",")")).mkString("")
  def cyclesToText = cycles.filter(_.length > 1).map(_.map(_._1).mkString("(",",",")")).mkString("")
  def isIdentity: Boolean = {
    val n = arr.length
    var i = 0
    while (i < n) {
      if (Dom._0(i) != image(Dom._0(i)))
        return false
      i += 1
    }
    return true
  }
  def size = arr.length
//  def toTeX = TeX("{}^"+arr.size)+TeX(cycles.filter(_.size>1).map(_.mkString("(",",",")")).mkString(""))
  def invImage(k: Dom): Dom = {
    var i = 0
    val n = arr.length
    while (i < n) {
      if (arr(i) == k._0)
        return Dom._0(i)
      i += 1
    }
    throw new IllegalArgumentException("Permutation should contain the image")
  }
  def image(k: Dom) = Dom._0(arr(k._0))
  def images: DomArray = DomArray.fromZeroBasedArray(arr)
  def compatible(that: Perm) = size == that.size
  def toExplicit = this
  // note that the image of b under the product g*h is given by:
  // b^(g*h) = (b^g)^h
  def *(that: Perm): Perm = {
    require_(compatible(that))
    val a = new Array[Int](size)
    var i = 0
    val n = arr.length
    while (i < n) {
      a(i) = that.arr(arr(i))
      i += 1
    }
    new Perm(a)
  }
  def inverse: Perm = {
    val a = new Array[Int](size)
    val n = arr.length
    var i = 0
    while (i < n) {
      a(arr(i)) = i
      i += 1
    }
    new Perm(a)
  }
  override def hashCode() = scala.util.hashing.MurmurHash3.seqHash(arr)
  def ===(that: Perm) = {
    require_(compatible(that))
    arr.sameElements(that.arr)
  }
  def withSwap(i: Dom, j: Dom): Perm = {
    require_(isDefinedAt(i) && isDefinedAt(j))
    val a = arr.clone
    val k = a(i._0)
    a(i._0) = a(j._0)
    a(j._0) = k
    new Perm(a)
  }
  def apply(s: String): Perm = {
    val points = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val list = s.map( c => Dom._0(points.indexOf(c)) )
    apply(list:_*)
  }
  def apply(cycle: Dom*): Perm = {
    val a = arr.clone
    val a0 = a(cycle(0)._0)
    for (i <- 0 until cycle.size - 1)
      a(cycle(i)._0) = a(cycle(i + 1)._0)
    a(cycle(cycle.size-1)._0) = a0
    new Perm(a)
  }
}
