package net.alasc

import scala.{ specialized => spec }
import spire.algebra.{Eq, Order}
import spire.macrosk.Ops
import spire.syntax.eq._
import scala.language.experimental.macros
import scala.language.implicitConversions
import indexSyntax._

trait Length[T] {
  /** Returns the length of t. */
  def indexLength(t: T): Int
}

/** Type class for objects of type T containing a sequence of objects of type A. */
trait Index[T, @spec(Int) A] extends Length[T] {
  /** Returns the i-th element of t if i is in range, or throws an exception. */
  def indexElement(t: T, i: Int): A
  def indexToIndexedSeq(t: T): IndexedSeq[A] = new IndexedSeq[A] {
    def length = indexLength(t)
    def apply(i: Int): A = indexElement(t, i)
  }
}

final class IndexOps[T](val lhs: T) extends AnyVal {
  def indexLength(implicit ev: Length[T]): Int =
    macro Ops.unopWithEv[Length[T], Int]
  def indexToIndexedSeq[@spec(Int) A](implicit ev: Index[T, A]): IndexedSeq[A] = 
    ev.indexToIndexedSeq(lhs)
  def indexElement[@spec(Int) A](rhs: Int)(implicit ev: Index[T, A]): A = 
    ev.indexElement(lhs, rhs)
}

class IndexLexicographicEq[T, @spec(Int) A](implicit val index: Index[T, A], implicit val eq: Eq[A])  extends Eq[T] {
  def eqv(x: T, y: T): Boolean = {
    var k = 0
    val length = x.indexLength
    require(y.indexLength == length)
    while (k < length) {
      if (x.indexElement(k) =!= y.indexElement(k))
        return false
      k += 1
    }
    true
  }
}

class IndexLexicographicOrder[T, @spec(Int) A](implicit val index: Index[T, A], implicit val order: Order[A]) {
  def compare(x: T, y: T): Int = {
    var k = 0
    val length = x.indexLength
    require(length == y.indexLength)
    while (k < length) {
      val c = order.compare(x.indexElement(k), y.indexElement(k))
      if (c != 0)
        return c
      k += 1
    }
    0
  }
}

class ArrayIndex[@spec(Int) A] extends Index[Array[A], A] {
  def indexElement(t: Array[A], i: Int) = t(i)
  def indexLength(t: Array[A]) = t.length
}

class IndexedSeqIndex[@spec(Int) A] extends Index[IndexedSeq[A], A] {
  def indexElement(t: IndexedSeq[A], i: Int) = t(i)
  def indexLength(t: IndexedSeq[A]) = t.length
}
