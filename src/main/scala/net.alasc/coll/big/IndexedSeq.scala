package net.alasc.coll
package big

import scala.{ specialized => spec }

trait IndexedSeq[A] extends Any with Keyable[BigInt, A] with HasSize {
  def apply(idx: BigInt): A
  def length: BigInt
}

trait IndexedSeqImpl[A] extends Any with IndexedSeq[A] {
  self =>
  def stringPrefix : String = {
    var string = self.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }/*
  override def toString = stringPrefix + (0 until length.toInt).map(apply(_)).mkString("(", ",", ")") // TODO*/
  def size = BigIntSize(length)
}

object IndexedSeq {
  implicit class IndexedSeqMapped[A, B](val m: Mapped[IndexedSeq[A], A, B]) extends AnyVal with IndexedSeq[B] {
    @inline def apply(idx: BigInt) = m.f(m.t.apply(idx))
    @inline def length = m.t.length
    @inline def size = m.t.size
  }
}
