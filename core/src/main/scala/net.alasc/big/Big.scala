package net.alasc.big

import spire.util.Opt

trait BigIterable[A] { self =>
  override def toString = s"$stringPrefix of size $size"
  def stringPrefix : String = {
    var string = self.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }

  def size: BigInt
  def iterator: Iterator[A]
}

trait BigIndexed[A] extends BigIterable[A] {
  def apply(idx: BigInt): A
}

trait BigSet[A] extends BigIterable[A] { self =>
  def contains(a: A): Boolean
}

trait BigIndexedSet[A] extends BigIndexed[A] with BigSet[A]

trait BigIndexedHead[A] extends BigIndexed[A] {
  def head: A
}
/*
trait BigIndexedSetFind[A] extends BigIndexedSetHead[A] {
  def find(value: A): Opt[BigInt]
}
 */
