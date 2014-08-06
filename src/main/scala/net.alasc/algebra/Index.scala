package net.alasc.algebra

import scala.{ specialized => spec }

trait Index[T, @spec(Int) A] extends Length[T] {
  def element(t: T, i: Int): A
  def toIndexedSeq(t: T): IndexedSeq[A]
}

trait BigIndex[T, @spec(Int) A] extends BigLength[T] {
  def bigElement(t: T, i: BigInt): A
}
