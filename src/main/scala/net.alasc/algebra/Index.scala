package net.alasc.algebra

import scala.{ specialized => spec }

trait Index[@spec(Int) A, T] extends Length[T] {
  def element(t: T, i: Int): A
  def toIndexedSeq(t: T): IndexedSeq[A]
}

trait BigIndex[@spec(Int) A, T] extends BigLength[T] {
  def bigElement(t: T, i: BigInt): A
}
