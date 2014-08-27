package net.alasc.algebra

import scala.{ specialized => spec }

trait Sequence[T, @spec(Int) A] {
  def elemAt(t: T, i: Int): A
  def length(t: T): Int
  def toIndexedSeq(t: T): IndexedSeq[A]
}
