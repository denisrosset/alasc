package net.alasc

import scala.collection.IndexedSeqLike
import scala.collection.mutable.{Builder, ArrayBuffer}
import scala.collection.generic.CanBuildFrom

final class DomArray private[alasc] (val array: Array[Int]) extends IndexedSeq[Dom] with IndexedSeqLike[Dom, DomArray] {
  import DomArray._

  override protected[this] def newBuilder: Builder[Dom, DomArray] = DomArray.newBuilder

  def apply(i0: Int): Dom = Dom._0(array(i0))

  override def foreach[U](f: Dom => U): Unit =
    array.map(Dom._0(_)).foreach(f)

  def length = array.length
  def zeroBased = array
  def oneBased: Array[Int] = array.map(_+1)
}

object DomArray {
  def fromSeq(images: Seq[Dom]): DomArray = new DomArray(images.map(_._0).toArray)

  def apply(images: Dom*) = fromSeq(images)

  def newBuilder: Builder[Dom, DomArray] =
    new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[DomArray, Dom, DomArray] =
    new CanBuildFrom[DomArray, Dom, DomArray] {
      def apply(): Builder[Dom, DomArray] = newBuilder
      def apply(from: DomArray): Builder[Dom, DomArray] = newBuilder
    }

  def fromZeroBasedArray(arr: Array[Int]) = new DomArray(arr.clone)
  def fromOneBasedArray(arr: Array[Int]) = new DomArray(arr.map(_-1))

  def fromZeroBasedSeq(seq: Seq[Int]) = new DomArray(seq.toArray)
  def fromOneBasedSeq(seq: Seq[Int]) = new DomArray(seq.map(_-1).toArray)
}
