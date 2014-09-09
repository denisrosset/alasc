package net.alasc
package math
package enum

import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.{Eq, GroupAction, Order}
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.util._

import bsgs._

trait SequencesHash[T, A, G] extends Representatives[T, G] {
  implicit def sequenceTA: Sequence[T, A]

  val aMap = mutable.HashMap.empty[A, Int]
  val tLength = sequenceTA.length(t)
  val tIntArray = Array.tabulate(tLength) { i =>
    val newIndex = aMap.size
    aMap.getOrElseUpdate(sequenceTA.elemAt(t, i), newIndex)
  }

  def tInt(idx: Int) = tIntArray(idx)

  def seqInt(seq: T, idx: Int): NNOption = aMap.get(sequenceTA.elemAt(seq, idx)) match {
    case Some(i) => NNSome(i)
    case None => NNNone
  }
}

trait SequencesOrdered[T, A, G] extends RepresentativesOrdered[T, G] {
  implicit def sequenceTA: Sequence[T, A]
  implicit def orderA: Order[A]
  var aMap = immutable.SortedMap.empty[A, Int](Order.ordering(orderA))
/*
  val tLength = sequenceTA.length(t)
  val tIntArray = Array.tabulate(tLength) { i =>
    val a = sequenceTA.elemAt(t, i)
    aMap.get(a) match {
      case Some(index) => index
      case None =>
        val newIndex = aMap.size
        aMap += (a -> newIndex)
        newIndex
    }
  }

  def asIntArray(r: T): RefOption[Array[Int]] = {
    require(sequenceTA.length(r) == tLength)
    val array = new Array[Int](tLength)
    var i = 0
    while (i < tLength) {
      aMap.get(sequenceTA.elemAt(r, i)) match {
        case Some(index) => array(i) = index
        case None => return RefNone
      }
      i += 1
    }
    RefSome(array)
  }
 */
}
