package net.alasc
package math
package enum

import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.{Eq, Action, Order}
import spire.syntax.group._
import spire.syntax.action._

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
  val maxInt = aMap.size - 1

  def tInt(idx: Int) = tIntArray(idx)

  def seqInt(seq: T, idx: Int): NNOption = aMap.get(sequenceTA.elemAt(seq, idx)) match {
    case Some(i) => NNSome(i)
    case None => NNNone
  }
}

trait SequencesOrdered[T, A, G] extends RepresentativesOrdered[T, G] {
  implicit def sequenceTA: Sequence[T, A]
  implicit def orderA: Order[A]
  val tLength = sequenceTA.length(t)
  protected def sortedSet = mutable.SortedSet.empty[A](Order.ordering(orderA)) ++ (0 until tLength).map(i => sequenceTA.elemAt(t, i))
  val aMap = mutable.HashMap.empty[A, Int] ++ sortedSet.zipWithIndex
  val maxInt = aMap.size - 1
  val tIntArray = Array.tabulate(tLength) { i => aMap(sequenceTA.elemAt(t, i)) }

  def tInt(idx: Int) = tIntArray(idx)

  def seqInt(seq: T, idx: Int): NNOption = aMap.get(sequenceTA.elemAt(seq, idx)) match {
    case Some(i) => NNSome(i)
    case None => NNNone
  }
}

object SequencesOrdered {
  def computeIntegerArray[T, A](t: T)(implicit sequenceTA: Sequence[T, A], orderA: Order[A]): Array[Int] = {
    val tLength = sequenceTA.length(t)
    val sortedSet = mutable.SortedSet.empty[A](Order.ordering(orderA)) ++ (0 until tLength).map(i => sequenceTA.elemAt(t, i))
    val aMap = mutable.HashMap.empty[A, Int] ++ sortedSet.zipWithIndex
    Array.tabulate(tLength) { i => aMap(sequenceTA.elemAt(t, i)) }
  }
}
