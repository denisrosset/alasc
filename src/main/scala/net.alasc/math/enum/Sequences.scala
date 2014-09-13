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
  val tLength = sequenceTA.length(t)
  protected def sortedSet = mutable.SortedSet.empty[A](Order.ordering(orderA)) ++ (0 until tLength).map(i => sequenceTA.elemAt(t, i))
  val aMap = mutable.HashMap.empty[A, Int] ++ sortedSet.zipWithIndex
  val tIntArray = Array.tabulate(tLength) { i => aMap(sequenceTA.elemAt(t, i)) }

  def tInt(idx: Int) = tIntArray(idx)

  def seqInt(seq: T, idx: Int): NNOption = aMap.get(sequenceTA.elemAt(seq, idx)) match {
    case Some(i) => NNSome(i)
    case None => NNNone
  }
}
