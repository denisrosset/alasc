package net.alasc
package math
package enum

import scala.collection.immutable
import scala.collection.mutable

import spire.algebra.{Eq, Action, Order}
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.util._

import com.faacets.qalg.algebra.Vec

import bsgs._

trait SequencesHash[T, A, G] extends Representatives[T, G] {
  implicit def T: Vec[T, A]

  val aMap = mutable.HashMap.empty[A, Int]
  val tLength = T.length(t)
  val tIntArray = Array.tabulate(tLength) { i =>
    val newIndex = aMap.size
    aMap.getOrElseUpdate(T.apply(t, i), newIndex)
  }
  val maxInt = aMap.size - 1

  def tInt(idx: Int) = tIntArray(idx)

  def seqInt(seq: T, idx: Int): NNOption = aMap.get(T.apply(seq, idx)) match {
    case Some(i) => NNSome(i)
    case None => NNNone
  }
}

trait SequencesOrdered[T, A, G] extends RepresentativesOrdered[T, G] {
  implicit def T: Vec[T, A]
  implicit def orderA: Order[A]
  val tLength = T.length(t)
  protected def sortedSet = mutable.SortedSet.empty[A](Order.ordering(orderA)) ++ (0 until tLength).map(i => T.apply(t, i))
  val aMap = mutable.HashMap.empty[A, Int] ++ sortedSet.zipWithIndex
  val maxInt = aMap.size - 1
  val tIntArray = Array.tabulate(tLength) { i => aMap(T.apply(t, i)) }

  def tInt(idx: Int) = tIntArray(idx)

  def seqInt(seq: T, idx: Int): NNOption = aMap.get(T.apply(seq, idx)) match {
    case Some(i) => NNSome(i)
    case None => NNNone
  }
}

object SequencesOrdered {
  def computeIntegerArray[T, A](t: T)(implicit T: Vec[T, A], orderA: Order[A]): Array[Int] = {
    val tLength = T.length(t)
    val sortedSet = mutable.SortedSet.empty[A](Order.ordering(orderA)) ++ (0 until tLength).map(i => T.apply(t, i))
    val aMap = mutable.HashMap.empty[A, Int] ++ sortedSet.zipWithIndex
    Array.tabulate(tLength) { i => aMap(T.apply(t, i)) }
  }
}
