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

trait ImplIntArray[T] {
  def t: T

  /** Representation of `t` as an integer array, containing non-negative elements, such that
    * `tIntArray(i) == tIntArray(j)` iff `t(i) === t(j)`.
    */
  protected def tIntArray: Array[Int]
  /** Returns a representation of another sequence using the same mapping as `tIntArray`, i.e.
    * `asIntArray(r)(i) == tIntArray(j)` iff `r(i) === t(j)`.
    * 
    * If the sequence `r` contains elements not included in `t`, returns `RefNone`.
    */
  protected def asIntArray(r: T): RefOption[Array[Int]]
}

/** Specialization of `RepresentativesIntArray` such that integer representations of elements of `t`
  * reproduce the ordering of elements.
  */ 
trait ImplIntArrayOrdered[T] extends ImplIntArray[T] {
  protected def tIntArray: Array[Int]
  protected def asIntArray(r: T): RefOption[Array[Int]]
}

trait ImplIntArrayHashed[T, A] extends ImplIntArray[T] {
  implicit def sequenceTA: Sequence[T, A]

  val aMap = mutable.HashMap.empty[A, Int]
  val tLength = sequenceTA.length(t)
  val tIntArray = Array.tabulate(tLength) { i =>
    val newIndex = aMap.size
    aMap.getOrElseUpdate(sequenceTA.elemAt(t, i), newIndex)
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

  val partition = Partition.fromSeqHashCode(tIntArray)
}

trait ImplIntArrayOrderedFromOrder[T, A] extends ImplIntArray[T] {
  implicit def sequenceTA: Sequence[T, A]
  implicit def orderA: Order[A]
  var aMap = immutable.SortedMap.empty[A, Int](Order.ordering(orderA))

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
}
