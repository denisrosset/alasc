package net.alasc
package math
package enum

import scala.collection.mutable

import spire.algebra.{Eq, GroupAction, Order}
import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.util._

import bsgs._

trait RepresentativesIterableOrdered[T, G] extends RepresentativesOrdered[T, G] with coll.big.IndexedSet[Representative[T, G]] {
  self =>
  def stringPrefix = "RepresentativesIterable"

  def size = coll.BigIntSize(grp.order / symGrp.order)

  def foreach[U](f: Representative[T, G] => U): Unit = iterator.foreach(f)
}
