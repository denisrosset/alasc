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

trait RepresentativesIterableUnordered[T, G] extends Representatives[T, G] with coll.Iterable[Representative[T, G]] {
  self =>
  def stringPrefix = "RepresentativesIterable"

  def size = coll.BigIntSize(grp.order / symGrp.order)
  def iterator = (symGrp \ grp).iterator.map { coset => new Representative[T, G] {
    val element = coset.g
    val original = t
    implicit val actionTG = self.actionTG
  } }
  def foreach[U](f: Representative[T, G] => U): Unit = iterator.foreach(f)
}
