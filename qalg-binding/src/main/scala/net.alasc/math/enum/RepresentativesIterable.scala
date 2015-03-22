package net.alasc
package math
package enum

import scala.collection.mutable

import spire.algebra.{Eq, Order}
import spire.syntax.group._
import spire.syntax.action._

import net.alasc.algebra._
import net.alasc.util._

import bsgs._
import big._

trait RepresentativesIterableUnordered[T, G] extends Representatives[T, G] with BigIterable[Representative[T, G]] {
  self =>
  override def stringPrefix = "RepresentativesIterable"

  def size = grp.order / symGrp.order
  def iterator = (symGrp \ grp).iterator.map { coset => new Representative[T, G] {
    val element = coset.g
    val original = t
    implicit val actionTG = self.actionTG
  } }
}
