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

trait RepresentativesIterable[T, G] extends Representatives[T, G] with coll.Iterable[Representative[T, G]] {
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

//trait RepresentativesHead[T, G] extends Representatives[T, G] with coll.HasHead[Representative[T, G]]

/*
trait RepresentativesHeadIntArray[T, G, A] extends RepresentativesHead[T, G] {
  lazy val grpStart = grp.chain(representation, 
  def head: LexRepresentative[T, G] = {
    val minEl = Array.fill(tLength)(-1)
    def rec(chain: Chain[G], i: Int, gInv: G): Unit = chain match {
      case node: Node[G] =>
        var minimalIndex = -1
        val candOrbits = debox.Buffer.empty[Int]
        val it = node.orbit.iterator
        while (it.hasNext) {
          val b = it.next
          val betaImage = b <|+| gInv
          if (minimalIndex == -1 || compare(betaImage, minimalIndex) < 0) {
            minimalIndex = betaImage
            candOrbits.clear
            candOrbits += b
          } 
        }
        val c = if (minEl(i) == -1) -1 else compare(minimalIndex, minEl(i))
        if (c < 0)
          minEl(i) = minimalIndex
        if (c <= 0) {
          candOrbits.foreach { b =>
            val nextgInv = node.u(b) |+| gInv
            rec(node.next, i + 1, gInv,
          }
        }
      case _: Term[G] =>
    }
  }
}
 */
