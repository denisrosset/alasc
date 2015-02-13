package net.alasc
package math
package enum

import spire.algebra.partial.RightPartialAction
import spire.syntax.partialAction._

trait Representative[T, G] {
  implicit def actionTG: RightPartialAction[T, G]
  override def toString = s"Rep(${get.toString}, ${element.toString})"
  def get: T = (original <|+|? element).get
  def element: G
  protected def original: T
}

trait LexRepresentative[T, G] extends Representative[T, G] {
  def rank: BigInt
  override def toString = s"Rep(${get.toString}, ${element.toString}, ${rank})"
}
