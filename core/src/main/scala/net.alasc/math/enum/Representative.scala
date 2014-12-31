package net.alasc
package math
package enum

import spire.algebra.RightPartialAction
import spire.syntax.action._

trait Representative[T, G] {
  override def toString = s"Rep(${get.toString}, ${element.toString})"
  def get: T = original <|+| element
  def element: G
  protected def original: T
  implicit def actionTG: RightPartialAction[T, G]
}

trait LexRepresentative[T, G] extends Representative[T, G] {
  def rank: BigInt
  override def toString = s"Rep(${get.toString}, ${element.toString}, ${rank})"
}
