package net.alasc
package math
package enum

import spire.algebra.partial.RightPartialAction
import spire.syntax.partialAction._

/** Representative of a permuted sequence. */
trait Representative[T, G] {
  implicit def actionTG: RightPartialAction[T, G]
  override def toString = s"Rep(${original} <|+| ${element} = ${get})"
  def get: T = (original <|+|? element).get
  def original: T
  def element: G
}

final class RepresentativeImpl[T, G](val original: T, val element: G)(implicit val actionTG: RightPartialAction[T, G]) extends Representative[T, G]

object Representative {
  def apply[T, G](original: T, element: G)(implicit actionTG: RightPartialAction[T, G]): Representative[T, G] = new RepresentativeImpl[T, G](original, element)
}
