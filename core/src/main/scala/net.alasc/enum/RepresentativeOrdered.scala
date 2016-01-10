package net.alasc.enum

import spire.algebra.partial.RightPartialAction
import spire.syntax.partialAction._

/** Ordered representative of a permuted sequence. */
trait RepresentativeOrdered[T, G] extends Representative[T, G] {

  def rank: BigInt

}

final class RepresentativeOrderedImpl[T, G](val original: T, val element: G, val rank: BigInt)(implicit val action: RightPartialAction[T, G]) extends RepresentativeOrdered[T, G]

object RepresentativeOrdered {

  def apply[T, G](original: T, element: G, rank: BigInt)(implicit action: RightPartialAction[T, G]): RepresentativeOrdered[T, G] = new RepresentativeOrderedImpl[T, G](original, element, rank)

}
