package net.alasc.bsgs

import net.alasc.algebra.PermutationAction
import net.alasc.perms.{Perm, PermAlgebra}

/** Result of a sifting operation. */
sealed abstract class SiftResult[G, A <: PermutationAction[G] with Singleton]

object SiftResult {

  /** Represents the result of a completely sifted element equal to the identity. */
  final class Id[G, A <: PermutationAction[G] with Singleton] private[SiftResult]() extends SiftResult[G, A]

  object Id {

    private[this] val instance: Id[Perm, PermAlgebra.type] = new Id[Perm, PermAlgebra.type]

    def apply[G, A <: PermutationAction[G] with Singleton] = instance.asInstanceOf[Id[G, A]]

  }

  /** Represents the result of a completely sifted element not equal to the identity, thus
    * part of the kb of the action. */
  case class NotId[G, A <: PermutationAction[G] with Singleton](remainder: G) extends SiftResult[G, A]

  /** Represents the result of an incomplete sift, with
    * implicitly[A].movesAnyPoint(remainder) true. The returned mutableNode `stop` is where
    * the remainder should be inserted as a strong generator.
    */
  case class Stop[G, A <:PermutationAction[G] with Singleton](remainder: G, node: MutableNode[G, A]) extends SiftResult[G, A]

}
