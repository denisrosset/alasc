package net.alasc

import scala.annotation.tailrec

trait GroupBSGSElements[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSElements {
    self: Group[F]#BSGSChain =>

    def elements: Iterator[F] = this.isTerminal match {
      case true => Iterator(identity)
      case false => for {
        rest <- tail.elements
        b <- transversal.keysIterator
      } yield rest * transversal(b).u
    }

    def randomElement(gen: scala.util.Random): F = this.isTerminal match {
      case true => identity
      case false => tail.randomElement(gen) * transversal.randomElement(gen)
    }
  }
}
