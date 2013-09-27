package net.alasc

import scala.annotation.tailrec

trait GroupBSGSData[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSData {
    self: BSGSChain =>

    def beta: Dom = transversal.beta
    def transversalSize = transversal.size

    final def base: List[Dom] = this.isTerminal match {
      case true => Nil
      case false => beta :: tail.base
    }

    final def length: Int = this.isTerminal match {
      case true => 0
      case false => tail.length + 1
    }

    final def order: BigInt = this.isTerminal match {
      case true => BigInt(1)
      case false => transversal.size * tail.order
    }

    final def makeImmutable: Unit = this match {
      case terminal: BSGSTerminal => { }
      case node: BSGSNode => {
        node.isImmutable = true
        tail.makeImmutable
      }
    }
  }
}
