package net.alasc

trait GroupBSGSData[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSData {
    self: BSGSChain =>

    def beta: Dom = transversal.beta

    def iterator = new Iterator[BSGSChain] {
      private var current: BSGSChain = self
      def hasNext = !current.isTerminal
      def next = {
        if (hasNext) {
          val result = current
          current = result.tail
          result
        } else Iterator.empty.next
      }
    }

    final def transversals: List[Transversal[F]] = this.isTerminal match {
      case true => Nil
      case false => transversal :: tail.transversals
    }

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
      case false => if(transversal.size == 1) tail.order else transversal.size * tail.order
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
