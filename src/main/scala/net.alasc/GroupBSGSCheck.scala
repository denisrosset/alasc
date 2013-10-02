package net.alasc

trait GroupBSGSCheck[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  def check = bsgs.check

  trait BSGSCheck {
    self: BSGSChain =>

    /** Checks the consistency of the BSGS chain.
      * 
      * Throws an exception if an inconsistency is found.
      */
    def check = {
      assert(base.toSet.size == base.size)
      checkFixBase(Nil)
      checkNode
    }

    def checkFixBase(partialBase: List[Dom]): Unit = this match {
      case node: BSGSNode => {
        for (WithInverse(u, uinv) <- transversal.valuesIterator; b <- partialBase)
          assert(action(u, b) == b)
        for (g <- strongGeneratingSet; b <- partialBase)
          assert(action(g, b) == b)
        tail.checkFixBase(beta :: partialBase)
      }
      case _ => { }
    }

    def checkNode: Unit = this match {
      case node: BSGSNode => {
        assert(strongGeneratingSet.toSet.size == strongGeneratingSet.size) // no duplicates
        assert(tail.strongGeneratingSet.toSet.subsetOf(strongGeneratingSet.toSet))
        for ((b, WithInverse(u, uinv)) <- transversal) {
          assert(action(u, beta) == b)
          assert(action(uinv, b) == beta)
        }
        val o = makeOrbit(beta, strongGeneratingSet)
        assert(o.orbit == transversal.orbit)
        tail.checkNode
      }
      case _ => { }
    }
  }
}
