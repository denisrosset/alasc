package net.alasc
package bsgs

trait BSGSCheck[E <: PermElement[E]] {
  self: BSGSGroup[E] =>

  /** Checks the consistency of the BSGS chain.
    * 
    * Throws an exception if an inconsistency is found.
    */
  def check = {
    assert(base.list.toSet.size == base.list.size)
    checkNode
    checkFixBase(Nil)
  }

  def checkFixBase(partialBase: List[Dom]): Unit = this match {
    case g: BSGSGroupNode[E] => {
      for ((u, uinv) <- transversal.valuesIterator; b <- partialBase)
        assert(b**u == b)
      for (g <- strongGeneratingSet; b <- partialBase)
        assert(b ** g == b)
      tail.checkFixBase(beta :: partialBase)
    }
    case _ => { }
  }

  def checkNode: Unit = this match {
    case g: BSGSGroupNode[E] => {
      tail.checkNode
      for ((b, (u, uinv)) <- transversal) {
        assert(beta**u == b)
        assert(b**uinv == beta)
      }
    }
    case _ => { }
  }
}
