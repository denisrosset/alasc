package net.alasc
package bsgs

trait BSGSLexicographic[E <: PermElement[E]] {
  self: BSGSGroup[E] =>

  def findSequenceMinimalRepresentative[T <: Ordered[T]](seq: IndexedSeq[T], symmetryGroup: BSGSGroup[E]): E = {
    assert(base == symmetryGroup.base)
    def innerSearch(subgroup: BSGSGroup[E], symmetrySubgroup: BSGSGroup[E], candidates: List[E]): E = {
      subgroup match {
        case _: BSGSGroupTerminal[E] => {
          assert(candidates.size == 1)
          candidates.head
        }
        case _: BSGSGroupNode[E] if subgroup.transversal.size == 1 => {
          val minimum = candidates.map(candidate => seq((subgroup.beta**candidate)._0)).min
          val newCandidates = candidates.filter(candidate => seq((subgroup.beta**candidate)._0) == minimum)
          innerSearch(subgroup.tail, symmetrySubgroup.tail, newCandidates)
        }
        case _: BSGSGroupNode[E] => {
          val minimum = (for {
            candidate <- candidates
            b <- subgroup.transversal.keysIterator
            betaimage = b**candidate
          } yield seq(betaimage._0)).min
          val newCandidates = scala.collection.mutable.HashSet.empty[E] ++ (for {
            candidate <- candidates
            (b, (u, uinv)) <- subgroup.transversal.iterator
            betaimage = b**candidate if seq(betaimage._0) == minimum
          } yield {
            val g = u*candidate
            symmetryGroup.rightCosetMinimalRepresentative(g.inverse).inverse
          })
          innerSearch(subgroup.tail, symmetrySubgroup.tail, newCandidates.toList) 
        }
      }
    }
    innerSearch(this, symmetryGroup, List(representedIdentity))
  }

  def rightCosetMinimalRepresentative(e: E): E = {
    implicit val ordering = DomainOrdering
    def innerSearch(subgroup: BSGSGroup[E], el: E): E = subgroup match {
      case _: BSGSGroupTerminal[E] => el
      case _: BSGSGroupNode[E] if subgroup.transversal.size == 1 =>
        innerSearch(subgroup.tail, el)
      case _: BSGSGroupNode[E] => {
        val minimumB = subgroup.transversal.keysIterator.minBy(b => (b**el))
        innerSearch(subgroup.tail, subgroup.transversal.u(minimumB) * el)
      }
    }
    innerSearch(this, e)
  }
}
