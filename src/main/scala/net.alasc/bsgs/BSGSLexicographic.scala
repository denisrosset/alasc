package net.alasc
package bsgs

trait BSGSLexicographic[E <: PermElement[E]] {
  self: BSGSGroup[E] =>
/*
  def sequenceRepresentative[T <: Ordered[T]](kth: BigInt, seq: IndexedSeq[T], symmetryGroup: BSGSGroup[E]): E = {
    assert(base == symmetryGroup.base)
    def innerSearch(k: BigInt, n: BigInt, subgroup: BSGSGroup[E], symmetrySubgroup: BSGSGroup[E], candidates: List[E]): E = {
      subgroup match {
        case _: BSGSGroupTerminal[E] => {
          assert(candidates.size == 1)
          candidates.head
        }
        case _: BSGSGroupNode[E] => {
          val withDupValueCount = scala.collection.mutable.Map.empty[T, Int]
          for (candidate <- candidates; b <- subgroup.transversal.keysIterator) {
            val imageOfBeta = b ** candidate
            val value = seq(imageOfBeta._0)
            withDupValueCount(value) = withDupValueCount.getOrElse(value, 0) + 1
          }
          assert(withDupValueCount.valuesIterator.forall(_ % symmetrySubgroup.tail.order == 0))
          val valueCount = withDupValueCount.mapValues(_ / symmetrySubgroup.tail.order)
          val sizeOfBlock = subgroup.tail.order / symmetrySubgroup.tail.order
          val number = valueCount.values.sum * sizeOfBlock
          println( (number, n) )
          assert(number == n)
          assert(k < valueCount.values.sum * sizeOfBlock)
          val indexOfBlock = k / sizeOfBlock
          val sortedValueCount = valueCount.toSeq.sortBy(_._1)
          val cumSum = sortedValueCount.map(_._2).scanLeft(BigInt(0))(_+_)
          val indexOfValue = cumSum.zipWithIndex.find(sm => sm._1 > indexOfBlock).get._2 - 1
          val valueToGet = sortedValueCount(indexOfValue)._1
          val newK = k - cumSum(indexOfValue)*sizeOfBlock
          val newN = sortedValueCount(indexOfValue)._2 * sizeOfBlock
          val newCandidates = Set.empty[E] ++ (for {
            candidate <- candidates
            (b, (u, uinv)) <- subgroup.transversal.iterator
            value = seq((b ** candidate)._0) if value == valueToGet
            g = u * candidate
            newCandidate = symmetryGroup.rightCosetMinimalRepresentative(g.inverse).inverse
          } yield newCandidate)
          innerSearch(newK, newN, subgroup.tail, symmetrySubgroup.tail, newCandidates.toList)
        }
      }
    }
    innerSearch(kth, order/symmetryGroup.order, this, symmetryGroup, List(representedIdentity))
  }*/

  def sequenceRepresentativeIterator[T <: Ordered[T]](seq: IndexedSeq[T], symmetryGroup: BSGSGroup[E]): Iterator[E] = {
    assert(base == symmetryGroup.base)
    def innerSearch(subgroup: BSGSGroup[E], symmetrySubgroup: BSGSGroup[E], candidates: List[E]): Iterator[E] = {
      subgroup match {
        case _: BSGSGroupTerminal[E] => {
          assert(candidates.size == 1)
          Iterator(candidates.head)
        }
        case _: BSGSGroupNode[E] => {
          val groupsForSequenceValues = (for {
            candidate <- candidates
            b <- subgroup.transversal.keysIterator
            betaimage = b**candidate
          } yield (seq(betaimage._0), b, candidate)).groupBy(_._1)
          for {
            value <- groupsForSequenceValues.keys.toList.sorted.toIterator
            newCandidates = Set.empty[E] ++ (for {
              (value, b, candidate) <- groupsForSequenceValues(value)
              g = subgroup.transversal.u(b)*candidate
            } yield symmetryGroup.rightCosetMinimalRepresentative(g.inverse).inverse)
            element <- innerSearch(subgroup.tail, symmetrySubgroup.tail, newCandidates.toList)
          } yield element
        }
      }
    }
    innerSearch(this, symmetryGroup, List(representedIdentity))
  }

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
