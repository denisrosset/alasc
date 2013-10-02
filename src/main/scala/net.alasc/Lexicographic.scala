package net.alasc

object Lexicographic {

  def sequenceRepresentativeIterator[F <: FiniteElement[F], T <% Ordered[T]](group: Group[F])(seq: IndexedSeq[T], providedSymmetrySubgroup: Option[group.Subgroup] = None): Iterator[F] = {
    val seqBase = (1 to seq.size).map(Dom._1(_)).toList
    val symmetryBSGS = providedSymmetrySubgroup.getOrElse(group.subgroup.fixing(seq)).subBSGS.withBase(seqBase)
    val groupBSGS = group.bsgs.withBase(seqBase)
    def innerSearch(groupChain: group.BSGSChain, symmetryChain: group.BSGSChain, candidates: List[F]): Iterator[F] = {
      groupChain match {
        case terminal: group.BSGSTerminal => {
          assert(candidates.size == 1)
          Iterator(candidates.head)
        }
        case node: group.BSGSNode => {
          val groupsForSequenceValues = (for {
            candidate <- candidates
            b <- groupChain.transversal.keysIterator
            betaimage = group.action(candidate, b)
          } yield (seq(betaimage._0), b, candidate)).groupBy(_._1)
          for {
            value <- groupsForSequenceValues.keys.toList.sorted.toIterator
            newCandidates = Set.empty[F] ++ (for {
              (value, b, candidate) <- groupsForSequenceValues(value)
              g = groupChain.transversal(b).u*candidate
            } yield rightCosetMinimalRepresentative(group)(new group.Subgroup(symmetryBSGS), g.inverse).inverse)
            element <- innerSearch(groupChain.tail, symmetryChain.tail, newCandidates.toList)
          } yield element
        }
      }
    }
    innerSearch(groupBSGS, symmetryBSGS, List(group.identity))
  }

  def findSequenceMinimalRepresentative[F <: FiniteElement[F], T <% Ordered[T]](group: Group[F])(seq: IndexedSeq[T], providedSymmetrySubgroup: Option[group.Subgroup] = None): F = {
    val seqBase = (1 to seq.size).map(Dom._1(_)).toList
    val symmetryBSGS = providedSymmetrySubgroup.getOrElse(group.subgroup.fixing(seq)).subBSGS.withBase(seqBase)
    val groupBSGS = group.bsgs.withBase(seqBase)
    def innerSearch(groupChain: group.BSGSChain, symmetryChain: group.BSGSChain, candidates: List[F]): F = {
      groupChain match {
        case terminal: group.BSGSTerminal => {
          println(candidates)
          assert(candidates.size == 1)
          candidates.head
        }
        case node: group.BSGSNode if groupChain.transversal.size == 1 => {
          val minimum = candidates.map(candidate => seq(group.action(candidate, groupChain.beta)._0)).min
          val newCandidates = candidates.filter(candidate => seq(group.action(candidate, groupChain.beta)._0) == minimum)
          innerSearch(groupChain.tail, symmetryChain.tail, newCandidates)
        }
        case node: group.BSGSNode => {
          val minimum = (for {
            candidate <- candidates
            b <- groupChain.transversal.keysIterator
            betaimage = group.action(candidate, b)
          } yield seq(betaimage._0)).min
          val newCandidates = scala.collection.mutable.HashSet.empty[F] ++ (for {
            candidate <- candidates
            b <- groupChain.transversal.keysIterator
            u = groupChain.transversal(b).u
            betaimage = group.action(candidate, b) if seq(betaimage._0) == minimum
          } yield {
            val g = u*candidate
            rightCosetMinimalRepresentative(group)(new group.Subgroup(symmetryBSGS), g.inverse).inverse
          })
          innerSearch(groupChain.tail, symmetryChain.tail, newCandidates.toList)
        }
      }
    }
    innerSearch(groupBSGS, symmetryBSGS, List(group.identity))
  }

  def rightCosetMinimalRepresentative[F <: FiniteElement[F]](g: Group[F])(subgroup: g.Subgroup, f: F): F = {
    implicit val ordering = subgroup.subBSGS.DomainOrdering
    def innerSearch(chain: g.BSGSChain, el: F): F = chain match {
      case terminal: g.BSGSTerminal => el
      case node: g.BSGSNode if node.transversal.size == 1 =>
        innerSearch(chain.tail, el)
      case node: g.BSGSNode => {
        val minimumB = chain.transversal.keysIterator.minBy(b => (g.action(el, b)))
        innerSearch(chain.tail, chain.transversal(minimumB).u * el)
      }
    }
    innerSearch(subgroup.subBSGS, f)
  }
}
