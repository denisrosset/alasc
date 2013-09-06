package net.alasc
package bsgs

trait BSGSSearch[E <: PermElement[E]] {
  self: BSGSGroup[E] =>

  // Search
  def generalSearch(predicate: Predicate[E], test: BaseImageTest = TrivialBaseImageTest, uPrev:E = representedIdentity): Iterator[E] = this match {
    case g: BSGSGroupTerminal[E] => if (predicate(uPrev)) Iterator(uPrev) else Iterator.empty
    case g: BSGSGroupNode[E] => for {
      b <- g.transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
      baseImage = uPrev.image(b)
      (takeIt, newTest) = test(baseImage) if takeIt
      uThis = g.transversal.u(b) * uPrev
      u <- g.tail.generalSearch(predicate, newTest, uThis)
    } yield u
  }

  def subgroupSearch(predicate: Predicate[E], test: BaseImageTest = TrivialBaseImageTest): BSGSGroup[E] = this match {
    case g: BSGSGroupTerminal[E] => g
    case g: BSGSGroupNode[E] => {
      val cons = BSGS.mutableFromBaseAndGeneratingSet(g.base, Nil, g.id, g.trv.builder)
      val SubgroupSearchResult(restartFrom, levelCompleted) = subgroupSearchRec(predicate, test, g.id, 0, g.length, cons, cons)
      assert(levelCompleted == 0)
      cons.removeRedundantGenerators
      cons.makeImmutable
      cons
    }
  }

  def intersection(h: BSGSGroup[E]): BSGSGroup[E] = this match {
    case g: BSGSGroupTerminal[E] => g
    case g: BSGSGroupNode[E] => {
      case class IntersectionTest(hSubgroup: BSGSGroup[E], hPrev: E) extends BaseImageTest {
        def apply(baseImage: Dom): (Boolean, BaseImageTest) = {
          val b = hPrev.invImage(baseImage)
          if (!hSubgroup.transversal.isDefinedAt(b))
            return (false, null)
          val uh = hSubgroup.transversal.u(b)
          return (true, IntersectionTest(hSubgroup.tail, uh * hPrev))
        }
      }
      subgroupSearch(h.contains(_), IntersectionTest(h, g.representedIdentity))
    }
  }

  private[bsgs] case class SubgroupSearchResult(val restartFrom: Int, val levelCompleted: Int) { }
  /** Recursive exploration of the elements of this group to build the subgroup.
    * 
    * @return The subgroup new generators and the level to restart the exploration from.
    */
  private[bsgs] def subgroupSearchRec(predicate: Predicate[E], test: BaseImageTest, uPrev: E, level: Int, levelCompleted: Int, partialSubgroup: BSGSGroup[E], startSubgroup: BSGSGroup[E]): SubgroupSearchResult = this match {
    case g: BSGSGroupTerminal[E] => {
      if (predicate(uPrev) && !uPrev.isIdentity) {
        startSubgroup.addStrongGeneratorsInChain(List(uPrev))
        return SubgroupSearchResult(levelCompleted - 1, levelCompleted)
      }
      return SubgroupSearchResult(level - 1, levelCompleted)
    }
    case g: BSGSGroupNode[E] => {
      var newLevelCompleted = levelCompleted
      val sortedOrbit = transversal.keysIterator.toList.sorted(ImageOrdering(uPrev))
      var sPrune = g.trv.size
      for (
        deltaP <- sortedOrbit;
        delta = uPrev.image(deltaP);
        (takeIt, newTest) = test(delta) if takeIt;
        uThis = transversal.u(deltaP) * uPrev
      ) {
        if (sPrune < partialSubgroup.transversal.size)
          return SubgroupSearchResult(level - 1, level)
        val SubgroupSearchResult(subRestartFrom, subLevelCompleted) =
          tail.subgroupSearchRec(predicate, newTest, uThis, level + 1, newLevelCompleted, partialSubgroup.tail, startSubgroup)
        newLevelCompleted = subLevelCompleted
        if (subRestartFrom < level)
          return SubgroupSearchResult(subRestartFrom, newLevelCompleted)
        sPrune -= 1
      }
      SubgroupSearchResult(level - 1, level)
    }
  }
}
