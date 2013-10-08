package net.alasc

import scala.annotation.tailrec

trait BaseImageTest {
  def apply(baseImage: Dom): (Boolean, BaseImageTest)
}

object TrivialBaseImageTest extends BaseImageTest {
  def apply(baseImage: Dom) = (true, this)
}

trait GroupBSGSSearch[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSSearch {
    self: BSGSChain =>

    lazy val domainOrder = {
      assert(isImmutable)
      val a = Array.fill[Int](action.dimension)(-1)
      for ( (bel, i) <- base.zipWithIndex ) a(bel._0) = i
      var k = base.length
      for ( i <- 0 until action.dimension ) {
        if (a(i) == -1) {
          a(i) = k
          k += 1
        }
      }
      a
    }

    object DomainOrdering extends Ordering[Dom] {
      def compare(a: Dom, b: Dom) = Ordering.Int.compare(domainOrder(a._0), domainOrder(b._0))
    }

    object ElementOrdering extends Ordering[F] {
      def compare(a: F, b: F): Int = {
        for (bel <- base) {
          val ord = DomainOrdering.compare(action(a, bel), action(b, bel))
          if (ord != 0)
            return ord
        }
        0
      }
    }

    case class ImageOrdering(u: F) extends Ordering[Dom] {
      def compare(a: Dom, b: Dom) = DomainOrdering.compare(action(u, a), action(u, b))
    }

    /** Iterates through the elements of the represented group using the order
      * defined in Holt pp. 109-111
      */
    def orderedIterator(uPrev: F = identity): Iterator[F] = this match {
      case terminal: BSGSTerminal => Iterator(uPrev)
      case node: BSGSNode => for {
        b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
        uThis = transversal(b).u * uPrev
        u <- tail.orderedIterator(uThis)
      } yield u
    }

    def generalSearch(predicate: Predicate[F], test: BaseImageTest = TrivialBaseImageTest, uPrev: F = identity): Iterator[F] = this match {
      case terminal: BSGSTerminal => if (predicate(uPrev)) Iterator(uPrev) else Iterator.empty
      case node: BSGSNode => for {
        b <- transversal.keysIterator.toList.sorted(ImageOrdering(uPrev)).toIterator
        baseImage = action(uPrev, b)
        (takeIt, newTest) = test(baseImage) if takeIt
        uThis = transversal(b).u * uPrev
        u <- tail.generalSearch(predicate, newTest, uThis)
      } yield u
    }

    case class SubgroupSearchResult(val restartFrom: Int, val levelCompleted: Int) { }

    /** Recursive exploration of the elements of this group to build the subgroup.
      * 
      * @return The subgroup new generators and the level to restart the exploration from.
      */
    def subgroupSearchRec(predicate: Predicate[F], test: BaseImageTest, uPrev: F, level: Int, levelCompleted: Int, partialSubgroup: BSGSChain, startSubgroup: BSGSChain): SubgroupSearchResult = this match {
      case terminal: BSGSTerminal => {
        if (predicate(uPrev) && !uPrev.isIdentity) {
          startSubgroup.addStrongGeneratorsInChain(List(uPrev))
          return SubgroupSearchResult(levelCompleted - 1, levelCompleted)
        }
        return SubgroupSearchResult(level - 1, levelCompleted)
      }
      case node: BSGSNode => {
        var newLevelCompleted = levelCompleted
        val sortedOrbit = transversal.keysIterator.toList.sorted(ImageOrdering(uPrev))
        var sPrune = transversal.size
        for {
          deltaP <- sortedOrbit
          delta = action(uPrev, deltaP)
          (takeIt, newTest) = test(delta) if takeIt
          uThis = transversal(deltaP).u * uPrev
        } {
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

    def subgroupSearch(predicate: Predicate[F], test: BaseImageTest = TrivialBaseImageTest): BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode => {
        val cons = BSGSChain.mutableFromBaseAndGeneratingSet(base, Nil)
        val SubgroupSearchResult(restartFrom, levelCompleted) = subgroupSearchRec(predicate, test, identity, 0, length, cons, cons)
        assert(levelCompleted == 0)
        cons.cleanupGenerators
        cons.removeRedundantGenerators
        cons.makeImmutable
        cons
      }
    }

    def intersection(h: BSGSChain): BSGSChain = this match {
      case terminal: BSGSTerminal => terminal
      case node: BSGSNode => {
        val hWithBase = h.withBase(base)
        case class IntersectionTest(hSubgroup: BSGSChain, hPrev: F) extends BaseImageTest {
          def apply(baseImage: Dom): (Boolean, BaseImageTest) = {
            val b = action(hPrev.inverse, baseImage)
            if (!hSubgroup.transversal.isDefinedAt(b))
              return (false, null)
            val uh = hSubgroup.transversal(b).u
            return (true, IntersectionTest(hSubgroup.tail, uh * hPrev))
          }
        }
        subgroupSearch(hWithBase.contains(_), IntersectionTest(hWithBase, identity))
      }
    }
  }
}
