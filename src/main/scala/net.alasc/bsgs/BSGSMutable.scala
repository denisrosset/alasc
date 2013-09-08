package net.alasc
package bsgs

trait BSGSMutable[E <: PermElement[E]] {
  self: BSGSGroup[E] =>

  // Mutable/immutable toggle
  private[bsgs] def isImmutable: Boolean
  private[bsgs] def makeImmutable: Unit

  private[bsgs] def putInOrder: Boolean = this match {
    case g: BSGSGroupTerminal[E] => false
    case g: BSGSGroupNode[E] => {
      assert(!isImmutable)
      while(tail.putInOrder) { }
      for (b <- g.trv.keysIterator) {
        val ub = g.trv.u(b)
        for (x <- g.sg) { // TODO: test if generator is trivial with more clever transversals
          if (!g.trv.isDefinedAt(x.image(b)))
            g.trv = g.trv.updated(List(x), g.sg)
          val schreierGen = ub*x*g.trv.uinv(x.image(b))
          addElement(schreierGen).map( someH => {
            while(g.tail.putInOrder) { }
            addStrongGeneratorsHere(List(someH))
            return true
          } )
        }
      }
      false
    }
  }
  private[bsgs] def addStrongGeneratorsHere(l: List[E]) {
    this match {
      case g: BSGSGroupTerminal[E] => if(!l.isEmpty) throw new IllegalArgumentException("Cannot add strong generators to BSGS chain terminal.")
      case g: BSGSGroupNode[E] => {
        assert(!isImmutable)
        g.sg = l ++ g.sg
        g.trv = g.trv.updated(l, g.sg)
      }
    }
  }
  private[bsgs] def addStrongGeneratorsInChain(l: List[E]) {
    this match {
      case g: BSGSGroupTerminal[E] => if(!l.isEmpty) throw new IllegalArgumentException("Cannot add strong generators to BSGS chain terminal.")
      case g: BSGSGroupNode[E] => {
        assert(!isImmutable)
        addStrongGeneratorsHere(l)
        g.tail.addStrongGeneratorsInChain(l.filter(_.image(beta) == beta))
      }
    }
  }
  private[bsgs] def collectAllGenerators: Set[E] = {
    this match {
      case g: BSGSGroupTerminal[E] => Set.empty[E]
      case g: BSGSGroupNode[E] => g.tail.collectAllGenerators ++ g.sg
    }
  }
  private[bsgs] def replaceGenerators(gens: List[E]) {
    this match {
      case g: BSGSGroupTerminal[E] => { }
      case g: BSGSGroupNode[E] => {
        g.sg = gens
        g.tail.replaceGenerators(gens.filter(_.image(g.beta) == g.beta))
      }
    }
  }
  private[bsgs] def cleanupGenerators {
    replaceGenerators(collectAllGenerators.toList)
  }
  private[bsgs] def addElement(e: E): Option[E] = this match {
    case g: BSGSGroupTerminal[E] => throw new IllegalArgumentException("Cannot add element to BSGS chain terminal.")
    case g: BSGSGroupNode[E] => {
      assert(!isImmutable)
      val b = e.image(beta)
      if (!g.trv.isDefinedAt(b)) {
        addStrongGeneratorsHere(List(e))
        return Some(e)
      }
      val h = e * g.trv.uinv(b)
      assert(h.image(beta) == beta)
      if (g.tail.isTerminal) {
        if (h.isIdentity)
          return None
        val newBase = e.domain.find( k => h.image(k) != k ).get
        val newTrans = g.trv.builder.empty(newBase, g.id)
        g.tl = new BSGSGroupNode(newTrans, Nil, g.id, false, g.tl)
        addStrongGeneratorsInChain(List(h))
        return Some(h)
      } else
        g.tail.addElement(h).map(gen => {addStrongGeneratorsHere(List(gen)); gen})
    }
  }
  private[bsgs] def tryToRemoveGenerator: Option[E] = this match {
    case g: BSGSGroupTerminal[E] => None
    case g: BSGSGroupNode[E] =>  {
      assert(!isImmutable)
      val orbitSize = g.transversal.size
      val toRemove = g.tail.tryToRemoveGenerator.orElse {
        val candidatesToRemoval: List[E] = g.sg.filter(_.image(beta) != beta)
        def findCandidate: Option[E] = {
          for (h <- candidatesToRemoval) {
            val newGenerators: List[E] = g.sg.filterNot(_ == h)
            val newOrbitSize = OrbitSet.fromSet(beta, newGenerators).size
            if (newOrbitSize == orbitSize)
              return Some(h)
          }
          None
        }
        findCandidate
      }
      toRemove match {
        case Some(h) => {
          assert( OrbitSet.fromSet(beta, g.sg).size == transversal.size )
          g.sg = g.sg.filterNot(_ == h)
          if ( OrbitSet.fromSet(beta, g.sg).size != transversal.size ) {
            println(g.sg)
            println(h)
            println(transversal.keysIterator.toList)
            def printSGS(e: BSGSGroup[E]): Unit = e match {
              case term: BSGSGroupTerminal[E] => { }
              case node: BSGSGroupNode[E] => {
                println( ((node.beta, node.strongGeneratingSet)) )
                printSGS(node.tail)
              }
            }
            printSGS(this)
            assert(false)
          }
          Some(h)
        }
        case None => None
      }
    }
  }
  private[bsgs] def removeRedundantGenerators {
    this match {
      case g: BSGSGroupTerminal[E] => { }
      case g: BSGSGroupNode[E] => {
        // Straight-forward implementation of REMOVEGENS, section 4.4.4, p.95 of Holt.
        while(!tryToRemoveGenerator.isEmpty) { }
      }
    }
  }
}
