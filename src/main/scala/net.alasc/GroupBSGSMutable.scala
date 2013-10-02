package net.alasc

import scala.util.Random

trait GroupBSGSMutable[F <: FiniteElement[F]] {
  groupSelf: Group[F] =>

  trait BSGSMutableCompanion {
    def randomSchreierSims(base: List[Dom], randomElement: Random => F, knownOrder: BigInt): BSGSChain = {
      def findBaseElement: Dom = {
        val f = randomElement(options.randomGenerator)
        action.domain.find(b => action(f, b) != b).getOrElse(Dom._1(1))
      }
      val chain = mutableFromBase(if(base.isEmpty) List(findBaseElement) else base)
      while (chain.order < knownOrder)
        chain.addElement(randomElement(options.randomGenerator))
      chain.cleanupGenerators
      chain.removeRedundantGenerators
      chain.cleanupGenerators
      chain.makeImmutable
      chain
    }

    def deterministicSchreierSims(base: List[Dom], generators: List[F]) = {
      def findBaseElement: Dom = {
        if (generators.isEmpty)
          Dom._1(1)
        else
          action.domain.find(b => action(generators.head, b) != b).get
      }
      val chain = mutableFromBaseAndGeneratingSet(if(base.isEmpty) List(findBaseElement) else base, generators)
      while (chain.putInOrder) { }
      chain.cleanupGenerators
      chain.removeRedundantGenerators
      chain.cleanupGenerators
      chain.makeImmutable
      chain
    }
    def fromBaseAndGeneratingSet(base: List[Dom], genSet: List[F]) = {
      val chain = mutableFromBaseAndGeneratingSet(base, genSet)
      chain.makeImmutable
      chain
    }
    def mutableFromBaseAndGeneratingSet(base: List[Dom], genSet: List[F]): BSGSChain = base match {
      case Nil => new BSGSTerminal
      case beta :: tailBase => {
        val transversal = makeTransversal(beta, genSet)
        val tailGenSet = genSet.filter(g => action(g, beta) == beta)
        val tail = mutableFromBaseAndGeneratingSet(tailBase, tailGenSet)
        new BSGSNode(transversal, genSet, tail, false)
      }
    }
    def mutableFromBase(base: List[Dom]): BSGSChain = base match {
      case Nil => new BSGSTerminal
      case beta :: tailBase => {
        val transversal = makeTransversal(beta)
        val tail = mutableFromBase(tailBase)
        new BSGSNode(transversal, Nil, tail, false)
      }
    }
  }
  trait BSGSMutable {
    bsgsSelf: BSGSChain =>

    def removeRedundantGenerators {
      if (isTerminal)
        return
      val node = this.asInstanceOf[BSGSNode]
      // Straight-forward implementation of REMOVEGENS, section 4.4.4, p.95 of Holt.
      while(!tryToRemoveGenerator.isEmpty) { }
    }

    def tryToRemoveGenerator: Option[F] = this match {
      case terminal: BSGSTerminal => None
      case node: BSGSNode => {
        assert(!isImmutable)
        val orbitSize = transversal.size
        val toRemove = tail.tryToRemoveGenerator.orElse {
          val candidatesToRemoval: List[F] = strongGeneratingSet.filter(g => action(g, beta) != beta)
          def findCandidate: Option[F] = {
            for (h <- candidatesToRemoval) {
              val newGenerators: List[F] = strongGeneratingSet.filterNot(_ == h)
              val newOrbitSize = OrbitSet.fromSet(beta, action, newGenerators).size
              if (newOrbitSize == orbitSize)
                return Some(h)
            }
            None
          }
          findCandidate
        }
        toRemove match {
          case Some(h) => {
            assert( OrbitSet.fromSet(beta, action, strongGeneratingSet).size == transversal.size )
            node.strongGeneratingSet = strongGeneratingSet.filterNot(_ == h)
            assert( OrbitSet.fromSet(beta, action, strongGeneratingSet).size == transversal.size )
            Some(h)
          }
          case None => None
        }
      }
    }

    def putInOrder: Boolean = this match {
      case terminal: BSGSTerminal => false
      case node: BSGSNode => {
        assert(!isImmutable)
        while(tail.putInOrder) { }
        for (b <- transversal.keysIterator) {
          val ub = transversal(b).u
          for (x <- strongGeneratingSet) {
            // TODO: test if generator is trivial with more clever transversals
            if (!transversal.isDefinedAt(action(x, b)))
              node.transversal = node.transversal.updated(List(x), strongGeneratingSet)
            val schreierGen = ub*x*transversal(action(x, b)).uinv
            addElement(schreierGen) match {
              case None => { }
              case Some(someH) => {
                while(tail.putInOrder) { }
                addStrongGeneratorsHere(List(someH))
                return true
              }
            }
          }
        }
        false
      }
    }

    def addElement(f : F): Option[F] = {
      val node = this.asInstanceOf[BSGSNode]
      assert(!isImmutable)
      val b = action(f, beta)
      if (!transversal.isDefinedAt(b)) {
        addStrongGeneratorsHere(List(f))
        return Some(f)
      }
      val h = f * transversal(b).uinv
      assert(action(h, beta) == beta)
      if (tail.isTerminal) {
        if (h.isIdentity)
          return None
        val newBasePoint = action.domain.find( k => action(h, k) != k ).get
        val newTransversal = makeTransversal(newBasePoint)
        val newTail = new BSGSNode(newTransversal, Nil, tail, false)
        node.tail = newTail
        addStrongGeneratorsInChain(List(h))
        return Some(h)
      } else {
        tail.addElement(h) match {
          case None => None
          case Some(gen) => {
            addStrongGeneratorsHere(List(gen))
            Some(gen)
          }
        }
      }
    }

    def addStrongGeneratorsHere(newGenerators: List[F]) {
      this match {
        case terminal: BSGSTerminal => assert(newGenerators.isEmpty)
        case node: BSGSNode => {
          assert(!isImmutable)
          node.strongGeneratingSet = newGenerators ++ node.strongGeneratingSet
          node.transversal = node.transversal.updated(newGenerators, node.strongGeneratingSet)
        }
      }
    }

    def addStrongGeneratorsInChain(newGenerators: List[F]) {
      this match {
        case terminal: BSGSTerminal => assert(newGenerators.isEmpty)
        case node: BSGSNode => {
          assert(!isImmutable)
          addStrongGeneratorsHere(newGenerators)
          val tailGenerators = newGenerators.filter(g => action(g, beta) == beta)
          node.tail.addStrongGeneratorsInChain(tailGenerators)
        }
      }
    }

    def collectAllGenerators: Set[F] = this match {
      case terminal: BSGSTerminal => Set.empty[F]
      case node: BSGSNode => tail.collectAllGenerators ++ node.strongGeneratingSet
    }

    def buildStrongGeneratorsList(newStrongGenerators: List[F]): List[F] = this match {
      case terminal: BSGSTerminal => {
        assert(newStrongGenerators.isEmpty)
        List.empty[F]
      }
      case node: BSGSNode => {
        val (fixingBeta, notFixingBeta) =
          newStrongGenerators.partition(g => action(g, beta) == beta)
        val generatorsTail = tail.buildStrongGeneratorsList(fixingBeta)
        node.strongGeneratingSet = notFixingBeta ++ generatorsTail
        node.strongGeneratingSet
      }
    }

    def cleanupGenerators {
      if (!isTerminal)
        buildStrongGeneratorsList(collectAllGenerators.toList)
    }
  }
}
