package net.alasc

trait OrderedPermutable[P <: OrderedPermutable[P, F, T], F <: FiniteElement[F], T] extends Permutable[P, F, T] with Ordered[P] {
  orderedPermutable: P =>
  def compare(that: P): Int = {
    for(i <- permutableSequence.indices) {
      val compareValue = permutableOrdering.compare(permutableSequence(i),
        that.permutableSequence(i))
      if (compareValue != 0)
        return compareValue
    }
    0
  }
}

/*
 Trait for objects that can be permuted by the action of a finite group.

 These objects are represented by a sequence of T, and T is any type with an
 ordering `permutableOrdering`.
 F is the type of the finite group elements.
 */
trait Permutable[P <: Permutable[P, F, T], F <: FiniteElement[F], T] {
  permutable: P =>


  val permutableOrdering: Ordering[T]
  /* Returns the current object permuted by f. */
  def permutedBy(f: F): P
  /* Returns the sequence associated with the current object. */
  def permutableSequence: IndexedSeq[T]
  /* The permutation group whose action acts on `sequence`. */
  val permutableGroup: Group[F]
  /* Returns the symmetry subgroup leaving `sequence` invariant. */
  def permutableSymmetrySubgroup: permutableGroup.Subgroup = permutableGroup.subgroup.fixing(permutableSequence)
  /* Given an object `that`, finds a permutation p such that this.permutedBy(p) = `that`. */
  def findPermutationTo(that: P) = representatives.Block.start.blockForSequence(that.permutableSequence).permutation.inverse

  def domainEnd = Dom._1(permutableSequence.size)

  /* Big sequence of representatives in the lexicographic order. */
  object representatives extends BigSeq[P] {
    import collection.mutable.{ HashMap => MutableHashMap, MultiMap, Set => MutableSet, ArrayBuffer, WeakHashMap }
    import Dom.IntOrder.DomOrdering
    
    def groupBSGSStart = permutableGroup.bsgs.withHeadBasePoint(Dom._0(0))
    def symmetryBSGS = permutableSymmetrySubgroup.subBSGS

    /* Returns the minimal lexicographic representative. */
    def head = permutedBy(
      findSequenceMinimalRepresentativeSubgroup(Iterable(permutableGroup.identity), groupBSGSStart, symmetryBSGS).inverse)

    /* Returns an iterator on the lexicographic representatives. */
    def iterator: Iterator[P] = {
      def iteratorSearch(block: Block): Iterator[F] = {
        if (block.size == 1) {
          val Seq((f, chain)) = block.candidates
          Iterator(f)
        } else {
          for {
            value <- block.sortedValues.toIterator;
            f <- iteratorSearch(block.blockForValue(value))
          } yield f
        }
      }
      iteratorSearch(Block.start).map(f => permutedBy(f.inverse))
    }

    /* Returns the number of lexicographic representatives. */
    def length = permutableGroup.order / permutableSymmetrySubgroup.order

    /* Finds the index of the representative `that`. */
    def indexOf(that: P = permutable) = Block.start.indexOfSequence(that.permutableSequence)

    /* Finds the lexicographic representative with index `index`. */
    def apply(index: BigInt): P = {
      if (index < 0 || index >= length)
        throw new IllegalArgumentException("Representative index should be between 0 and " + (size - 1))
      permutedBy(Block.start.blockForIndex(index).permutation.inverse)
    }

    /* Finds the minimal lexicographic representative.

     The candidate permutations `candidates` have been selected at the previous levels
     of the BSGS chain, and they will be filtered using the remaining subgroup given by
     `groupChain`. The redundancy given by the symmetry group `symChain` will be removed
     by looking at minimal coset representatives in symChain.

     The main `sequence` can be implicitly permuted by the argument `permutedByInverseOf`.

     The argument `candidatesAreMinimal` is used internally to avoid looking twice for
     minimal coset representatives when the current transversal is trivial (size = 1).
     */
    @annotation.tailrec protected final def findSequenceMinimalRepresentativeSubgroup(candidates: Iterable[F], groupChain: permutableGroup.BSGSChain, symChain: permutableGroup.BSGSChain, permutedByInverseOf: Option[F] = None, candidatesAreMinimal: Boolean = false): F = {
      import Dom.IntOrder._

      val beta = groupChain.beta

      def permutedSequence(k: Dom): T = permutedByInverseOf match {
        case None => permutableSequence(k._0)
        case Some(f) => permutableSequence(permutableGroup.act(f, k)._0)
      }

      def toMinimal(f: F) = symChain.rightCosetMinimalRepresentativeUsingBSGSBase(f.inverse).inverse

      (beta == domainEnd) match {
        case true => {
          assert(candidates.size == 1)
          if (!candidatesAreMinimal)
            toMinimal(candidates.head)
          else
            candidates.head
        }
        case false => {
          var first = true
          var minimumValue: T = permutableSequence(0)
          var possibleCandidates = ArrayBuffer.empty[(F, Dom)]

          for (c <- candidates; b <- groupChain.transversal.keysIterator) {
            val betaimage = permutableGroup.act(c, b)
            val value = permutedSequence(betaimage)
            if (first || permutableOrdering.compare(value, minimumValue) == -1) {
              possibleCandidates.clear
              minimumValue = value
              first = false
            }
            if (value == minimumValue)
              possibleCandidates += ((c, b))
          }

          val nextBeta = Dom._0(beta._0 + 1)

          if (groupChain.transversal.size == 1 && candidatesAreMinimal)
            findSequenceMinimalRepresentativeSubgroup(possibleCandidates.map(_._1), groupChain.lexicographicTail, symChain, permutedByInverseOf, true)
          else {
            val newCandidates = MutableSet.empty[F] ++ (for ((c, b) <- possibleCandidates) yield {
              if (b == beta && candidatesAreMinimal)
                c else toMinimal(groupChain.transversal(b).u * c)
            })

            findSequenceMinimalRepresentativeSubgroup(newCandidates, groupChain.lexicographicTail, symChain, permutedByInverseOf, true)
          }
        }
      }
    }

    object Block {
      def start = Block(groupBSGSStart, Seq( (permutableGroup.identity, symmetryBSGS) ))
    }

    /*
     Block of representatives defined by the k-th first sequence values.
     
     The block is generated by the permutations given in `candidates` and
     the elements of the remanining BSGS chain given in `chain`.
     
     Each candidate has an associated symmetry subgroup valid restricted to the base
     points of `chain`. 
     */

    case class Block(chain: permutableGroup.BSGSChain, candidates: Seq[(F, permutableGroup.BSGSChain)]) {
      /* New possible candidates grouped by the permuted sequence value at `chain`.beta. */
      protected lazy val candidatesForValue = {
        val map = new MutableHashMap[T, MutableSet[((F, permutableGroup.BSGSChain), Dom)]] with MultiMap[T, ((F, permutableGroup.BSGSChain), Dom)]
        for (cSym <- candidates; b <- chain.transversal.keysIterator) {
          val betaImage = permutableGroup.act(cSym._1, b)
          val value = permutableSequence(betaImage._0)
          map.addBinding(value, ((cSym, b)))
        }
        map
      }

      /* Cache of children blocks. */
      protected val blocksForValue = new WeakHashMap[T, Block]

      /* Number of representatives in this block. */
      lazy val size: BigInt = {
        val co = chain.order
        candidates.map( cSym => co / cSym._2.order ).sum
      }

      /* Sorted possible values for the permuted sequence at `chain`.beta. */
      lazy val sortedValues = candidatesForValue.keys.toSeq.sorted(permutableOrdering)

      /* When the block is trivial, returns the associated permutation. */
      def permutation = {
        assert(size == 1)
        candidates.head._1
      }

      /* Return the block where all permuted sequences have value `value` at `chain`.beta. */
      def blockForValue(value: T) = {
        if (!blocksForValue.isDefinedAt(value)) {
          val newCandidates = new MutableHashMap[F, permutableGroup.BSGSChain]
          if (chain.transversal.size == 1)
            for (((c, sym), b) <- candidatesForValue(value))
              newCandidates(c) = sym.withHeadBasePoint(chain.beta).tail
          else
            for (((c, sym), b) <- candidatesForValue(value)) {
              val u = chain.transversal(b).u
              val newU = findSequenceMinimalRepresentativeSubgroup(Iterable(u), chain.lexicographicTail, sym, Some(c))
              val newC = newU * c
              if (!newCandidates.isDefinedAt(newC)) {
                val newSym = sym.conjugatedBy(newU.inverse).withHeadBasePoint(chain.beta).tail
                newCandidates(newC) = newSym
              }
            }
          val newBlock = Block(chain.lexicographicTail, newCandidates.toSeq)
          blocksForValue(value) = newBlock
        }
        blocksForValue(value)
      }

      /* Return the index of the first representative of the block corresponding to
       the permuted sequence value `value` at `chain`.beta. */
      def indexOfValue(value: T) =
        sortedValues.view.filter(permutableOrdering.compare(_, value) == -1).map(blockForValue(_).size).sum

      /* Returns the block corresponding to the sequence `seq`. */
      def blockForSequence(seq: IndexedSeq[T] = permutableSequence): Block = {
        if (chain.isTerminal)
          this
        else {
          val value = seq(chain.beta._0)
          if (!candidatesForValue.isDefinedAt(value))
            throw new IllegalArgumentException("Given sequence " + seq + " cannot be permuted into the current sequence.")
          blockForValue(value).blockForSequence(seq)
        }
      }

      /* Returns the index of the representative with sequence `seq`. */
      def indexOfSequence(seq: IndexedSeq[T] = permutableSequence, currentIndex: BigInt = 0): BigInt = {
        if (chain.isTerminal)
          currentIndex
        else {
          val value = seq(chain.beta._0)
          if (!candidatesForValue.isDefinedAt(value))
            throw new IllegalArgumentException("Given sequence " + seq + " cannot be permuted into the current sequence.")
          blockForValue(value).indexOfSequence(seq, currentIndex + indexOfValue(value))
        }
      }

      /* Returns the children block corresponding to the partial index `index`.
       Also returns the child subindex. */
      def partialBlockForIndex(index: BigInt): (Block, BigInt) = {
        assert(0 <= index && index < size)
        var remainingIndex = index
        for (value <- sortedValues) {
          val block = blockForValue(value)
          if (block.size > remainingIndex)
            return (block, remainingIndex)
          remainingIndex -= block.size
        }
        sys.error("Consistency error in block.")
      }

      import scala.annotation.tailrec

      /* Returns the final block corresponding to partial index `index`. */
      @tailrec final def blockForIndex(index: BigInt): Block = {
        val (newBlock, newIndex) = partialBlockForIndex(index)
        if (newIndex == 0 && newBlock.size == 1)
          newBlock
        else newBlock.blockForIndex(newIndex)
      }
    }
  }
}
