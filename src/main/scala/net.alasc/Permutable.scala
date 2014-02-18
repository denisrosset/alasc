package net.alasc

/*
 Trait for objects that can be permuted by the action of a finite group.

 These objects are represented by a sequence of T, and T is any type with an
 ordering `elementsOrdering`.
 F is the type of the finite group elements.
 */
object Permutable {
  val hashSeed = "Permutable".hashCode
}

trait Permutable[+P <: Permutable[P, F], F <: Finite[F]] {

  /* Returns the sequence of elements associated with the current object. */
  def integerSeq: IndexedSeq[Int]

  /* Returns the current object permuted by f. */
  def permutedBy(f: F): P

  trait Permutations {
    /* The base permutation group whose action acts on `elements`. */
    val baseGroup: Group[F]

    /*
     The permutation subgroup to use in the computations. 
     Default is `baseGroup` itself if not overridden.
     */
    def permutationSubgroup: baseGroup.Subgroup = baseGroup.subgroup

    def minimalRepresentative: P

    def minimalPermutation: F

    def minimal: (P, F)
  }
}

trait PermutableLike[P <: Permutable[P, F], F <: Finite[F]] {
  permutable: P =>

  trait PermutationsLike extends Permutations {
    import Dom.ZeroBased._
    import baseGroup.{Subgroup, identity, act, options}

    def permutationSubgroupBSGS = permutationSubgroup.subBSGS.withHeadBasePoint(Dom.first)
    
    def minimalRepresentative: P =
      permutedBy(minimalPermutation)

    def minimal: (P, F) = {
      val p = minimalPermutation
      (permutedBy(p), p)
    }

    def domainEnd = Dom.last(integerSeq.length)

    final case class PermutedBy(finv: F) extends Ordered[PermutedBy] {
      def permutation: F = finv.inverse
      def length = integerSeq.length
      def apply(k: Int) = integerSeq(act(finv, k))
      def get: P = permutedBy(finv.inverse)
      def compare(that: PermutedBy): Int = {
        var k = 0
        while (k < integerSeq.length) {
          val c = apply(k) - that.apply(k)
          if (c != 0)
            return c
          k += 1
        }
        0
      }
      override def hashCode: Int = {
        import scala.util.hashing.MurmurHash3
        var h = Permutable.hashSeed
        var k = 0
        while (k < integerSeq.length) {
          h = MurmurHash3.mix(h, apply(k))
          k += 1
        }
        MurmurHash3.finalizeHash(h, integerSeq.length)
      }
      override def equals(any: Any): Boolean = any match {
        case that: PermutedBy =>
          var k = 0
          while (k < integerSeq.length) {
            if (this(k) != that(k))
              return false
            k += 1
          }
          true
        case _ => false
      }
    }
  }

  trait BruteForcePermutations extends PermutationsLike {
    import Dom.ZeroBased._
    import baseGroup.{Subgroup, identity, act, options}
    import collection.mutable.{ HashMap => MutableHashMap, MultiMap, Set => MutableSet, ArrayBuffer, WeakHashMap }

    /* Finds the minimal lexicographic representative.

     Returns the minimal lexicographic representative and the permutation `p` such that
     `this.permutedBy(p) == this.firstLexicographicRepresentative._1`
     */

    def minimalPermutation: F =
      firstLexicographicRepresentativeRec(ArrayBuffer(identity), permutationSubgroupBSGS).inverse

    /*
     The candidate permutations `candidates` have been selected at the previous levels
     of the BSGS chain, and they will be filtered using the remaining subgroup given by
     `groupChain`.
     */
    class ShortBuffer(var array: Array[Short], var length: Int) extends IndexedSeq[Short] {
      def grow {
        val newArray = new Array[Short](length * 2)
        Array.copy(array, 0, newArray, 0, length)
        array = newArray
      }
      def +=(i: Short) {
        if (length == array.length)
          grow
        array(length) = i
        length += 1
      }
      def apply(ind: Int): Short = array(ind)
      def update(ind: Int, elem: Short) = {
        while (length >= array.length)
          grow
        array(ind) = elem
        if (ind >= length)
          length = ind + 1
      }
      def clear {
        length = 0
      }
    }

    object ShortBuffer {
      def empty = new ShortBuffer(new Array[Short](16), 0)
    }

    def firstLexicographicRepresentativeRec(candidates: ArrayBuffer[F], groupChain: BSGSChain[F]): F = {
      groupChain.isTerminal match {
        case true => candidates.head
        case false => {
          val beta = groupChain.beta
          var minimumValue = Int.MaxValue
          val keysArray = groupChain.transversal.keysIterator.map(_._0).toArray
          var possibleCandidatesF = ArrayBuffer.empty[F]
          var possibleCandidatesDom0 = ShortBuffer.empty
          var cind = 0
          val csize = candidates.size
          val ksize = keysArray.size
          while (cind < csize) {
            var kind = 0
            val c = candidates(cind)
            while (kind < ksize) {
              val b = Dom._0(keysArray(kind))
              val betaimage = act(c, b)
              val value = integerSeq(betaimage)
              if (value < minimumValue) {
                possibleCandidatesF.clear
                possibleCandidatesDom0.clear
                minimumValue = value
              }
              if (value == minimumValue) {
                possibleCandidatesF += c
                possibleCandidatesDom0 += b._0.toShort
              }
              kind += 1
            }
            cind += 1
          }

          val nextBeta = beta + 1

          if (groupChain.transversal.size == 1)
            firstLexicographicRepresentativeRec(possibleCandidatesF, groupChain.lexicographicTail)
          else {
            val newCandidates = ArrayBuffer.empty[F]
            var i = 0
            val n = possibleCandidatesF.size
            while (i < n) {
              val c = possibleCandidatesF(i)
              val b = Dom._0(possibleCandidatesDom0(i))
              if (b == beta)
                newCandidates += c
              else
                newCandidates += groupChain.transversal(b).u * c
              i += 1
            }
            firstLexicographicRepresentativeRec(newCandidates,
              groupChain.lexicographicTail)
          }
        }
      }
    }
  }

  trait WithoutSymmetrySubgroupPermutations extends PermutationsLike {
    import Dom.ZeroBased._
    import baseGroup.{Subgroup, identity, act, options}
    import collection.mutable.{ HashMap => MutableHashMap, MultiMap, Set => MutableSet, ArrayBuffer, WeakHashMap }

    /* Finds the minimal lexicographic representative.

     Returns the minimal lexicographic representative and the permutation `p` such that
     `this.permutedBy(p) == this.firstLexicographicRepresentative._1`
     */

    def minimalPermutation: F =
      firstLexicographicRepresentativeRec(Seq(PermutedBy(identity)), permutationSubgroupBSGS).inverse

    /*
     The candidate permutations `candidates` have been selected at the previous levels
     of the BSGS chain, and they will be filtered using the remaining subgroup given by
     `groupChain`.
     */

    def firstLexicographicRepresentativeRec(candidates: Seq[PermutedBy], groupChain: BSGSChain[F]): F = {
      groupChain.isTerminal match {
        case true =>
          assert(candidates.length == 1)
          candidates.head.finv
        case false => {
          val beta = groupChain.beta
          var minimumValue = Int.MaxValue
          val possibleCandidates = ArrayBuffer.empty[(PermutedBy, Dom)]
          for (b <- groupChain.transversal.keysIterator; c <- candidates) {
            val value = c(b)
            if (value < minimumValue) {
              possibleCandidates.clear
              minimumValue = value
            }
            if (value == minimumValue)
              possibleCandidates += (c -> b)
          }
          val nextBeta = beta + 1

          if (groupChain.transversal.size == 1)
            firstLexicographicRepresentativeRec(possibleCandidates.map(_._1),
              groupChain.lexicographicTail)
          else {
            val newCandidates = MutableSet.empty[PermutedBy]
            for ((c, b) <- possibleCandidates) {
              if (b == beta)
                newCandidates += c
              else
                newCandidates += PermutedBy(groupChain.transversal(b).u * c.finv)
            }
            firstLexicographicRepresentativeRec(newCandidates.toSeq,
              groupChain.lexicographicTail)
          }
        }
      }
    }
  }

    /* Big sequence of representatives in the lexicographic order. */
  trait BigSeqPermutations extends PermutationsLike with BigSeq[P] {
    representatives =>
    import Dom.ZeroBased._
    import baseGroup.{Subgroup, identity, act, options}
    import collection.mutable.{ HashMap => MutableHashMap, MultiMap, Set => MutableSet, ArrayBuffer, WeakHashMap }

    /* Returns the symmetry subgroup leaving `sequence` invariant. */
    def permutableSymmetrySubgroup: baseGroup.Subgroup =
      permutationSubgroup.fixing(integerSeq)

    /* Given an object `that`, finds a permutation p such that this.permutedBy(p) = `that`. */
    def findPermutationTo(that: P) = Block.start
      .blockForSequence(that.integerSeq).permutation.inverse

    /*
     The method `minimalPermutation` returns a group element `f` such that
     `this.permutedBy(f) == apply(0)`.
     */
    def minimalPermutation: F =
      findSequenceMinimalRepresentativeSubgroup(ArrayBuffer(identity),
        permutationSubgroupBSGS, symmetryBSGS).inverse

    def symmetryBSGS = permutableSymmetrySubgroup.subBSGS

    /* Returns the minimal lexicographic representative. */
    def head = minimalRepresentative

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
    def length = permutationSubgroup.order / permutableSymmetrySubgroup.order

    /* Finds the index of the representative `that`. */
    def indexOf(that: P = permutable) = Block.start.indexOfSequence(that.integerSeq)

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
    @annotation.tailrec protected final def findSequenceMinimalRepresentativeSubgroup(candidates: ArrayBuffer[F], groupChain: BSGSChain[F], symChain: BSGSChain[F], permutedByInverseOf: Option[F] = None, candidatesAreMinimal: Boolean = false): F = {
      val beta = groupChain.beta

      def permutedSequence(k: Dom): Int = permutedByInverseOf match {
        case None => integerSeq(k._0)
        case Some(f) => integerSeq(act(f, k)._0)
      }

      def toMinimal(f: F) = symChain.rightCosetMinimalRepresentativeUsingBSGSBase(f.inverse).inverse

      (beta == domainEnd) match {
        case true => {
          val minimalCandidates = candidates.map(toMinimal).toSet
          assert(minimalCandidates.size == 1)
          minimalCandidates.head
        }
        case false => {
          var minimumValue = Int.MaxValue
          val keysArray = groupChain.transversal.keysIterator.map(_._0).toArray
          var possibleCandidatesF = ArrayBuffer.empty[F]
          var possibleCandidatesDom0 = ArrayBuffer.empty[Int]
          var cind = 0
          val csize = candidates.size
          val ksize = keysArray.size
          while (cind < csize) {
            var kind = 0
            val c = candidates(cind)
            while (kind < ksize) {
              val b = Dom._0(keysArray(kind))
              val betaimage = act(c, b)
              val value = permutedSequence(betaimage)
              if (value < minimumValue) {
                possibleCandidatesF.clear
                possibleCandidatesDom0.clear
                minimumValue = value
              }
              if (value == minimumValue) {
                possibleCandidatesF += c
                possibleCandidatesDom0 += b
              }
              kind += 1
            }
            candidates(cind) = candidates(0)
            cind += 1
          }

          val nextBeta = beta + 1

          if (groupChain.transversal.size == 1 && candidatesAreMinimal)
            findSequenceMinimalRepresentativeSubgroup(possibleCandidatesF, groupChain.lexicographicTail, symChain, permutedByInverseOf, true)
          else {
            val newCandidates = MutableSet.empty[F]
            var i = 0
            val n = possibleCandidatesF.size
            while (i < n) {
              val c = possibleCandidatesF(i)
              val b = Dom._0(possibleCandidatesDom0(i))
              if (b === beta && candidatesAreMinimal)
                newCandidates += c
              else
                newCandidates += toMinimal(groupChain.transversal(b).u * c)
              possibleCandidatesF(i) = possibleCandidatesF(0)
              i += 1
            }
            findSequenceMinimalRepresentativeSubgroup(ArrayBuffer.empty[F] ++ newCandidates, groupChain.lexicographicTail, symChain, permutedByInverseOf, true)
          }
        }
      }
    }

    object Block {
      def start = Block(permutationSubgroupBSGS, Seq( (identity, symmetryBSGS) ))
    }

    /*
     Block of representatives defined by the k-th first sequence values.
     
     The block is generated by the permutations given in `candidates` and
     the elements of the remanining BSGS chain given in `chain`.
     
     Each candidate has an associated symmetry subgroup valid restricted to the base
     points of `chain`. 
     */

    case class Block(chain: BSGSChain[F], candidates: Seq[(F, BSGSChain[F])]) {
      lazy val chainTail = chain.lexicographicTail
      /* New possible candidates grouped by the permuted sequence value at `chain`.beta. */
      protected lazy val candidatesForValue = {
        val map = new MutableHashMap[Int, MutableSet[((F, BSGSChain[F]), Dom)]] with MultiMap[Int, ((F, BSGSChain[F]), Dom)]
        for (cSym <- candidates; b <- chain.transversal.keysIterator) {
          val betaImage = act(cSym._1, b)
          val value = integerSeq(betaImage._0)
          map.addBinding(value, ((cSym, b)))
        }
        map
      }

      /* Number of representatives in this block. */
      lazy val size: BigInt = {
        val co = chain.order
        candidates.map( cSym => co / cSym._2.order ).sum
      }

      /* Sorted possible values for the permuted sequence at `chain`.beta. */
      lazy val sortedValues = candidatesForValue.keys.toSeq.sorted

      /* When the block is trivial, returns the associated permutation. */
      def permutation = {
        assert(candidates.size == 1)
        candidates.head._1
      }

      /* Return the block where all permuted sequences have value `value` at `chain`.beta. */
      def blockForValue(value: Int) = {
        val newCandidates = new MutableHashMap[F, BSGSChain[F]]
        if (chain.transversal.size == 1)
          for (((c, sym), b) <- candidatesForValue(value))
            newCandidates(c) = sym.withHeadBasePoint(chain.beta).tail
        else
          for (((c, sym), b) <- candidatesForValue(value)) {
            val u = chain.transversal(b).u
            val newU = findSequenceMinimalRepresentativeSubgroup(ArrayBuffer(u), chainTail, sym, Some(c))
            val newC = newU * c
            if (!newCandidates.isDefinedAt(newC)) {
              val newSym = sym.conjugatedBy(newU.inverse).withHeadBasePoint(chain.beta).tail
              newCandidates(newC) = newSym
            }
          }
        Block(chainTail, newCandidates.toSeq)
      }

      /* Return the index of the first representative of the block corresponding to
       the permuted sequence value `value` at `chain`.beta. */
      def indexOfValue(value: Int) =
        sortedValues.view.filter(_ < value).map(blockForValue(_).size).sum

      /* Returns the block corresponding to the sequence `seq`. */
      @annotation.tailrec final def blockForSequence(seq: IndexedSeq[Int] = integerSeq): Block = {
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
      @annotation.tailrec final def indexOfSequence(seq: IndexedSeq[Int] = integerSeq, currentIndex: BigInt = 0): BigInt = {
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
      @annotation.tailrec final def blockForIndex(index: BigInt): Block = {
        val (newBlock, newIndex) = partialBlockForIndex(index)
        if (newIndex == 0 && newBlock.size == 1)
          newBlock
        else newBlock.blockForIndex(newIndex)
      }
    }
  }
}
