package net.alasc.perms.orbits

import scala.annotation.tailrec

import spire.algebra._
import spire.math.SafeLong
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.group._
import spire.util.Opt

import metal._
import metal.mutable.Buffer
import metal.syntax._

import net.alasc.algebra._
import net.alasc.bsgs.{BaseChange, BaseGuideLex, BaseOrder, BaseSwap, BuildChain, Chain, ChainRec, GrpChain, Node, SchreierSims, SubgroupSearch, Term}
import net.alasc.perms.{MutableOrbit, orbits}
import net.alasc.util._

final case class Representatives[G, F <: PermutationAction[G] with Singleton]
  (val seq: Array[Int], val grp: GrpChain[G, F], val symGrp: GrpChain[G, F])
  (implicit baseChange: BaseChange, baseSwap: BaseSwap, schreierSims: SchreierSims) {

  import grp.{action, equ, group}

  import Representatives.toUnsignedLong

  val n = seq.length

  val lexChain = if (grp.chain.hasLexicographicBase) grp.chain else
    BuildChain.fromChain[G, F, F](grp.chain, Opt(BaseGuideLex(n)))

  val arrayMaxInt = {
    var res = 0
    cforRange(0 until n) { i =>
      require(seq(i) >= 0)
      if (seq(i) > res) res = seq(i)
    }
    res
  }

  def size: SafeLong = grp.order / symGrp.order

  val intBits = if (arrayMaxInt == 0) 1 else (32 - java.lang.Integer.numberOfLeadingZeros(arrayMaxInt))
  val maxSkip = 64 / intBits

  def apply(idx: SafeLong): TermBlock = {
    @tailrec def rec(block: Block): TermBlock = block match {
      case nb: NodeBlock => nb.blockForIndex(idx) match {
        case Opt(nextBlock) => rec(nextBlock)
        case _ => throw new IndexOutOfBoundsException
      }
      case tb: TermBlock => tb
    }
    rec(Block.start)
  }

  def orderedIterator: Iterator[TermBlock] = {
    def iteratorSearch(block: Block): Iterator[TermBlock] = block match {
      case nb: NodeBlock => for {
        block <- nb.children
        repr <- iteratorSearch(block)
      } yield repr
      case tb: TermBlock => Iterator(tb)
    }
    iteratorSearch(Block.start)
  }

  def find(other: Array[Int]): Opt[TermBlock] = {
    @tailrec def rec(block: Block): Opt[TermBlock] = block match {
      case nb: NodeBlock => nb.blockForArray(other) match {
        case Opt(nextBlock) => rec(nextBlock)
        case _ => Opt.empty[TermBlock]
      }
      case tb: TermBlock => Opt(tb)
    }
    rec(Block.start)
  }

  abstract class Block {
    def chain: Chain[G, F]
    def size: SafeLong
    def index: SafeLong
    def images: Long
    def level: Int
  }

  object Block {
    def start = lexChain match {
      case node: Node[G, F] => NodeBlock(0, node, 0L, SafeLong.zero, Buffer(Group[G].id), metal.mutable.Buffer(symGrp))
      case term: Term[G, F] => TermBlock(0, term, 0L, SafeLong.zero, Group[G].id)
    }
  }

  case class TermBlock(level: Int, chain: Term[G, F], images: Long, index: SafeLong, gInverse: G) extends Block {
    def size = SafeLong.one
    def element = gInverse.inverse
  }

  case class NodeBlock(level: Int, chain: Node[G, F], images: Long, index: SafeLong, candidates: Buffer[G], symGrps: Buffer[GrpChain[G, F]]) extends Block {

    case class NextCandidate(b: Int, c: Int, tail: Opt[NextCandidate] = Opt.empty[NextCandidate])

    val beta = chain.beta

    val chainNextBeta = chain.next match {
      case nextNode: Node[G, F] => nextNode.beta
      case _ => n
    }
    assert(beta < chainNextBeta)
    val nextBeta = chainNextBeta.min(beta + maxSkip)

    protected lazy val candidatesForImages: metal.mutable.Map[Long, NextCandidate] = {
      val map = metal.mutable.HashMap.empty[Long, NextCandidate]
      var c = 0
      val n = candidates.length
      while (c < n) {
        val g = candidates(c)
        val it = chain.orbit.iterator
        while (it.hasNext) {
          val b = it.next
          var images: Long = toUnsignedLong(seq(b <|+| g))
          var k = beta + 1
          while (k < nextBeta) {
            val u = chain.u(b)
            images = (images << intBits) + toUnsignedLong(seq((k <|+| u) <|+| g))
            k += 1
          }
          val nextCandidate = NextCandidate(b, c, Opt(map.getOrElse(images, null)))
          map.update(images, nextCandidate)
        }
        c += 1
      }
      map
    }

    protected lazy val sortedImages: Array[Long] = {
      val res = new Array[Long](candidatesForImages.longSize.toInt)
      @tailrec def rec(ptr: Ptr[candidatesForImages.type], i: Int): Unit = ptr match {
        case IsVPtr(vp) =>
          res(i) = vp.key
          rec(vp.next, i + 1)
        case _ =>
      }
      rec(candidatesForImages.ptr, 0)
      spire.math.Sorting.quickSort(res)(UnsignedLongOrder, implicitly)
      res
    }

    lazy val size: SafeLong = {
      val co = ChainRec.order(chain: Chain[G, F], SafeLong.one)

      symGrps.foldLeft(SafeLong.zero) { case (sm, symGrp) => sm + co / symGrp.order }
    }

    def blockForIndex(idx: SafeLong): Opt[Block] = {
      val it = children
      while (it.hasNext) {
        val block = it.next
        if (block.index <= idx && idx < block.index + block.size)
          return Opt(block)
      }
      Opt.empty[Block]
    }

    def blockForArray(array: Array[Int]): Opt[Block] = {
      var seqImages = 0L
      var k = beta
      while (k < nextBeta) {
        val a = array(k)
        require(a >= 0)
        if (a > arrayMaxInt) return Opt.empty[Block]
        seqImages = (seqImages << intBits) + toUnsignedLong(a)
        k += 1
      }
      if (candidatesForImages.contains(seqImages)) {
        val it = children
        while (it.hasNext) {
          val block = it.next
          if (block.images == seqImages)
            return Opt(block)
        }
      }
      Opt.empty[Block]
    }


    def children: Iterator[Block] = new ChildrenIterator

    class ChildrenIterator extends Iterator[Block] {
      var i = 0
      var blockIndex = index

      def hasNext = i < sortedImages.length

      def next: Block = {
        val images = sortedImages(i)
        val newBlockCandidates = metal.mutable.Buffer.empty[G]
        val newBlockSymGrps = metal.mutable.Buffer.empty[GrpChain[G, F]]
        var it = Opt(candidatesForImages(images.toLong))
        val sOrbit = MutableOrbit.forSize(n)
        while (it.nonEmpty) {
          val NextCandidate(b, c, next) = it.get
          val u = chain.u(b)
          val g = candidates(c)
          val bg = b <|+| g
          if (orbits.Points.isSmallestInOrbit(bg, symGrps(c).generators, Opt(sOrbit))(implicitly, spire.std.int.IntAlgebra)) {
            val nextSym = GrpChain.stabilizer(symGrps(c), bg)
            newBlockCandidates += u |+| g
            newBlockSymGrps += nextSym
          }
          it = next
        }
        val block = chain.next match {
          case nextNode: Node[G, F] if nextNode.beta == nextBeta =>
            NodeBlock(level + 1, nextNode, images, blockIndex, newBlockCandidates, newBlockSymGrps)
          case nextNode: Node[G, F] =>
            assert(nextNode.beta < nextBeta)
            val nNode = Node.trivial(nextBeta, nextNode)
            NodeBlock(level + 1, nNode, images, blockIndex, newBlockCandidates, newBlockSymGrps)
          case termNode: Term[G, F] =>
            assert(newBlockCandidates.length == 1)
            TermBlock(level + 1, termNode, images, blockIndex, newBlockCandidates(0))
        }
        blockIndex += block.size
        i += 1
        block
      }
    }

  }

}

object Representatives {

  @inline def toUnsignedLong(i: Int): Long = i & 0xFFFFFFFFL

  /** Returns the permutation `g` in `chainGrp` such that `seq <|+| g == repr` if it exists.
    *
    * @param seq  Array of integers
    * @param repr Another array of integers
    * @param grp Group of permutations
    * @param symGrp   Maximal subgroup of the group described by `grp` leaving `seq` invariant, i.e.
    *                 for all `h` in `symGrp`, `seq(i <|+| h) = seq(i)`
    */

  def permutationTo[G, F <: PermutationAction[G] with Singleton]
    (seq: Array[Int], repr: Array[Int], grp: GrpChain[G, F], symGrp: GrpChain[G, F])
    (implicit baseChange: BaseChange, baseSwap: BaseSwap, schreierSims: SchreierSims): Opt[G] = {
    import grp.{action, group}
    val chainGrp0 = grp.chain
    val n = seq.length
    val bo = BaseOrder[G, F](chainGrp0.base)
    val basePointGroups = SubgroupSearch.basePointGroups(chainGrp0, n)
    if (chainGrp0.isTrivial)
      return (if (seq.sameElements(repr)) Opt(Group[G].id) else Opt.empty[G])
    def rec(level: Int, g: G, chainGrp: Chain[G, F], chainSym: GrpChain[G, F], sOrbit: MutableOrbit): Opt[G] =
      chainGrp match {
        case node: Node[G, F] =>
          val beta = node.beta
          val orbitIt = node.orbitIterator
          while (orbitIt.hasNext) {
            val b = orbitIt.next
            val bg = b <|+| g
            if (repr(beta) == seq(bg)) {
              val nextG = node.u(b) |+| g
              var j = 1
              var disagree = false
              val m = basePointGroups(level).length
              while (j < m && !disagree) {
                val c = basePointGroups(level)(j)
                if (repr(c) != seq(c <|+| nextG))
                  disagree = true
                j += 1
              }
              if (!disagree) {
                if (orbits.Points.isSmallestInOrbit(bg, chainSym.generators, Opt(sOrbit))(implicitly, bo)) {
                  val nextSym = GrpChain.stabilizer(chainSym, bg)
                  val res = rec(level + 1, nextG, node.next, nextSym, sOrbit)
                  if (res.nonEmpty)
                    return res
                }
              }
            }
          }
          Opt.empty[G]
        case _: Term[G, F] => Opt(g.inverse)
      }
    rec(0, Group[G].id, chainGrp0, symGrp, MutableOrbit.forSize(n))
  }

  def findPermutationToMaximal[G, F <: PermutationAction[G] with Singleton]
    (array: Array[Int], grp: GrpChain[G, F], symGrp: GrpChain[G, F])
    (implicit baseChange: BaseChange, baseSwap: BaseSwap, schreierSims: SchreierSims): G = {
    val invArray = new Array[Int](array.length)
    cforRange(0 until array.length) { i => invArray(i) = Int.MaxValue - array(i) }
    findPermutationToMinimal(invArray, grp, symGrp)
  }

  /** Returns the minimal lexicographic representative of an array of integers under permutation.
    *
    * @param array  Array of integers
    * @param grp    Group of permutations, described as a BSGS chain
    * @param symGrp Subgroup of the group described by `chainGrp` leaving `sym` invariant, i.e.
    *               for all `h` in `symGrp`, `seq(i <|+| h) = seq(i)`
    * @return the permutation `g` in `chainGrp` such that `seq <|+| g` is the lexicographic minimal sequence.
    */
  def findPermutationToMinimal[G, F <: PermutationAction[G] with Singleton]
    (array: Array[Int], grp: GrpChain[G, F], symGrp: GrpChain[G, F])
    (implicit baseChange: BaseChange, baseSwap: BaseSwap, schreierSims: SchreierSims): G = {
    import grp.{action, equ, group}
    val n = array.length
    val minimal = new Array[Int](n)
    var minimalCorrectBefore = 0
    var minimalG = Group[G].id
    // Implements breadth-first search in the cosets `symGrp \ grp`, filtering elements that do not lead to a minimal
    // lexicographic representative at each step in the stabilizer chain.
    def rec(level: Int, toLevel: Int, curG: G, curChainGrp: Chain[G, F], curSymGrp: GrpChain[G, F], sOrbit: MutableOrbit): Unit =
      curChainGrp match {
        case node: Node[G, F] if level <= toLevel =>
          val candidates = metal.mutable.Buffer.empty[Int]
          val beta = node.beta
          val nextBeta = node.next match {
            case nextNode: Node[G, F] => nextNode.beta
            case _: Term[G, F] => n
          }
          if (nextBeta > minimalCorrectBefore) {
            cforRange(minimalCorrectBefore until nextBeta) { k =>
              minimal(k) = array(k <|+| minimalG)
            }
            minimalCorrectBefore = nextBeta
          }
          node.foreachOrbit { b =>
            val bg = b <|+| curG
            var comp = array(bg) - minimal(beta)
            var k = beta + 1
            lazy val nextG = node.u(b) |+| curG
            while (k < nextBeta && comp == 0) {
              comp = array(k <|+| nextG) - minimal(k)
              k += 1
            }
            if (comp <= 0) {
              if (comp < 0) {
                cforRange(beta until minimalCorrectBefore) { k =>
                  minimal(k) = array(k <|+| nextG)
                }
                minimalG = nextG
                candidates.clear
              }
              candidates += b
            }
          }
          cforRange(0 until candidates.length.toInt) { i =>
            val b = candidates(i)
            val bg = b <|+| curG
            if (orbits.Points.isSmallestInOrbit(bg, curSymGrp.generators, Opt(sOrbit))(implicitly, spire.std.int.IntAlgebra)) {
              val nextG = node.u(b) |+| curG
              rec(level + 1, toLevel, nextG, node.next, GrpChain.stabilizer(curSymGrp, bg), sOrbit)
            }
          }
        case _ =>
      }
    val sOrbit = MutableOrbit.forSize(n)
    val lexChain = if (grp.chain.hasLexicographicBase) grp.chain else
      BuildChain.fromChain[G, F, F](grp.chain, Opt(BaseGuideLex(n)))
    cforRange(0 until lexChain.length) { i =>
      rec(0, i, Group[G].id, lexChain, symGrp, sOrbit)
    }
    minimalG.inverse
  }

}
