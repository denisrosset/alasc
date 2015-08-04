package net.alasc
package math
package enum

import scala.annotation.tailrec

import spire.algebra._
import spire.math.ULong
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.math.guide._
import net.alasc.syntax.subgroup._
import net.alasc.util._

import bsgs._
import ptrcoll._
import maps._

trait RepresentativesOrdered[T, G, A] extends BigIndexedSeq[RepresentativeOrdered[T, G]] with RepresentativesSearchable[T, G] {
  import collection.immutable.{BitSet, SortedMap}
  implicit def T: EnumerableOrdered[T, A]
  def groups: SortedMap[A, Set[Int]] = T.groups(t)
  def array: Array[Int] = {
    val res = new Array[Int](T.size(t))
    @tailrec def rec(it: Iterator[Set[Int]], k: Int): Unit =
      if (it.hasNext) {
        val s = it.next
        s.foreach( res(_) = k )
        rec(it, k + 1)
      }
    rec(groups.valuesIterator, 0)
    res
  }
  def arrayMaxInt: Int = array.max
  def orderedIterator: Iterator[RepresentativeOrdered[T, G]]
  override def iterator: Iterator[RepresentativeOrdered[T, G]] = orderedIterator
}

final class RepresentativesOrderedImpl[T, G, A](val t: T, val grp: Grp[G])(implicit val T: EnumerableOrdered[T, A], val TG: Permutable[T, G]) extends RepresentativesOrdered[T, G, A] {
  import grp.{classTag, equality, finiteGroup}

  override lazy val groups = super.groups
  override lazy val array = super.array
  override lazy val arrayMaxInt = super.arrayMaxInt

  def seqInt(seq: T, k: Int): NNOption = {
    val el = T.element(seq, k)
    if (groups.isDefinedAt(el))
      NNSome(array(groups(el).head))
    else
      NNNone
  }
  override lazy val chainInRepresentation = super.chainInRepresentation

  override def head: RepresentativeOrdered[T, G] = {
    val minG = Algorithms.findMinimalPermutation(array, chainInRepresentation, symGrp, representation)
    RepresentativeOrdered(t, minG.inverse, 0)
  }

  override def last: RepresentativeOrdered[T, G] = {
    val revArray = array.map(arrayMaxInt - _)
    val maxG = Algorithms.findMinimalPermutation(revArray, chainInRepresentation, symGrp, representation)
    RepresentativeOrdered(t, maxG.inverse, length - 1)

  }

  override lazy val partition = super.partition

  override lazy val symGrp = super.symGrp

  def findRepresentative(other: T): Opt[RepresentativeOrdered[T, G]] = {
    @tailrec def rec(block: Block): Opt[RepresentativeOrdered[T, G]] = block match {
      case nb: NodeBlock => nb.blockForSeq(other) match {
        case Opt(nextBlock) => rec(nextBlock)
        case _ => Opt.empty[RepresentativeOrdered[T, G]]
      }
      case tb: TermBlock => Opt(tb)
    }
    rec(Block.start)
  }

  def apply(idx: BigInt) = {
    @tailrec def rec(block: Block): RepresentativeOrdered[T, G] = block match {
      case nb: NodeBlock => nb.blockForIndex(idx) match {
        case Opt(nextBlock) => rec(nextBlock)
        case _ => throw new IndexOutOfBoundsException
      }
      case tb: TermBlock => tb
    }
    rec(Block.start)
  }

  def orderedIterator: Iterator[RepresentativeOrdered[T, G]] = {
    def iteratorSearch(block: Block): Iterator[RepresentativeOrdered[T, G]] = block match {
      case nb: NodeBlock => for {
        block <- nb.children
        repr <- iteratorSearch(block)
      } yield repr
      case tb: TermBlock => Iterator(tb)
    }
    iteratorSearch(Block.start)
  }

  val intBits = if (arrayMaxInt == 0) 1 else (32 - java.lang.Integer.numberOfLeadingZeros(arrayMaxInt))
  val maxSkip = 64 / intBits

  abstract class Block {
    def chain: Chain[G]
    def size: BigInt
    def index: BigInt
    def images: ULong
    def level: Int
  }

  object Block {
    def start = chainInRepresentation match {
      case node: Node[G] => NodeBlock(0, node, ULong(0), 0, debox.Buffer(FiniteGroup[G].id), debox.Buffer(symGrp))
      case term: Term[G] => TermBlock(0, term, ULong(0), 0, FiniteGroup[G].id)
    }
  }

  case class TermBlock(level: Int, chain: Term[G], images: ULong, index: BigInt, permutation: G) extends Block with RepresentativeOrdered[T, G] {
    def size = 1
    val element = permutation.inverse
    val rank = index
    val original = t
    implicit val actionTG = TG.action
  }

  case class NodeBlock(level: Int, chain: Node[G], images: ULong, index: BigInt, candidates: debox.Buffer[G], symGrps: debox.Buffer[Grp[G]]) extends Block {
    implicit def action = representation.action

    case class NextCandidate(b: Int, c: Int, tail: Opt[NextCandidate] = Opt.empty[NextCandidate])

    val beta = chain.beta

    val chainNextBeta = chain.next match {
      case nextNode: Node[G] => nextNode.beta
      case _ => array.length
    }
    assert(beta < chainNextBeta)
    val nextBeta = chainNextBeta.min(beta + maxSkip)

    protected lazy val candidatesForImages: MMap[Long, NextCandidate] = {
      val map = HashMMap.empty[Long, NextCandidate]
      var c = 0
      val n = candidates.length
      while (c < n) {
        val g = candidates(c)
        val it = chain.orbit.iterator
        while (it.hasNext) {
          val b = it.next
          var images: ULong = ULong.fromInt(array(b <|+| g))
          var k = beta + 1
          while (k < nextBeta) {
            val u = chain.u(b)
            images = (images << intBits) + ULong.fromInt(array((k <|+| u) <|+| g))
            k += 1
          }
          val nextCandidate = NextCandidate(b, c, Opt(map.getOrElse(images.toLong, null)))
          map.update(images.toLong, nextCandidate)
        }
        c += 1
      }
      map
    }

    protected lazy val sortedImages: Array[Long] = {
      val res = new Array[Long](candidatesForImages.size)
      @tailrec def rec(ptr: candidatesForImages.Ptr, i: Int): Unit = ptr match {
        case Valid(vptr) =>
          res(i) = candidatesForImages.ptrKey(vptr)
          rec(candidatesForImages.ptrNext(vptr), i + 1)
        case _ =>
      }
      rec(candidatesForImages.ptrStart, 0)
      object ULongOrder extends Order[Long] {
        import spire.syntax.order._
        def compare(x: Long, y: Long): Int = ULong(x) compare ULong(y)
      }
      spire.math.Sorting.quickSort(res)(ULongOrder, scala.reflect.classTag[Long])
      res
    }

    lazy val size: BigInt = {
      val co = (chain: Chain[G]).order
        (BigInt(0) /: symGrps.iterator) { case (sm, symGrp) => sm + co / symGrp.order }
    }

    def blockForSeq(seq: T): Opt[Block] = {
      var seqImages = ULong(0)
      var k = beta
      while (k < nextBeta) {
        seqInt(seq, k) match {
          case NNOption(i) => seqImages = (seqImages << intBits) + ULong.fromInt(i)
          case _ => return Opt.empty[Block]
        }
        k += 1
      }
      if (candidatesForImages.contains(seqImages.toLong)) {
        val it = children
        while (it.hasNext) {
          val block = it.next
          if (block.images == seqImages)
            return Opt(block)
        }
        sys.error("Map defined => block in children")
      }
      Opt.empty[Block]
    }

    def blockForIndex(idx: BigInt): Opt[Block] = {
      val it = children
      while (it.hasNext) {
        val block = it.next
        if (block.index <= idx && idx < block.index + block.size)
          return Opt(block)
      }
      Opt.empty[Block]
    }

    def children: Iterator[Block] = new ChildrenIterator

    class ChildrenIterator extends Iterator[Block] {
      var i = 0
      var blockIndex = index

      def hasNext = i < sortedImages.length

      def next: Block = {
        val images = ULong.fromLong(sortedImages(i))
        val newBlockCandidates = debox.Buffer.empty[G]
        val newBlockSymGrps = debox.Buffer.empty[Grp[G]]
        var it = Opt(candidatesForImages(images.toLong))
        while (it.nonEmpty) {
          val NextCandidate(b, c, next) = it.get
          val u = chain.u(b)
          val g = candidates(c)
          val bg = b <|+| g
          val (nextSym, transversal) = symGrps(c).stabilizer(bg, representation)
          if (transversal.orbit.min == bg) {
            newBlockCandidates += u |+| g
            newBlockSymGrps += nextSym
          }
          it = next
        }
        val block = chain.next match {
          case nextNode: Node[G] if nextNode.beta == nextBeta =>
            NodeBlock(level + 1, nextNode, images, blockIndex, newBlockCandidates, newBlockSymGrps)
          case nextNode: Node[G] =>
            assert(nextNode.beta < nextBeta)
            val nNode = Node.trivial(nextBeta, nextNode)
            NodeBlock(level + 1, nNode, images, blockIndex, newBlockCandidates, newBlockSymGrps)
          case termNode: Term[G] =>
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
