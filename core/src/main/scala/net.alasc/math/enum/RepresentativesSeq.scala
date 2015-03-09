package net.alasc
package math
package enum

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Action, Order}
import spire.math.ULong
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.syntax.subgroup._
import net.alasc.util._

import bsgs._

import ptrcoll._
import maps._
import big._

trait RepresentativesSeq[T, G] extends RepresentativesOrdered[T, G] with BigIndexed[Representative[T, G]] {
  self =>
  implicit def classTagG: ClassTag[G]
  override def stringPrefix = "RepresentativesIterable"

  def size = grp.order / symGrp.order

  def foreach[U](f: Representative[T, G] => U): Unit = iterator.foreach(f)

  def find(seq: T): Opt[LexRepresentative[T, G]] = {
    @tailrec def rec(block: Block): Opt[LexRepresentative[T, G]] = block match {
      case nb: NodeBlock => nb.blockForSeq(seq) match {
        case Opt(nextBlock) => rec(nextBlock)
        case _ => Opt.empty[LexRepresentative[T, G]]
      }
      case tb: TermBlock => Opt(tb)
    }
    rec(Block.start)
  }

  def apply(idx: BigInt) = {
    @tailrec def rec(block: Block): LexRepresentative[T, G] = block match {
      case nb: NodeBlock => nb.blockForIndex(idx) match {
        case Opt(nextBlock) => rec(nextBlock)
        case _ => throw new IndexOutOfBoundsException
      }
      case tb: TermBlock => tb
    }
    rec(Block.start)
  }

  def iterator = {
    def iteratorSearch(block: Block): Iterator[Representative[T, G]] = block match {
      case nb: NodeBlock => for {
        block <- nb.children
        repr <- iteratorSearch(block)
      } yield repr
      case tb: TermBlock => Iterator(tb)
    }
    iteratorSearch(Block.start)
  }

  val intBits = if (maxInt == 0) 1 else (32 - java.lang.Integer.numberOfLeadingZeros(maxInt))
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
      case node: Node[G] => NodeBlock(0, node, ULong(0), 0, debox.Buffer(finiteGroupG.id), debox.Buffer(symGrp))
      case term: Term[G] => TermBlock(0, term, ULong(0), 0, finiteGroupG.id)
    }
  }

  case class TermBlock(level: Int, chain: Term[G], images: ULong, index: BigInt, permutation: G) extends Block with LexRepresentative[T, G] {
    def size = 1
    val element = permutation.inverse
    val rank = index
    val original = t
    implicit val actionTG = self.actionTG
  }

  case class NodeBlock(level: Int, chain: Node[G], images: ULong, index: BigInt, candidates: debox.Buffer[G], symGrps: debox.Buffer[Grp[G]]) extends Block {
    implicit def action = representation.action

    case class NextCandidate(b: Int, c: Int, tail: Opt[NextCandidate] = Opt.empty[NextCandidate])

    val beta = chain.beta
    val chainNextBeta = chain.next match {
      case nextNode: Node[G] => nextNode.beta
      case _ => tLength
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
          var images: ULong = ULong.fromInt(tInt(b <|+| g))
          var k = beta + 1
          while (k < nextBeta) {
            val u = chain.u(b)
            images = (images << intBits) + ULong.fromInt(tInt((k <|+| u) <|+| g))
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
      import ptrcoll.syntax.all._
      val res = new Array[Long](candidatesForImages.size)
      var i = 0
      var ptr = candidatesForImages.pointer
      import candidatesForImages.PtrTC
      while (ptr.hasAt) {
        res(i) = ptr.at
        i += 1
        ptr = ptr.nextPtr
      }
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
