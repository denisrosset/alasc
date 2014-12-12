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

import net.alasc.algebra._
import net.alasc.syntax.sequence._
import net.alasc.syntax.subgroup._
import net.alasc.util._

import bsgs._

trait RepresentativesSeq[T, G] extends RepresentativesOrdered[T, G] with coll.big.IndexedSeq[Representative[T, G]] {
  self =>
  implicit def classTagG: ClassTag[G]
  def stringPrefix = "RepresentativesIterable"

  def size = coll.BigIntSize(grp.order / symGrp.order)

  def foreach[U](f: Representative[T, G] => U): Unit = iterator.foreach(f)

  def find(seq: T): Option[LexRepresentative[T, G]] = {
    @tailrec def rec(block: Block): Option[LexRepresentative[T, G]] = block match {
      case nb: NodeBlock => nb.blockForSeq(seq) match {
        case RefOption(nextBlock) => rec(nextBlock)
        case _ => None
      }
      case tb: TermBlock => Some(tb)
    }
    rec(Block.start)
  }

  def apply(idx: BigInt) = {
    @tailrec def rec(block: Block): LexRepresentative[T, G] = block match {
      case nb: NodeBlock => nb.blockForIndex(idx) match {
        case RefOption(nextBlock) => rec(nextBlock)
        case _ => throw new IndexOutOfBoundsException
      }
      case tb: TermBlock => tb
    }
    rec(Block.start)
  }

  def length = grp.order / symGrp.order

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

    case class NextCandidate(b: Int, c: Int, tail: RefOption[NextCandidate] = RefNone)

    val beta = chain.beta
    val chainNextBeta = chain.next match {
      case nextNode: Node[G] => nextNode.beta
      case _ => tLength
    }
    assert(beta < chainNextBeta)
    val nextBeta = chainNextBeta.min(beta + maxSkip)

    protected lazy val candidatesForImages: debox.spkey.Map[Long, NextCandidate] = {
      val map = debox.spkey.Map.empty[Long, NextCandidate]
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
          val nextCandidate = NextCandidate(b, c, RefOption(map.getOrElse(images.toLong, null)))
          map.update(images.toLong, nextCandidate)
        }
        c += 1
      }
      map
    }

    protected lazy val sortedImages: Array[Long] = {
      val res = candidatesForImages.mapToArray[Long]((images, next) => images)
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

    def blockForSeq(seq: T): RefOption[Block] = {
      var seqImages = ULong(0)
      var k = beta
      while (k < nextBeta) {
        seqInt(seq, k) match {
          case NNOption(i) => seqImages = (seqImages << intBits) + ULong.fromInt(i)
          case _ => return RefNone
        }
        k += 1
      }
      if (candidatesForImages.contains(seqImages.toLong)) {
        val it = children
        while (it.hasNext) {
          val block = it.next
          if (block.images == seqImages)
            return RefSome(block)
        }
        sys.error("Map defined => block in children")
      }
      RefNone
    }

    def blockForIndex(idx: BigInt): RefOption[Block] = {
      val it = children
      while (it.hasNext) {
        val block = it.next
        if (block.index <= idx && idx < block.index + block.size)
          return RefSome(block)
      }
      RefNone
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
        var it = RefOption(candidatesForImages(images.toLong))
        while (it.nonEmpty) {
          val NextCandidate(b, c, next) = it.a
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
