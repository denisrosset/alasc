package net.alasc.math
package wreath

import scala.language.higherKinds

import scala.collection.generic.CanBuildFrom
import scala.collection.SeqLike
import scala.collection.immutable
import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

import spire.algebra._
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.partialOrder._
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.lattice._
import spire.util.Nullbox

import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.util._

class InhWrImprimitiveRepresentations[A, H](implicit val aReps: Representations[A], val aAlgebra: FiniteGroup[A], val hAlgebra: Permutation[H]) extends InhWrRepresentations[A, H] {
  self =>
  object R extends RBuilder
  implicit val RClassTag: ClassTag[R] = classTag[R]
  def get(generators: Iterable[Wr[A, H]]) =
    if (generators.isEmpty)
      lattice.zero
    else
      generators.map(R(_)).reduce(_ join _)
  case class R(partition: Domain#Partition, repForBlock: Array[aReps.R]) extends ShapedR {
    def repForBlockString = repForBlock.mkString("Array(", ",", ")")
    override def toString = s"R($partition, $repForBlockString)"
    val startIndex: Array[Int] = {
      val n = partition.size
      val res = new Array[Int](n + 1)
      var acc = 0
      var i = 0
      while (i < n) {
        res(i) = acc
        acc += repForIndex(i).size
        i += 1
      }
      res(i) = acc
      res
    }
    def size = startIndex(startIndex.length - 1)
    val action = new DefaultAction {
      def actr(k: Int, w: Wr[A, H]): Int = {
        val n = partition.size
        var acc = 0
        var block = 0
        while (block < n) {
          if (startIndex(block) <= k && k < startIndex(block + 1)) {
            val sub = k - startIndex(block)
            val newBlock = block <|+| w.h
            if (block >= w.aSeq.size)
              return startIndex(newBlock) + sub
            else
              return startIndex(newBlock) + repForIndex(block).action.actr(sub, w.aSeq(block))
          }
          block += 1
        }
        k
      }
    }
  }
}
