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

import net.alasc.algebra._
import net.alasc.std.seq._
import net.alasc.syntax.permutationAction._
import net.alasc.syntax.subgroup._
import net.alasc.util._

/** Wreath product group inhomogenous permutation representation.
  * 
  * Has limitations: only works for small sizes where fast division using `Divisor` is possible, and
  * only handle domains with `n <= 63` elements.
  */
class InhWrPrimitiveRepresentations[A:Eq:Group, H:Eq:Permutation](implicit val aReps: Representations[A]) extends InhWrRepresentations[A, H] {
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
    val n = partition.size
    val sizes = new Array[Int](n)
    val factors = new Array[Int](n + 1)
    val size: Int = {
      var i = 0
      var res = 1
      while (i < n) {
        sizes(i) = repForIndex(i).size
        factors(i) = res
        res *= sizes(i)
        i += 1
      }
      factors(i) = res
      res
    }
    val mul = new Array[Long](n)
    val shift = new Array[Int](partition.size)
    val offset: Long = {
      assert(n <= 63)
      var res = 0L
      var i = 0
      var remSize = size
      while (i < n) {
        val Divisor(_, max, off, f, sh) = Divisor(remSize - 1, sizes(i))
        assert(max >= remSize - 1)
        if (off == 1)
          res += (1 << i)
        else
          assert(off == 0)
        mul(i) = f
        shift(i) = sh
        i += 1
      }
      res
    }
    val action = new DefaultAction {
      def actr(k: Int, w: Wr[A, H]): Int =
        if (k >= size) k else {
          var rem = k
          var i = 0
          var ind = 0
          val an = w.aSeq.size
          while (i < an) {
            val nextRem = (((rem.toLong + ((offset & (1 << i)) >>> i)) * mul(i)) >>> shift(i)).toInt
            val alphai = rem - nextRem * sizes(i)
            rem = nextRem
            ind += factors(i <|+| w.h) * repForIndex(i).action.actr(alphai, w.aSeq(i))
            i += 1
          }
          while (i < n) {
            val nextRem = (((rem.toLong + ((offset & (1 << i)) >>> i)) * mul(i)) >>> shift(i)).toInt
            val alphai = rem - nextRem * sizes(i)
            rem = nextRem
            ind += factors(i <|+| w.h) * alphai
            i += 1
          }
          ind
        }
    }
  }
}
