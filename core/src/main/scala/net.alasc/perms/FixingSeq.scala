package net.alasc.perms

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.eq._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._

object FixingSeq {

  def apply[G:ClassTag:Eq:PGrpBuilder:Permutation](seq: Seq[Any]): Grp[G] = {
    val degree = seq.size
    val groups = seq.zipWithIndex.groupBy(_._1).map(_._2.map(_._2)).toSeq
    val order = (BigInt(1) /: groups) { case (acc, group) => acc * Sym.order(group.size) }
    val generators: Seq[G] = groups.flatMap {
      case Seq() => None
      case Seq(one) => None
      case indices =>
        val pairsToSwap = (indices zip indices.tail)
        pairsToSwap.map {
          case (i, j) => Permutation[G].swapping(i, j)
        }
    }
    implicitly[PGrpBuilder[G]].fromGeneratorsAndOrder(generators, order)
  }

}
