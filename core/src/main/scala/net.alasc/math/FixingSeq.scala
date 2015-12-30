package net.alasc.math

import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.eq._
import spire.util.Opt

import net.alasc.algebra._

object FixingSeq {

  def apply[P:ClassTag:Eq:Permutation](seq: Seq[Any]): Grp[P] = {
    val degree = seq.size
    val groups = seq.zipWithIndex.groupBy(_._1).map(_._2.map(_._2)).toSeq
    val order = (BigInt(1) /: groups) { case (acc, group) => acc * Sym.order(group.size) }
    val generators: Seq[P] = groups.flatMap {
      case Seq() => None
      case Seq(one) => None
      case indices =>
        val pairsToSwap = (indices zip indices.tail)
        pairsToSwap.map {
          case (i, j) => Permutation[P].swapping(i, j)
        }
    }
    import Grp.defaultAlgorithms
    implicit val reps = new PermutationRepresentations[P]
    new GrpLazy(generators, Opt(order))
  }

}
