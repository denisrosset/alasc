package net.alasc.perms

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong

import net.alasc.algebra.PermutationAction
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.group._

import metal.syntax._
import metal.mutable.FixedBitSet

import net.alasc.util.ParQuickSort
import net.alasc.syntax.permutationAction._

object FilterOrders {

  def orbit[G:PermutationAction](p: Int, g: G, bitset: FixedBitSet): Unit = {
    bitset += p
    var i = p <|+| g
    while (i != p) {
      bitset += i
      i = i <|+| g
    }
  }

  def isSubset(sup: FixedBitSet, sub: FixedBitSet): Boolean = {
    require(sup.nWords == sub.nWords)
    cforRange(0 until sup.nWords) { i =>
      val wp = sup.word(i)
      val wb = sub.word(i)
      if ((wp & wb) != wb) return false
    }
    true
  }

  def isRedundant[G:Eq:Group:PermutationAction](gi: G, gj: G, oi: Int, oj: Int, bi: FixedBitSet, bj: FixedBitSet): Boolean = {
    if (oi % oj != 0) return false // is not a potential power
    val p = gj.smallestMovedPoint.get
    orbit(p, gi, bi)
    orbit(p, gj, bj)
    val isSub = isSubset(bi, bj)
    bi.reset()
    bj.reset()
    if (!isSub) return false // has not a compatible orbit
    val a = Group[G].combineN(gi, oi/oj)
    cforRange(1 until oj) { z =>
      if (spire.math.gcd(z, oj) == 1 && Group[G].combineN(a, z) === gj) return true
    }
    false
  }

  def apply[G:Eq:Group](generators: IndexedSeq[G], faithfulAction: PermutationAction[G]): IndexedSeq[G] = {
    implicit def fa: PermutationAction[G] = faithfulAction
    @inline def asInt(s: SafeLong): Int = {
      require(s.isValidInt)
      s.toInt
    }
    val n = faithfulAction.largestMovedPoint(generators).getOrElseFast(-1) + 1
    val generatorsAndOrder = generators
      .map( g => (g, asInt(faithfulAction.permutationOrder(g))) ) // with order
      .filterNot(_._2 == 1) // remove identity
      .sortBy(go => -go._2) // sort by decreasing order
      .toList // make a list
    val bs1 = metal.mutable.FixedBitSet.reservedSize(n)
    val bs2 = metal.mutable.FixedBitSet.reservedSize(n)

    def keepNotRedundant(goi: (G, Int), current: List[(G, Int)]): List[(G, Int)] = current match {
      case Nil => Nil
      case hd :: tl if isRedundant(goi._1, hd._1, goi._2, hd._2, bs1, bs2) => keepNotRedundant(goi, tl)
      case hd :: tl => hd :: keepNotRedundant(goi, tl)
    }

    def removePowers(current: List[(G, Int)]): List[G] = current match {
      case Nil => Nil
      case hd :: tl => hd._1 :: removePowers(keepNotRedundant(hd, tl))
    }

    removePowers(generatorsAndOrder).toIndexedSeq
  }

}
