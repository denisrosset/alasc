package net.alasc
package math

import scala.annotation.tailrec
import scala.util.Random

import spire.syntax.group._
import spire.syntax.groupAction._

import net.alasc.algebra._
import net.alasc.syntax.all._
import net.alasc.util._
import bsgs._
import algorithms._

class GrpLattice[G](implicit val algebra: FiniteGroup[G], representations: Representations[G], algorithms: BasicAlgorithms[G]) extends BoundedBelowLattice[Grp[G]] {
  def zero = Grp.fromGenerators[G](Iterable.empty, RefNone)

  def joinRepresentation(lhs: Grp[G], rhs: Grp[G]): Representation[G] = lhs.representationIfComputed match {
    case RefOption(lhsRepr) => rhs.representationIfComputed match {
      case RefOption(rhsRepr) => representations.repJoin(lhsRepr, rhsRepr, lhs.generators, rhs.generators)
      case _ => representations.repJoin(lhsRepr, lhs.generators, rhs.generators)
    }
    case _ => rhs.representationIfComputed match {
      case RefOption(rhsRepr) => representations.repJoin(rhsRepr, rhs.generators, lhs.generators)
      case _ => representations.get(lhs.generators ++ rhs.generators)
    }
  }

  protected def unionByAdding(chain: Chain[G], rp: Representation[G], generators: Iterable[G]): Grp[G] = {
    val mutableChain = algorithms.mutableChainCopyWithAction(chain, rp.action)
    algorithms.insertGenerators(mutableChain, generators)
    algorithms.completeStrongGenerators(mutableChain)
    Grp.fromChain(mutableChain.toChain, RefSome(rp))
  }

  def join(lhs: Grp[G], rhs: Grp[G]): Grp[G] = {
    // if one of the arguments has a computed chain with a representation compatible with the other argument generators,
    // augment the computed chain with these generators
    if (lhs.chainIfComputed.nonEmpty && rhs.generators.forall(g => lhs.representation.represents(g)))
      return unionByAdding(lhs.chain, lhs.representation, rhs.generators)
    if (rhs.chainIfComputed.nonEmpty && lhs.generators.forall(g => rhs.representation.represents(g)))
      return unionByAdding(rhs.chain, rhs.representation, lhs.generators)
    // if representations are known but not compatible, use the join of the representations for the union
    val rp = joinRepresentation(lhs, rhs)
    if (lhs.orderIfComputed.nonEmpty) {
      if (rhs.orderIfComputed.nonEmpty) {
        if (lhs.order >= rhs.order)
          unionByAdding(lhs.chain(rp), rp, rhs.generators)
        else
          unionByAdding(rhs.chain(rp), rp, lhs.generators)
      } else
        unionByAdding(lhs.chain(rp), rp, rhs.generators)
    } else {
      if (rhs.orderIfComputed.nonEmpty)
        unionByAdding(rhs.chain(rp), rp, lhs.generators)
      else
        Grp.fromGenerators(lhs.generators ++ rhs.generators, RefSome(rp))
    }
  }

  def meet(lhs: Grp[G], rhs: Grp[G]): Grp[G] = {
    def grpFromChains(lChain: Chain[G], rChain: Chain[G], rp: Representation[G]): Grp[G] =
      Grp.fromChain(algorithms.intersection(lChain, rChain)(rp.action).toChain, RefSome(rp))
    if (lhs.chainIfComputed.nonEmpty && rhs.chainIfComputed.nonEmpty) {
      val lCompatible = rhs.generators.forall(g => lhs.representation.represents(g))
      val rCompatible = lhs.generators.forall(g => rhs.representation.represents(g))
      if (lCompatible && (!rCompatible || lhs.order >= rhs.order))
        grpFromChains(lhs.chain, rhs.chain(lhs.representation, BaseGuideSeq(lhs.chain.base)), lhs.representation)
      else
        grpFromChains(rhs.chain, lhs.chain(rhs.representation, BaseGuideSeq(rhs.chain.base)), rhs.representation)
    } else {
      val rp = joinRepresentation(lhs, rhs)
      val lChain = lhs.chain(rp)
      val rChain = rhs.chain(rp, BaseGuideSeq(lChain.base)) // TODO: use BaseGuideSeqStripped
      grpFromChains(lChain, rChain, rp)
    }
  }

  override def lteqv(x: Grp[G], y: Grp[G]): Boolean = x.generators.forall(y.contains(_))
  override def gteqv(x: Grp[G], y: Grp[G]): Boolean = y.generators.forall(x.contains(_))
  override def eqv(x: Grp[G], y: Grp[G]): Boolean = (x.order == y.order) && lteqv(x, y)
  override def lt(x: Grp[G], y: Grp[G]): Boolean = lteqv(x, y) && (x.order < y.order)
  override def gt(x: Grp[G], y: Grp[G]): Boolean = gteqv(x, y) && (x.order > y.order)

  def partialCompare(x: Grp[G], y: Grp[G]): Double = {
    val c = x.order.compare(y.order)
    if (c < 0) {
      if (lteqv(x, y)) -1.0 else Double.NaN
    } else if (c > 0) {
      if (gteqv(x, y)) 1.0 else Double.NaN
    } else { // c == 0
      if (lteqv(x, y)) 0.0 else Double.NaN
    }
  }
}
