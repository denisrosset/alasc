package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.util.Opt


import net.alasc.algebra.PermutationAction
import net.alasc.finite.Grp

trait MutableGrp[G] {

  def order: SafeLong

  def addGenerator(g: G): Unit

  def toGrp(completeChain: Boolean = true): Grp[G]

}

object MutableGrp {

  def fromAction[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (implicit action: F, schreierSims: SchreierSims): MutableGrp[G] =
    new MutableGrp[G] {

      private[this] val mutableChain = MutableChain.empty[G, F]

      def order = mutableChain.start.next.order

      def addGenerator(g: G): Unit = {
        schreierSims.siftAndAddStrongGenerator(mutableChain, g)
      }

      def toGrp(completeChain: Boolean = true): Grp[G] = {
        if (completeChain)
          mutableChain.completeStrongGenerators()
        new GrpChainExplicit(mutableChain.toChain(), Opt.empty[IndexedSeq[G]])
      }

    }

  def trivial[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
    (implicit action: F): MutableGrp[G] =
    new MutableGrp[G] {

        def order: SafeLong = SafeLong.one

        def addGenerator(g: G): Unit = {
          require(g.isId)
        }

        def toGrp(completeChain: Boolean = true): Grp[G] =
          new GrpChainExplicit(new Term[G, F], Opt(IndexedSeq.empty[G]))

    }

}
