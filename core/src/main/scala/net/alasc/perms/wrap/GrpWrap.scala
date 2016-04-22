package net.alasc.perms.wrap

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.util.Opt

import net.alasc.bsgs.{BaseChange, BaseSwap, BuildChain, GrpChain, GrpChainExplicit, SchreierSims}
import net.alasc.finite.{Grp, GrpBuilder, Rep}
import net.alasc.perms.{FaithfulPermRep, PermGrpBuilder}

class GrpWrapBuilder[G:ClassTag:Eq:Group, R <: FaithfulPermRep[G] with Singleton]
  (val rep: R)(implicit val baseChange: BaseChange, val baseSwap: BaseSwap, val schreierSims: SchreierSims)
  extends PermGrpBuilder[Rep.Wrap[G, R]] {

  implicit def F: R#F = rep.permutationAction
  type GG = Grp[Rep.Wrap[G, R]] with Wrapped

  def equ = Rep.Wrap.wrapEq[G, R]
  def group = Rep.Wrap.wrapGroup[G, R]

  trait Wrapped {

    def underlying: GrpChain[G, R#F]

  }

  def wrap(underlying0: GrpChain[G, R#F]): GG = new Grp[Rep.Wrap[G, R]] with Wrapped {

    val underlying: GrpChain[G, R#F] = underlying0
    def equ = Rep.Wrap.wrapEq[G, R]
    def group = Rep.Wrap.wrapGroup[G, R]
    def contains(g: Rep.Wrap[G, R]) = underlying.contains(g.underlying)
    def generators = underlying.generators.map(new Rep.Wrap[G, R](_))
    def iterator: Iterator[Rep.Wrap[G, R]] = underlying.iterator.map(new Rep.Wrap[G, R](_))
    def randomElement(random: scala.util.Random): Rep.Wrap[G, R] = new Rep.Wrap[G, R](underlying.randomElement(random))
    def order = underlying.order

  }

  def fromGenerators(generators: Iterable[Rep.Wrap[G, R]]): GG = {
    val gens = generators.map(_.underlying)
    wrap(new GrpChainExplicit[G, R#F](BuildChain.fromGenerators[G, R#F](gens), generatorsOpt = Opt(gens)))
  }

  def fromGeneratorsAndOrder(generators: Iterable[Rep.Wrap[G, R]], order: SafeLong): GG = {
    val gens = generators.map(_.underlying)
    wrap(new GrpChainExplicit[G, R#F](BuildChain.fromGeneratorsAndOrder[G, R#F](gens, order), generatorsOpt = Opt(gens)))
  }

  def fromGrp(grp: Grp[Rep.Wrap[G, R]]): GG = grp match {
    case gw: Grp[Rep.Wrap[G, R]] with Wrapped => gw
    case _ => fromGeneratorsAndOrder(grp.generators, grp.order)
  }

}
