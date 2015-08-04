package net.alasc

import scala.{specialized => sp}

import spire.algebra.partial._
import spire.util._

import algebra._
import com.faacets.qalg.algebra._
import com.faacets.qalg.syntax.indup.all._
import com.faacets.qalg.syntax.all._

package object qalg {
  final class VecPermutationAction[V, @sp(Double, Long) A, P: FiniteGroup: FaithfulPermutationAction](implicit V: VecBuild[V, A]) extends PartialAction[V, P]{
    import net.alasc.syntax.permutationAction._
    import spire.syntax.group._
    import spire.syntax.action._

    override def actlIsDefined(p: P, v: V) = p.supportMax.getOrElseFast(-1) < V.length(v)
    override def actrIsDefined(v: V, p: P) = p.supportMax.getOrElseFast(-1) < V.length(v)
    def partialActl(p: P, v: V): Opt[V] =
      if (p.supportMax.getOrElseFast(-1) >= v.length) Opt.empty[V] else
        Opt(V.tabulate(v.length)( k => v(k <|+| p) ))
    def partialActr(v: V, p: P): Opt[V] = partialActl(p.inverse, v)
  }
  implicit def VecPermutationAction[V, @sp(Double, Long) A, P: FiniteGroup: FaithfulPermutationAction](implicit V: VecBuild[V, A]): PartialAction[V, P] = new VecPermutationAction[V, A, P]
}

