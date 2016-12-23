package net.alasc.finite

import scala.reflect.ClassTag

import spire.NoImplicit
import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import scalin.algebra.MatField

import net.alasc.algebra.{PermutationAction}
import net.alasc.bsgs.{BaseChange, BuildChain, SchreierSims}
import net.alasc.util._

/** Representation of group elements of type `G` on a vector of `dimension` over the field `K`. */
trait Rep[G, K] {
  self =>

  def apply(g: G): scalin.immutable.Mat[K]

  /** Dimension/degree of the representation. */
  def dimension: Int

  /** Tests whether this representation can represent the element `g`. */
  def represents(g: G): Boolean

  def widen[L](f: K => L)(implicit L: MatField[L, _ <: scalin.immutable.Mat[L]]): Rep[G, L] = new Rep[G, L] {
    def dimension = self.dimension
    def represents(g: G) = self.represents(g)
    def apply(g: G): scalin.immutable.Mat[L] = self.apply(g).map(f(_))
  }

}
