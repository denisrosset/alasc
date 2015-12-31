package net.alasc.math

import scala.annotation.tailrec

import scala.collection.{BitSet, SortedSet}
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

import spire.algebra.{Eq, PartialOrder}
import spire.algebra.lattice._
import spire.syntax.eq._
import spire.syntax.partialOrder._
import spire.syntax.lattice._
import spire.util.Opt

import net.alasc.algebra.{PermutationAction}
import net.alasc.syntax.permutationAction._
import net.alasc.util._

trait InDomain {

  type D <: Domain with Singleton

  val domain: D

  implicit def witness: shapeless.Witness.Aux[D] = shapeless.Witness.mkWitness[D](domain)

}

object InDomain {

  trait Of[D0 <: Domain with Singleton] extends InDomain {

    type D = D0

  }

}

class Domain private (val size: Int) {

  override def toString = s"Domain($size)"

}

object Domain extends UniquenessCache[Int, Domain] {

  val empty: Domain = Domain(0)

  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)

}
