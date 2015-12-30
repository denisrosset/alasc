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

trait InDomain[D <: Domain with Singleton] {

  val domain: D

  implicit def witness: shapeless.Witness.Aux[D] = shapeless.Witness.mkWitness[D](domain)

}

class DomainTyped[D <: Domain with Singleton](val domain: D) extends AnyVal {

  def unapply[F[E <: Domain with Singleton] <: InDomain[E]](f: F[_]): Opt[F[D]] =
    if (f.domain == domain)
      Opt(f.asInstanceOf[F[D]])
    else
      Opt.empty[F[D]]

}

class Domain private (val size: Int) {

  override def toString = s"Domain($size)"

  def Partition: PartitionBuilder[this.type] = new PartitionBuilder[this.type](this)

  def PartitionMap: PartitionMapBuilder[this.type] = new PartitionMapBuilder[this.type](this)

  def Typed: DomainTyped[this.type] = new DomainTyped[this.type](this)

}

object Domain extends UniquenessCache[Int, Domain] {

  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)

}
