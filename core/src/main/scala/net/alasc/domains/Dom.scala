package net.alasc.domains

import spire.algebra.{Action, Eq, Order}

import org.scalacheck.{Arbitrary, Gen}
import shapeless.Witness

import net.alasc.domains.{Domain, Partition}

class Dom[D <: Domain with Singleton](val value: Int) extends AnyVal {

  override def toString = s"Dom($value)"

}

object Dom {

  def unapply(dom: Dom[_ <: Domain with Singleton]): Option[Int] = Some(dom.value)

  def apply(domain: Domain)(value: Int): Dom[domain.type] = {
    require(0 <= value && 0 < domain.size)
    new Dom[domain.type](value)
  }

  // we use a dirty cast to avoid allocating domain order instances
  private[this] val zeroDomain = Domain(0)

  private[this] val internalOrder: Order[Dom[zeroDomain.type]] = new Order[Dom[zeroDomain.type]] {

    def compare(x: Dom[zeroDomain.type], y: Dom[zeroDomain.type]): Int = spire.std.int.IntAlgebra.compare(x.value, y.value)

  }

  implicit def order[D <: Domain with Singleton]: Order[Dom[D]] = internalOrder.asInstanceOf[Order[Dom[D]]]
  
}
