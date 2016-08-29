package net.alasc.domains

import spire.util.Opt

import net.alasc.util._

trait InDomain {

  type InAnother[D0 <: Domain with Singleton]

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

  def CastIn: Domain.CastIn[this.type] = new Domain.CastIn[this.type](this)

  override def toString = s"Domain($size)"

}

object Domain extends UniquenessCache[Int, Domain] {

  class CastIn[D <: Domain with Singleton](val domain: D) extends AnyVal {

    def unapply[T <: InDomain](t: InDomain): Opt[t.InAnother[D]] =
      if (t.domain eq domain)
        Opt[t.InAnother[D]](t.asInstanceOf[t.InAnother[D]])
      else
        Opt.empty[t.InAnother[D]]

  }

  val empty: Domain = Domain(0)

  protected def valueFromKey(size: Int): Domain = new Domain(size)
  protected def keyFromValue(domain: Domain): Option[Int] = Some(domain.size)

}
