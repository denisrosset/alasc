package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}
import shapeless.Witness

import net.alasc.domains.{Dom, Domain, Partition}

object Doms {

  def inDomain(domain: Domain with Singleton):  Gen[Dom[domain.type]] =
    Gen.choose(0, domain.size - 1).map(v => Dom(domain)(v))

  implicit def arbDomInDomain[D <: Domain with Singleton]
    (implicit witness: Witness.Aux[D]): Arbitrary[Dom[D]] =
    Arbitrary( inDomain(witness.value: D) )

}
