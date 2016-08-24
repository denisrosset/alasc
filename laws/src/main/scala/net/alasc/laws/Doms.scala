package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}
import shapeless.Witness

import net.alasc.domains.{Dom, Domain, Partition}

object Doms {

  implicit def arbDomInDomain[D <: Domain with Singleton]
    (implicit witness: Witness.Aux[D]): Arbitrary[Dom[D]] =
    Arbitrary( Gen.choose(0, witness.value.size - 1).map(v => Dom(witness.value: D)(v)) )

}
