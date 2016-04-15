package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.finite._
import net.alasc.prep._

object Grps {

  def genRandomElement[G](grp: Grp[G]): Gen[G] = Gen.parameterized { params => grp.randomElement(params.rng) }

  def genSubgrp[G:GrpBuilder](grp: Grp[G]): Gen[Grp.SubgroupOf[grp.type, G]] =
    fromElements(genRandomElement(grp)).map(_.asSubgroupOf(grp).get)

  def fromElements[G:GrpBuilder](elements: Gen[G]): Gen[Grp[G]] =
    for {
      n <- Gen.choose(0, 4)
      generators <- Gen.containerOfN[Seq, G](n, elements)
    } yield Grp(generators: _*)

  implicit def arbGrp[G:Arbitrary:GrpBuilder]: Arbitrary[Grp[G]] = 
    Arbitrary {
      Gen.parameterized { parameters =>
        val gSize = math.max(parameters.size / 10, 3)
        val elements = Gen.resize(gSize, implicitly[Arbitrary[G]].arbitrary)
        fromElements(elements)
      }
    }

  implicit def arbPGrp[G](implicit arb: Arbitrary[Grp[G]], builder: PGrpBuilder[G]): Arbitrary[PGrp[G]] =
    Arbitrary {
      arb.arbitrary.map(grp => builder.fromGrp(grp))
    }

  implicit def arbSubgrp[GG <: Grp[G] with Singleton, G:GrpBuilder](implicit witness: shapeless.Witness.Aux[GG]): Arbitrary[Grp.SubgroupOf[GG, G]] =
    Arbitrary(genSubgrp(witness.value: GG))

  implicit def instances[G:Instances:GrpBuilder]: Instances[Grp[G]] =
    Instances[G].map(Grp(_))

}
