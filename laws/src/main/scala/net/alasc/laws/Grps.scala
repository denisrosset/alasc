package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.finite._

import spire.math.max

import Arbitrary.arbitrary

object Grps {

  def genRandomElement[G](grp: Grp[G]): Gen[G] = Gen.parameterized { params => grp.randomElement(params.rng) }

  def genSubgrp[G:GrpAlgos](grp: Grp[G]): Gen[Grp[G]] =
    fromElements(genRandomElement(grp))

  def fromElements[G:GrpAlgos](elements: Gen[G]): Gen[Grp[G]] =
    for {
      n <- Gen.choose(0, 4)
      generators <- Gen.containerOfN[Seq, G](n, elements)
      c <- elements
    } yield Grp(generators: _*).conjugatedBy(c)

  def conjugatedFromElements[G:GrpAlgos](elements: Gen[G], conjugateBy: Gen[G]): Gen[Grp[G]] =
    for {
      grp <- fromElements(elements)
      c <- conjugateBy
    } yield grp.conjugatedBy(c)

  implicit def arbGrp[G:Arbitrary:GrpAlgos](implicit arbSmallG: Arbitrary[Small[G]]): Arbitrary[Grp[G]] =
    Arbitrary(conjugatedFromElements(arbSmallG.arbitrary.map(_.underlying), arbitrary[G]))

  def arbSubgrp[GG <: Grp[G] with Singleton, G:GrpAlgos](implicit witness: shapeless.Witness.Aux[GG]): Arbitrary[Grp[G]] =
    Arbitrary(genSubgrp(witness.value: GG))

  implicit def instances[G:Instances:GrpAlgos]: Instances[Grp[G]] =
    Instances[G].map(Grp(_))

}
