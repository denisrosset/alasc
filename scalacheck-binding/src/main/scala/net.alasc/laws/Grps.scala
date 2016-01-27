package net.alasc.laws

import scala.reflect.ClassTag
import scala.util.Random

import org.scalacheck.{Arbitrary, Gen}

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.prep._
import net.alasc.syntax.permutationAction._

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
