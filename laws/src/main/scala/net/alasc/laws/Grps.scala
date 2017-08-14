package net.alasc.laws

import scala.annotation.tailrec
import scala.util.Random

import spire.algebra.{Eq, Group}

import org.scalacheck.{Arbitrary, Gen}

import net.alasc.finite._
import spire.math.{SafeLong, max}
import spire.util.Opt

import Arbitrary.arbitrary

object Grps {

  def genRandomElement[G](grp: Grp[G]): Gen[G] =
    arbitrary[Long].map( seed => grp.randomElement(new Random(seed)))

  def genSubgrp[G:Eq:Group:GrpGroup](grp: Grp[G]): Gen[Grp[G]] =
    fromElements(genRandomElement(grp))

  def fromElements[G:Eq:Group:GrpGroup](elements: Gen[G]): Gen[Grp[G]] =
    for {
      n <- Gen.choose(0, 4)
      generators <- Gen.containerOfN[Seq, G](n, elements)
      c <- elements
    } yield Grp(generators: _*).conjugatedBy(c)

  /** If the order of grp is bigger than maxOrder, removes generators until the order is <= maxOrder, or fails. */
  def forceSmallGroup[G:Eq:Group:GrpGroup](grp: Grp[G], maxOrder: SafeLong): Gen[Grp[G]] = {
    @tailrec def iter(smaller: Grp[G]): Opt[Grp[G]] =
      if (smaller.order <= maxOrder) Opt(smaller)
      else if (smaller.generators.length <= 1) Opt.empty[Grp[G]]
      else iter(Grp.fromGenerators(grp.generators.tail))
    iter(grp) match {
      case Opt(smallerGrp) => Gen.const(smallerGrp)
      case _ => Gen.fail[Grp[G]]
    }
  }

  def conjugatedFromElements[G:Eq:Group:GrpGroup](elements: Gen[G], conjugateBy: Gen[G]): Gen[Grp[G]] =
    for {
      grp <- fromElements(elements)
      c <- conjugateBy
    } yield grp.conjugatedBy(c)

  implicit def arbGrp[G:Arbitrary:Eq:Group:GrpGroup](implicit arbSmallG: Arbitrary[Small[G]]): Arbitrary[Grp[G]] =
    Arbitrary(conjugatedFromElements(arbSmallG.arbitrary.map(_.underlying), arbitrary[G]))

  implicit def instances[G:Instances:Eq:Group:GrpGroup]: Instances[Grp[G]] =
    Instances[G].map(Grp(_))

}
