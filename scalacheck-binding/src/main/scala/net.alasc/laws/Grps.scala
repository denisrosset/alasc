package net.alasc.laws

import scala.reflect.ClassTag
import scala.util.Random

import org.scalacheck.{Arbitrary, Gen}

import spire.algebra.Eq
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.math._
import net.alasc.syntax.permutationAction._

object Grps {
  def genFromGrp[P](grp: Grp[P]): Gen[P] = Gen.parameterized { params => grp.randomElement(params.rng) }

  def fromElements[G:Eq:FiniteGroup:Representations:ClassTag](elements: Gen[G]): Gen[Grp[G]] =
    for {
      n <- Gen.choose(2, 4)
      generators <- Gen.containerOfN[Seq, G](n, elements)
    } yield Grp(generators: _*)

  implicit def arbGrp[G:Arbitrary:Eq:FiniteGroup:Representations:ClassTag]: Arbitrary[Grp[G]] = 
    Arbitrary {
      Gen.parameterized { parameters =>
        val gSize = math.max(parameters.size / 10, 3)
        val elements = Gen.resize(gSize, implicitly[Arbitrary[G]].arbitrary)
        fromElements(elements)
      }
    }

  implicit def instances[G:Instances:Eq:FiniteGroup:Representations:ClassTag]: Instances[Grp[G]] =
    Instances[G].map(Grp(_))
}
