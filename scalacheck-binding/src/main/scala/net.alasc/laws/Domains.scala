package net.alasc.laws

import scala.reflect.ClassTag
import scala.util.Random
import org.scalacheck.{Arbitrary, Gen}

import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.std.int._

import net.alasc.algebra._
import net.alasc.math._
import net.alasc.syntax.permutationAction._

object Domains {

  def sized: Gen[Domain] = Gen.posNum[Int].map(Domain(_))

  implicit def arbDomain: Arbitrary[Domain] = Arbitrary(sized)

}
