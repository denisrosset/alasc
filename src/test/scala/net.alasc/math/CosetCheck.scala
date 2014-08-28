package net.alasc.math

import scala.annotation.tailrec

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import spire.syntax.group._

import net.alasc.syntax.subgroup._

object M11 {
  val order = BigInt(7920)
  val repr11 = Map(
    "a" -> Perm(2,10)(4,11)(5,7)(8,9),
    "b" -> Perm(1,4,3,8)(2,5,6,9)
  )
  val repr12 = Map(
    "a" -> Perm(1,6)(2,9)(5,7)(8,10),
    "b" -> Perm(1,6,7,4)(2,8)(3,9)(5,11,12,10)
  )
  val repr55 = Map(
    "a" -> Perm(1,37)(3,39)(4,46)(5,55)(6,48)(7,16)(8,43)(9,18)(10,52)(12,42)(14,51)(15,21)(17,41)(19,50)(22,29)(23,45)(24,31)(25,54)(26,35)(27,34)(28,33)(30,44)(32,53)(40,47),
    "b" -> Perm(1,10,13,36)(2,25)(3,26,11,32)(4,6,24,22)(5,9,27,29)(7,14,31,23)(8,15,34,30)(12,33)(16,19,18,28)(17,20,21,35)(37,40,39,44)(38,41,42,45)(46,49,48,53)(47,50,51,54)
  )
  val representations = Map(11 -> repr11, 12 -> repr12) //, 55 -> repr55)
  val subgroups = Map(
    "L2(11)" -> (Seq("a", "babbab"), BigInt(660)),
    "S5" -> (Seq("a", "abbabbababbabbab"), BigInt(120)),
    "2S4" -> (Seq("a", "bababbabab"), BigInt(48)),
    "M9:2" -> (Seq("b-a-b-a-bab", "b-b-a-babb"), BigInt(144)),
    "M10" -> (Seq("b-a-b-a-b-a-b-a-aabababab", "b-b-a-b-b-a-abababbababbabb"), BigInt(720))
  )
  def compute(word: String, a: Perm, b: Perm): Perm = {
    @tailrec def rec(remaining: String, g: Perm): Perm = {
      if (remaining.startsWith("a-"))
        rec(remaining.drop(2), g |+| (a.inverse))
      else if (remaining.startsWith("b-"))
        rec(remaining.drop(2), g |+| (b.inverse))
      else if (remaining.startsWith("a"))
        rec(remaining.drop(1), g |+| a)
      else if (remaining.startsWith("b"))
        rec(remaining.drop(1), g |+| b)
      else {
        assert(remaining.isEmpty)
        g
      }
    }
    rec(word, Perm.Algebra.id)
  }
  val genGroupSubgroup = for {
    repr <- Gen.oneOf(M11.representations.values.toSeq)
    a = repr("a")
    b = repr("b")
    grp = Grp(a, b)
    g = grp.randomElement(scala.util.Random)
    (subgroupWords, subgroupOrder) <- Gen.oneOf(M11.subgroups.values.toSeq)
  } yield (Grp(a, b), Grp(subgroupWords.map( word => g.inverse |+| compute(word, a, b) |+| g ): _*), subgroupOrder)
}

class CosetCheck extends PropSpec with Matchers with EqMatchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  property("Union of cosets contains all the group elements") {
    forAll(M11.genGroupSubgroup) {
      case (m11, subgroup, subgroupOrder) =>
        assert(subgroup.order == subgroupOrder)
        val unionOfCosets = (m11 / subgroup).iterator.flatMap(_.iterator).toSet
        unionOfCosets == m11.elements.iterator.toSet
    }
  }
}
