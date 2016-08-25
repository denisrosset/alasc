package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.domains.Domain
import net.alasc.finite.{Grp, GrpBuilder}
import net.alasc.perms._
import net.alasc.syntax.permutationAction._

/** Generators and arbitraries for permutations.
  * 
  * Note: by default, permutations are generated using the size provided
  * by ScalaTest as maximal domain size.
  * 
  * When used e.g. as generators for a random group, these generators
  * should be resized.
  */
object Permutations {

  def forDomain[P:PermutationBuilder](domain: Domain): Gen[P] =
    Gen.containerOfN[Array, Int](domain.size, arbitrary[Int]) flatMap { randomValues =>
      import spire.std.long.LongAlgebra
      val images = new Array[Int](domain.size)
      val toSort = new Array[Long](domain.size) // bits 63..32 are random, 31..0 represents a domain element
      cforRange(0 until domain.size) { k =>
        toSort(k) = (randomValues(k).toLong << 32) + k.toLong
      }
      spire.math.Sorting.sort(toSort) // sort, basically on the bits 63 .. 32
      cforRange(0 until domain.size)( k => images(k) = toSort(k).toInt )
      PermutationBuilder[P].fromImages(images)
    }

  def sized[P:PermutationBuilder]: Gen[P] = Gen.parameterized( parameters => forDomain[P](Domain(parameters.size)) )

  implicit def arbPermutation[P:PermutationBuilder]: Arbitrary[P] = Arbitrary(sized[P])

  implicit def permutationInstances[P](implicit P: PermutationBuilder[P]): Instances[P] =
    Instances[P](Seq(Perm(0,1).toPermutation[P], P.id))

  implicit def permutationCloner[P](implicit P: PermutationBuilder[P]): Cloner[P] =
    Cloner( (p: P) => P.fromImages(p.images(p.largestMovedPoint.fold(0)(_ + 1))) )

  implicit def permutationGrp[P:GrpBuilder:PermutationBuilder](domain: Domain): Gen[Grp[P]] =
    Grps.fromElements(forDomain(domain))

}
