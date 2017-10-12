package net.alasc.laws

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary
import spire.syntax.cfor._
import net.alasc.finite.{Grp, GrpGroup}
import net.alasc.perms._
import net.alasc.perms.sized.{Perm16, Perm32}
import net.alasc.syntax.permutationAction._
import spire.std.int._

/** Generators and arbitraries for permutations.
  * 
  * Note: by default, permutations are generated using the size provided
  * by ScalaTest as maximal domain size.
  * 
  * When used e.g. as generators for a random group, these generators
  * should be resized.
  */
object Permutations {

  def permForSize(size: Int): Gen[Perm] =
    Gen.containerOfN[Array, Int](size, arbitrary[Int]) flatMap { randomValues =>
      import spire.std.long.LongAlgebra
      val images = new Array[Int](size)
      val toSort = new Array[Long](size) // bits 63..32 are random, 31..0 represents a domain element
      cforRange(0 until size) { k =>
        toSort(k) = (randomValues(k).toLong << 32) + k.toLong
      }
      spire.math.Sorting.sort(toSort) // sort, basically on the bits 63 .. 32
      cforRange(0 until size)( k => images(k) = toSort(k).toInt )
      Perm.fromImages(images)
    }

  def cyclesForSize(size: Int): Gen[Cycles] = permForSize(size).map(_.toCycles)

  val sizedPerm: Gen[Perm] = Gen.parameterized( parameters => permForSize(parameters.size) )

  val sizedCycles: Gen[Cycles] = sizedPerm.map(_.toCycles)

  implicit val arbPerm: Arbitrary[Perm] = Arbitrary(sizedPerm)

  implicit val arbCycles: Arbitrary[Cycles] = Arbitrary(sizedCycles)

  implicit val permInstances: Instances[Perm] =
    Instances[Perm](Seq(Perm(0,1), Perm.id))

  implicit val cyclesInstances: Instances[Cycles] =
    Instances[Cycles](Seq(Cycles(0,1), Cycles.id))

  implicit val permCloner: Cloner[Perm] =
    Cloner( (p: Perm) => Perm.fromImages(p.images(p.largestMovedPoint.fold(0)(_ + 1))) )

  implicit val cyclesCloner: Cloner[Cycles] = Cloner( (c: Cycles) => c.toPerm.toCycles )

  implicit def permutationGrp(size: Int)(implicit ev: GrpGroup[Perm]): Gen[Grp[Perm]] =
    Grps.fromElements(permForSize(size))

  // sized


  val genPerm32: Gen[Perm32] = for {
    seq <- Gen.containerOfN[Seq, Int](32, Gen.choose(1, 10000))
    res = Perm.sorting(seq)
  } yield Perm32.fromPerm(res)

  implicit val arbPerm32: Arbitrary[Perm32] = Arbitrary(genPerm32)

  val genPerm16: Gen[Perm16] = for {
    seq <- Gen.containerOfN[Seq, Int](16, Gen.choose(1, 10000))
    res = Perm.sorting(seq)
  } yield Perm16.fromPerm(res)

  implicit val arbPerm16: Arbitrary[Perm16] = Arbitrary(genPerm16)

  implicit val perm32Instances: Instances[Perm32] =
    Instances[Perm32](Seq(Perm32(0,1), Perm32.id))

  implicit val perm16Instances: Instances[Perm16] =
    Instances[Perm16](Seq(Perm16(0,1), Perm16.id))

  implicit val perm32Cloner: Cloner[Perm32] =
    Cloner( (p: Perm32) => Perm32.fromPerm(p.toPerm) )

  import net.alasc.perms.sized.implicits._
  implicit val perm16Cloner: Cloner[Perm16] =
    Cloner( (p: Perm16) => Perm16.fromPerm(p.toPerm) )

}
