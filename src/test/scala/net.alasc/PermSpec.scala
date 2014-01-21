package net.alasc

import org.scalacheck._
import org.scalatest.FunSuite
import Arbitrary.arbitrary
import scala.collection.mutable.ArrayBuffer

object PermGenerators {
  def sortingSeq(seq: Seq[Int])(implicit builder: PermBuilder): Perm = {
    import Dom.ZeroBased._
    val images = ArrayBuffer(seq.view.zipWithIndex:_*)
    images.sortBy(_._1)
    builder.fromImages( seq.size, images(_)._2 )
  }

  val genSmallSize = Gen.choose(1, 100)

  val genSize = Gen.oneOf(1, 20) // TODO: replace by limits between Byte and Short, Short and Int

  val genPerm = for {
    n <- genSize
    a <- Gen.containerOfN[IndexedSeq, Int](n, arbitrary[Int])
  } yield sortingSeq(a)(Perm)

  val genPermPerm = for {
    n <- genSize
    a1 <- Gen.containerOfN[IndexedSeq, Int](n, arbitrary[Int])
    a2 <- Gen.containerOfN[IndexedSeq, Int](n, arbitrary[Int])
  } yield (sortingSeq(a1)(Perm), sortingSeq(a2)(Perm))

  val genPermDomain = for {
    n <- genSize
    a <- Gen.containerOfN[IndexedSeq, Int](n, arbitrary[Int])
    i <- Gen.choose(1, n)
  } yield (sortingSeq(a)(Perm), Dom._1(i))

  val genPermPermDomain = for {
    n <- genSize
    a1 <- Gen.containerOfN[IndexedSeq, Int](n, arbitrary[Int])
    a2 <- Gen.containerOfN[IndexedSeq, Int](n, arbitrary[Int])
    i <- Gen.choose(1, n)
  } yield (sortingSeq(a1)(Perm), sortingSeq(a2)(Perm), Dom._1(i))
}

object PermSpec extends Properties("Perm") {
  import PermGenerators._

  property("(g * g^-1) is identity") = Prop.forAll(genPerm) {
    g => (g*g.inverse).isIdentity
  }

  property("(g^-1)^-1 = g") = Prop.forAll(genPerm) {
    g => g.inverse.inverse === g
  }

  property("g = g") = Prop.forAll(genPerm) { pp => pp === pp }

  property("(g * h)^1 = h^-1 * g^-1") = Prop.forAll(genPermPerm) {
    case (g, h) => ((g*h).inverse) === (h.inverse)*(g.inverse)
  }

  property("{k^g}^(g^-1) = k") = Prop.forAll(genPermDomain) {
    case (g, k) => ((k ** g) ** (g.inverse)) === k
  }

  property("k^{g h} = {k^g}^h (right action)") = Prop.forAll(genPermPermDomain) {
    case (g, h, k) => (k ** (g * h)) === ((k ** g) ** h)
  }
}

class MurmurHash3Test extends FunSuite {
  test("MurmurHash3 hashCode does not depend on Array number type") {
    val intArray: Array[Int] = Array(1,2,3,100)
    val longArray: Array[Long] = Array(1L,2L,3L,100L)
    val byteArray: Array[Byte] = intArray.map(_.toByte)
    val shortArray: Array[Short] = intArray.map(_.toShort)
    import scala.util.hashing.MurmurHash3.arrayHash
    assert(arrayHash(intArray) == arrayHash(longArray))
    assert(arrayHash(intArray) == arrayHash(byteArray))
    assert(arrayHash(intArray) == arrayHash(shortArray))
  }
}
