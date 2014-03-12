package net.alasc

import org.scalacheck._
import org.scalatest.FunSuite
import Arbitrary.arbitrary
import scala.collection.mutable.ArrayBuffer

object PermGenerators {
  val genSmallSize = Gen.choose(1, 100)

  val genSize = Gen.oneOf(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
  255,256,65535,65536) // TODO: replace by limits between Byte and Short, Short and Int

  def perm(n: Int) = Gen.parameterized { params =>
    import Dom.ZeroBased._
    val seq = params.rng.shuffle((0 until n).toBuffer)
    Gen.const(Perm.fromImages(n)(seq(_)))
  }

  val genPerm = for {
    n <- genSize
    p <- perm(n)
  } yield p

  val genPermPerm = for {
    n <- genSize
    p1 <- perm(n)
    p2 <- perm(n)
  } yield (p1, p2)

  val genPermDomain = for {
    n <- genSize
    p <- perm(n)
    i <- Gen.choose(1, n)
  } yield (p, Dom._1(i))

  val genPermPermDomain = for {
    n <- genSize
    p1 <- perm(n)
    p2 <- perm(n)
    i <- Gen.choose(1, n)
  } yield (p1, p2, Dom._1(i))
}

object PermSpec extends Properties("Perm") {
  import PermGenerators._

  property("equals") = Prop.forAll(genPerm) {
    p1 => {
      val p2 = p1.toOptimized
      val p3 = p1.toGeneric
      val p4 = p1.toIntPerm
      p1 == p1 && p1 == p2 && p1 == p3 && p1 == p4 &&
      p2 == p2 && p2 == p2 && p2 == p3 && p2 == p4 &&
      p3 == p2 && p3 == p2 && p3 == p3 && p3 == p4 &&
      p4 == p2 && p4 == p2 && p4 == p3 && p4 == p4
    }
  }
  property("hashCode") = Prop.forAll(genPerm) {
    p1 => {
      val p2 = p1.toOptimized
      val p3 = p1.toGeneric
      val p4 = p1.toIntPerm
      val seq = Seq(p1, p2, p3, p4)
      seq.forall(p1.hashCode == _.hashCode) && seq.forall(p => p.hash == p.hashCode)
    }
  }
  property("hash") = Prop.forAll(genPerm) {
    p => p.toOptimized.hash == p.hash && p.hash == p.toGeneric.hashSpec
  }

  property("imagesSeq") = Prop.forAll(genPerm) {
    p => p.toOptimized.imagesSeq == p.toGeneric.imagesSeq
  }

  property("fromImages") = Prop.forAll(genPerm) {
    p => Perm.fromImages(p.size)(p.image) === GenericPerm.fromImages(p.size)(p.image)
  }

  property("fromPreimages") = Prop.forAll(genPerm) {
    p => Perm.fromPreimages(p.size)(p.image) === GenericPerm.fromPreimages(p.size)(p.image)
  }

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

  property("k^g = k^{g.cycles}") = Prop.forAll(genPermDomain) {
    case (g, k) => ((k ** g) === (k ** g.cycles))
  }

  property("k^{g h} = {k^g}^h (right action)") = Prop.forAll(genPermPermDomain) {
    case (g, h, k) => (k ** (g * h)) === ((k ** g) ** h)
  }
}

class PermSuite extends FunSuite {
  test("Perm product with cycle") {
    import Dom.OneBased._
    val p = Perm(3)(1,2,3)
    assert(p.image(1) === 2)
    assert(p.image(2) === 3)
    assert(p.image(3) === 1)
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
  test("Perm concatenation ++") {
    import Dom.OneBased._
    val p1 = Perm(3)(1,2)
    val p2 = Perm(2)(1,2)
    assert(p1 ++ p2 === Perm(5)(1,2)(4,5))
  }
}
