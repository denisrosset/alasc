package net.alasc
package bsgs

import org.scalatest.FunSuite
import org.scalacheck._

class BSGSLexicographicSuite extends FunSuite {
  test("Case study for rightCosetMinimalRepresentative") {
    import Dom.OneBased._
    val gens = List(Perm(8)(1,3,2,8,5,6), Perm(8)(1,2,3,7,5,8,6))
    val id = Perm(8)
    val h = Perm(8)(1,6,7,2)(3,5,4,8)
    val correctMinimal = Perm(8)(4,8,7,6,5)
    val bs = BSGS.schreierSims(gens, id, FullBase)
    val minimalToTest = bs.rightCosetMinimalRepresentative(h)
    implicit val ordering = bs.ElementOrdering
    assert(minimalToTest == correctMinimal)
  }
}
object BSGSLexicographicSpec extends Properties("BSGSLexicographic") {
  val genGeneratorsAndRandomPermutation = for {
    degree <- Gen.choose(2, 8)
    numberOfGens <- Gen.choose(2, 4)
    gens = List.fill(numberOfGens)(Sym(degree).random)
    perm = Sym(degree).random
  } yield (degree, gens, perm)

  property("rightCosetMinimalRepresentative") =
    Prop.forAllNoShrink(genGeneratorsAndRandomPermutation) {
      case (degree, gens, h) => {
        val bs = BSGS.schreierSims(gens, Perm(degree), FullBase)
        implicit val elementOrdering = bs.ElementOrdering
        import scala.math.Ordering.Implicits._
        val minimal = (for {
          bsgss <- bs.elements
          s = bsgss.represents
        } yield s * h).min
        val minimal1 = bs.rightCosetMinimalRepresentative(h)
        minimal == minimal1
      }
    }
}
