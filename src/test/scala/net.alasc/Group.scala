package net.alasc

import org.scalatest.FunSuite
import org.scalacheck._

class GroupSuite extends FunSuite {
  test("Rubik cube group order (example from GAP system)") {
    import Dom.OneBased._
    // http://www.gap-system.org/Doc/Examples/rubik.html
    val g1 = Perm(48)( 1, 3, 8, 6)( 2, 5, 7, 4)( 9,33,25,17)(10,34,26,18)(11,35,27,19)
    val g2 = Perm(48)( 9,11,16,14)(10,13,15,12)( 1,17,41,40)( 4,20,44,37)( 6,22,46,35)
    val g3 = Perm(48)(17,19,24,22)(18,21,23,20)( 6,25,43,16)( 7,28,42,13)( 8,30,41,11)
    val g4 = Perm(48)(25,27,32,30)(26,29,31,28)( 3,38,43,19)( 5,36,45,21)( 8,33,48,24)
    val g5 = Perm(48)(33,35,40,38)(34,37,39,36)( 3, 9,46,32)( 2,12,47,29)( 1,14,48,27)
    val g6 = Perm(48)(41,43,48,46)(42,45,47,44)(14,22,30,38)(15,23,31,39)(16,24,32,40)
    val g = PGroup(g1, g2, g3, g4, g5, g6)
    assert(g.order === BigInt("43252003274489856000"))
    val colors = List(
      1,1,1,1,1,1,1,1,
      2,2,2,2,2,2,2,2,
      3,3,3,3,3,3,3,3,
      4,4,4,4,4,4,4,4,
      5,5,5,5,5,5,5,5,
      6,6,6,6,6,6,6,6)
//    assert(g.fixing(colors).order === 1) FIXME: restore
  }

  test("Bug in removing strong generators") {
    import Dom.OneBased._
    val g = PGroup(Perm(8)(1,2),Perm(8)(2,3),Perm(8)(3,4,5,6),Perm(8)(3,4,7,8))
    assert(g.order == 40320)
  }

  test("Enumeration of coset representatives has the right size") {
    import Dom.OneBased._
    val g = PGroup.fromPermutingGroup(Sym(6))
    val s = g.Subgroup(Perm(6)(1,2,3), Perm(6)(4,5,6))
    val numberOfCosets = g.subgroup.cosetIterator(s).size
    assert(numberOfCosets == g.order / s.order)
  }
}

/*
object GroupGenerators {
  val genGroupAndSeq = for {
    degree <- Gen.choose(2, 20)
    seq <- Gen.listOfN(degree, Gen.choose(1, 5))
  } yield (Group.symmetricGroup(degree), seq)

}

object GroupSpec extends Properties("Group") {
  import GroupGenerators._
  property("fixing") = Prop.forAll(genGroupAndSeq) {
    case (group, seq) => group.fixing(seq).order == seq.groupBy(identity).map( kv => (2 to kv._2.length).map(BigInt(_)).fold(BigInt(1))(_*_) ).product
  }
}
 */
