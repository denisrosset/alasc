package net.alasc

import org.scalatest.FunSuite

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
    val gens = List(g1,g2,g3,g4,g5,g6)
    val g = Group(gens, Perm(48))
    assert(g.order === BigInt("43252003274489856000"))
  }
}
