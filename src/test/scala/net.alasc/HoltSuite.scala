package net.alasc

import org.scalatest.FunSuite

class HoltSuite extends FunSuite {
  test("Example 4.4") {
    import Dom.OneBased._
    val g1 = Perm(6)(1,2,3,4)
    val g2 = Perm(6)(2,4)
    val g3 = Perm(6)(5,6)
    val id = Perm(6)
    val base = List(1, 2, 5).map(Dom._1(_))
    val g = PGroup.fromGenerators(id, List(g1, g2, g3), base)
    assert(g.order == 16)
    val els: List[Perm] = g.bsgs.orderedIterator().toList
    assert( (els zip els.tail).forall( Function.tupled( (a,b) => g.bsgs.ElementOrdering.compare(a,b) < 0 ) ) )
    val printed = List("123456", "123465", "143256", "143265", "214356", "214365", "234156", "234165",
      "321456", "321465", "341256", "341265", "412356", "412365", "432156", "432165")
    assert( els.map(_.imagesSeq.map(_._1).mkString("")).sameElements(printed) )
  }

  test("Example in 4.6.2") {
    import Dom.OneBased._
    val g1 = Perm(6)(1,2,3,4)
    val g2 = Perm(6)(2,4)
    val g3 = Perm(6)(5,6)
    val id = Perm(6)
    val base = List[Dom](1,2,3,4,5,6)
    val g = PGroup.fromGenerators(id, List(g1, g2, g3), base)
    implicit val options = g.options
    assert(g.order == 16)
    case class Test(level: Int) extends BaseImageTest {
      def apply(b: Dom): (Boolean, BaseImageTest) = {
        if (level == 0)
          return (b === 1 || b === 3, Test(1))
        if (level == 1)
          return (b === 2, Test(2))
        return (true, Test(level + 1))
      }
    }
    def predicate(k: Perm) = (k.image(1) === 1 || k.image(1) === 3) && k.image(2) === 2
    val printed = List("123456", "123465", "321456", "321465")
    val els = g.bsgs.generalSearch(predicate, Test(0)).toList
    assert( els.map(_.imagesSeq.map(_._1).mkString("")).sameElements(printed) )
  }
  test("Example 4.6") {
    import Dom.OneBased._
    val group = PGroup.fromPermutingGroup(Sym(8))
    val g1 = Perm(8)(1,2,3)
    val g2 = Perm(8)(4,5,6)
    val g3 = Perm(8)(1,4)(2,5)(3,6)(7,8)
    val h1 = Perm(8)(1,6)(2,4)(3,5)(7,8)
    val h2 = Perm(8)(1,2)(3,7)(4,6)(5,8)
    val h3 = Perm(8)(2,3,7)(4,5,8)
    val g = group.Subgroup(g1,g2,g3)
    val h = group.Subgroup(h1,h2,h3)
    assert(g.order == 18)
    assert(h.order == 24)
    val ginterh = g.intersection(h)
    val hinterg = h.intersection(g)
    assert(ginterh.order == 6)
    assert(hinterg.order == 6)
  }
}
