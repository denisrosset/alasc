package net.alasc.tests
package lexico

import spire.algebra.Order

class LexOrdersSuite extends AlascSuite {

  test("Sequence lexicographic order (alasc)") {
    import net.alasc.lexico.lexSeqOrder._
    val data = Vector(
      Seq.empty[Int],
      Seq(0), Seq(0,0), Seq(0,0,0), Seq(0,0,1), Seq(0,1), Seq(0,1,0), Seq(0,1,1),
      Seq(1), Seq(1,0), Seq(1,0,0), Seq(1,0,1), Seq(1,1), Seq(1,1,0), Seq(1,1,1)
    )
    cforRange(0 until data.length) { i =>
      Order[Seq[Int]].compare(data(i), data(i)) shouldBe 0
    }
    cforRange(1 until data.length) { i =>
      val (before, after) = data.splitAt(i)
      for(b <- before; a <- after) {
        Order[Seq[Int]].compare(b, a) should be < 0
        Order[Seq[Int]].compare(a, b) should be > 0
      }
    }
  }

  test("Spire implements sequence lexicographic order") {
    import spire.std.seq._
    val data = Vector(
      Seq.empty[Int],
      Seq(0), Seq(0,0), Seq(0,0,0), Seq(0,0,1), Seq(0,1), Seq(0,1,0), Seq(0,1,1),
      Seq(1), Seq(1,0), Seq(1,0,0), Seq(1,0,1), Seq(1,1), Seq(1,1,0), Seq(1,1,1)
    )
    cforRange(0 until data.length) { i =>
      Order[Seq[Int]].compare(data(i), data(i)) shouldBe 0
    }
    cforRange(1 until data.length) { i =>
      val (before, after) = data.splitAt(i)
      for(b <- before; a <- after) {
        Order[Seq[Int]].compare(b, a) should be < 0
        Order[Seq[Int]].compare(a, b) should be > 0
      }
    }
  }


  test("Short lexicographic (shortlex) order") {
    import net.alasc.lexico.shortLexSeqOrder._
    val data = Vector(
      Seq.empty[Int],
      Seq(0), Seq(1),
      Seq(0,0), Seq(0,1), Seq(1,0), Seq(1,1),
      Seq(0,0,0), Seq(0,0,1), Seq(0,1,0), Seq(0,1,1),
      Seq(1,0,0), Seq(1,0,1), Seq(1,1,0), Seq(1,1,1)
    )
    cforRange(0 until data.length) { i =>
      Order[Seq[Int]].compare(data(i), data(i)) shouldBe 0
    }
    cforRange(1 until data.length) { i =>
      val (before, after) = data.splitAt(i)
      for(b <- before; a <- after) {
        Order[Seq[Int]].compare(b, a) should be < 0
        Order[Seq[Int]].compare(a, b) should be > 0
      }
    }
  }

}
