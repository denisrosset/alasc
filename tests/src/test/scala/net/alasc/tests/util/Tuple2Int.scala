package net.alasc
package util

import org.scalacheck.Arbitrary._

import net.alasc.tests.AlascSuite

class Tuple2IntSuite extends AlascSuite {

  test("Ints are stored and retrieved correctly") {
    forAll { (x: Int, y: Int) =>
      val tuple2 = Tuple2Int(x, y)
      (tuple2._1 == x && tuple2._2 == y) should equal(true)
    }
  }

  test("Tuple2Int extractor") {
    Tuple2Int(1,2) match {
      case Tuple2Int(x, y) => assert(x == 1 && y == 2)
      case _ => assert(false)
    }
  }

}
