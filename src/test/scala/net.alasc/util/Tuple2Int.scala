package net.alasc.util

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

class Tuple2IntCheck
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks with NonImplicitAssertions {
  property("Ints are stored and retrieved correctly") {
    forAll { (x: Int, y: Int) =>
      val tuple2 = Tuple2Int(x, y)
      (tuple2._1 == x && tuple2._2 == y) should equal(true)
    }
  }
}

class Tuple2IntSuite extends FunSuite {
  test("Tuple2Int extractor") {
    Tuple2Int(1,2) match {
      case Tuple2Int(x, y) => assert(x == 1 && y == 2)
      case _ => assert(false)
    }
  }
}

