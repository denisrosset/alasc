package net.alasc.tests.util

import spire.algebra.Eq
import spire.laws.OrderLaws
import spire.math.ULong

import net.alasc.tests.AlascSuite
import net.alasc.util.UnsignedLongOrder

class UnsignedLongOrderSuite extends AlascSuite {

    checkAll("Long as unsigned", OrderLaws[Long].order(UnsignedLongOrder))

    forAll { (x: Long, y: Long) =>
      UnsignedLongOrder.compare(x, y) should === (ULong(x).compare(ULong(y)))
    }

}
