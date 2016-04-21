package net.alasc.tests
package domains

import net.alasc.domains.MutableOrbit

class MutableOrbitSuite extends AlascSuite {

  test("Behavior of MutableOrbit") {
    val o = MutableOrbit.forSize(128)
    o.nextInCurrentCheck(0) shouldBe -1
    o.addNew(10)
    o.addNew(20)
    o.inOrbit(10) shouldBe true
    o.inOrbit(20) shouldBe true
    o.inOrbit(5) shouldBe false
    o.nextInCurrentCheck(0) shouldBe -1
    o.step()
    o.inOrbit(10) shouldBe true
    o.inOrbit(20) shouldBe true
    o.inOrbit(5) shouldBe false
    o.addNew(5)
    o.addNew(15)
    o.addNew(25)
    o.inOrbit(5) shouldBe true
    o.nextInCurrentCheck(0) shouldBe 10
    o.nextInCurrentCheck(10) shouldBe 10
    o.nextInCurrentCheck(11) shouldBe 20
    o.nextInCurrentCheck(20) shouldBe 20
    o.nextInCurrentCheck(21) shouldBe -1
    o.nextInCurrentCheck(128) shouldBe -1
    o.step()
    o.inOrbit(5) shouldBe true
    o.inOrbit(10) shouldBe true
    o.inOrbit(15) shouldBe true
    o.inOrbit(20) shouldBe true
    o.inOrbit(25) shouldBe true
    o.nextInCurrentCheck(0) shouldBe 5
    o.nextInCurrentCheck(6) shouldBe 15
    o.nextInCurrentCheck(16) shouldBe 25
    o.nextInCurrentCheck(26) shouldBe -1
    o.step()
    o.nextInCurrentCheck(0) shouldBe -1
    o.inOrbit(5) shouldBe true
    o.inOrbit(10) shouldBe true
    o.inOrbit(15) shouldBe true
    o.inOrbit(20) shouldBe true
    o.inOrbit(25) shouldBe true
  }

  forAll { set: Set[Int] =>
    val bitset = scala.collection.immutable.BitSet(set.map(x => (x / 2).abs % 10000).toSeq: _*)
    val n = bitset.toBitMask.length * 64
    val mo = MutableOrbit.forSize(n)
    mo.addNew(bitset)
    val newSet = scala.collection.immutable.BitSet.fromBitMaskNoCopy(mo.toBitMask)
    bitset shouldBe newSet
  }

}
