package net.alasc
package bsgs

import org.scalatest.FunSuite

object M11 {
  import Dom.OneBased._
  def a = Perm(11)(1,2,3,4,5,6,7,8,9,10,11)
  def b = Perm(11)(3,7,11,8)(4,10,5,6)
  def id = Perm(11)
  def g = new GroupFromGenerators(id,TrivialAction(id),List(a,b))
  val order = BigInt(7920)
}

object M12 {
  import Dom.OneBased._
  def a = Perm(12)(1,2,3,4,5,6,7,8,9,10,11)
  def b = Perm(12)(1,12)(2,11)(3,6)(4,8)(5,9)(7,10)
  def c = Perm(12)(3,7,11,8)(4,10,5,6)
  def id = Perm(12)
  def g = new GroupFromGenerators(id,TrivialAction(id),List(a,b,c))
  val order = BigInt(95040)
}

object M24 {
  def a = Perm(24)("0123456789ABCDEFGHIJKLM")("N")
  def b = Perm(24)("0N")("1M")("2B")("3F")("4H")("59")("6J")("7D")("8K")("AG")("CL")("EI")
  def c = Perm(24)("2G968")("3CDI4")("7HABM")("EJLKF")
  def id = Perm(24)
  def g = new GroupFromGenerators(id,TrivialAction(id),List(a,b,c))
  val order = BigInt(244823040)
}

class MathieuSuite extends FunSuite {
  test("Construct BSGS for M11 using deterministic/empty base and check order") {
    import M11._
    assert(g.order == order)
  }
/*
  test("Construct BSGS for M11 using deterministic/full base and check order") {
    import M11._
    val bsgs = BSGS.schreierSims(List(a,b), Sym(11).identity, FullBase)
    assert(bsgs.order == order)
  }
 */
  test("Construct BSGS for M12 using deterministic/empty base and check order") {
    import M12._
    assert(g.order == order)
  }
/*
  test("Construct BSGS for M12 using deterministic/full base and check order") {
    import M12._
    val bsgs = BSGS.schreierSims(List(a,b,c), Sym(12).identity, FullBase)
    assert(bsgs.order == order)
  }
 */
  test("Construct BSGS for M24 using deterministic/empty base and check order") {
    import M24._
    assert(g.order == order)
  }
/*
  test("Construct BSGS for M24 using deterministic/full base and check order") {
    import M24._
    val bsgs = BSGS.schreierSims(List(a,b,c), Sym(24).identity, FullBase)
    assert(bsgs.order == order)
  }
 */
}
