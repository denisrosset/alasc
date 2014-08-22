package net.alasc.math
package bsgs

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}

import spire.syntax.groupAction._

import net.alasc.syntax.subgroup._

class HoltSuite extends FunSuite with NonImplicitAssertions with Matchers with EqMatchers {
  test("Example 4.4 page 111") {
    val alg = algorithms.BasicAlgorithms.randomized[Perm]()
    val mchain = alg.completeChainFromGenerators(Seq(Perm(1,2,3,4), Perm(2,4), Perm(5,6)), Seq(1, 2, 5))
    assert(mchain.start.next.order == 16)
    val images = alg.orderedIterator(mchain).map(g => (1 to 6).map( k => k <|+| g).mkString).toSeq
    images should equal(Seq("123456", "123465", "143256", "143265", "214356", "214365", "234156", "234165",
      "321456", "321465", "341256", "341265", "412356", "412365", "432156", "432165"))
  }
}
