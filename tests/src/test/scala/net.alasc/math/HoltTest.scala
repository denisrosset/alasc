package net.alasc.math
package bsgs

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}

import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction
import net.alasc.syntax.subgroup._
import net.alasc.util._

import algorithms._

class HoltSuite extends FunSuite with NonImplicitAssertions with Matchers with EqMatchers {
  test("Example 4.4 page 111") {
    val alg = algorithms.BasicAlgorithms.randomized[Perm]()
    val mchain = alg.completeChainFromGenerators(Seq(Perm(1,2,3,4), Perm(2,4), Perm(5,6)), Seq(1, 2, 5))
    assert(mchain.start.next.order == 16)
    val images = BaseOrder.orderedIterator(mchain).map(g => (1 to 6).map( k => k <|+| g).mkString).toSeq
    images should equal(Seq("123456", "123465", "143256", "143265", "214356", "214365", "234156", "234165",
      "321456", "321465", "341256", "341265", "412356", "412365", "432156", "432165"))
  }

  test("Example in 4.6.2") {
    val alg = algorithms.BasicAlgorithms.randomized[Perm]()
    val mchain = alg.completeChainFromGenerators(Seq(Perm(1,2,3,4), Perm(2,4), Perm(5,6)), Seq(1,2,3,4,5,6))
    assert(mchain.start.next.order == 16)
    class Test(level: Int) extends SubgroupTest[Perm] {
      def test(b: Int, orbitImage: Int, currentG: Perm, node: Node[Perm])(
        implicit action: FaithfulPermutationAction[Perm]): Opt[Test] =
        (level, orbitImage) match {
          case (0, 1) | (0, 3) | (1, 2) => Opt(new Test(level + 1))
          case (0, _) | (1, _) => Opt.empty[Test]
          case _ => Opt(new Test(level + 1))
        }
    }
    def predicate(k: Perm) = ((1 <|+| k) == 1 || (1 <|+| k == 3)) && ((2 <|+| k) == 2)
    val printed = List("123456", "123465", "321456", "321465")
    val images = alg.generalSearch(mchain.start.next, predicate, new Test(0)).map(g => (1 to 6).map( k => k <|+| g).mkString).toSeq

    images should equal(Seq("123456", "123465", "321456", "321465"))
  }

  test("Example 4.6") {
    val alg = algorithms.BasicAlgorithms.randomized[Perm]()
    val g = Grp(Perm(1,2,3), Perm(4,5,6), Perm(1,4)(2,5)(3,6)(7,8))
    val h = Grp(Perm(1,6)(2,4)(3,5)(7,8), Perm(1,2)(3,7)(4,6)(5,8), Perm(2,3,7)(4,5,8))
    g.order should equal(18)
    h.order should equal(24)
    val ginterg = g intersect g
    val hinterh = h intersect h
    ginterg.order should equal(18)
    hinterh.order should equal(24)
    val ginterh = g intersect h
    val hinterg = h intersect g
    ginterh.order should equal(6)
    hinterg.order should equal(6)
  }
}
