package net.alasc.tests
package perms

import spire.math.SafeLong
import spire.util.Opt

import net.alasc.bsgs.{BaseGuide, BaseGuideLex, BaseOrder, Chain, GrpChainPermutationAction, KernelBuilder, Node, SchreierSims, SubgroupDefinition, SubgroupSearch, SubgroupTest}
import net.alasc.finite._
import net.alasc.perms.{Perm, PermAlgebra}

abstract class HoltSuite(implicit builder: GrpChainPermutationAction[Perm]) extends AlascSuite {

  test("Example 4.4 page 111") {
    val mchain = SchreierSims.deterministicSchreierSims[Perm, PermAlgebra.type](Seq(Perm(1,2,3,4), Perm(2,4), Perm(5,6)), KernelBuilder.trivial[Perm], Seq(1, 2, 5))
    assert(mchain.start.next.order == 16)
    val images = BaseOrder.orderedIterator(mchain).map(g => (1 to 6).map( k => k <|+| g).mkString).toSeq
    images should equal(Seq("123456", "123465", "143256", "143265", "214356", "214365", "234156", "234165",
      "321456", "321465", "341256", "341265", "412356", "412365", "432156", "432165"))
  }

  test("Example in 4.6.2") {
    val mchain = SchreierSims.deterministicSchreierSims[Perm, PermAlgebra.type](Seq(Perm(1,2,3,4), Perm(2,4), Perm(5,6)), KernelBuilder.trivial[Perm], Seq(1,2,3,4,5,6))
    assert(mchain.start.next.order == 16)

    object Definition extends SubgroupDefinition[Perm, PermAlgebra.type] {
      implicit def action: PermAlgebra.type = PermAlgebra
      def baseGuideOpt: Opt[BaseGuide] = Opt(BaseGuideLex(6))
      def inSubgroup(k: Perm): Boolean = ((1 <|+| k) == 1 || (1 <|+| k == 3)) && ((2 <|+| k) == 2) // we don't do anything special
      def firstLevelTest(guidedChain: Chain[Perm, PermAlgebra.type]): SubgroupTest[Perm, PermAlgebra.type] = new Test(0)
      class Test(level: Int) extends SubgroupTest[Perm, PermAlgebra.type] {
        def test(b: Int, orbitImage: Int, currentG: Perm, node: Node[Perm, PermAlgebra.type]): Opt[Test] =
          (level, orbitImage) match {
            case (0, 1) | (0, 3) | (1, 2) => Opt(new Test(level + 1))
            case (0, _) | (1, _) => Opt.empty[Test]
            case _ => Opt(new Test(level + 1))
          }
      }
    }
    val printed = List("123456", "123465", "321456", "321465")
    val images = SubgroupSearch.generalSearch(Definition, mchain.start.next).map(g => (1 to 6).map( k => k <|+| g).mkString).toSeq
    images should equal(printed)
  }

  test("Example 4.6") {
    val g = Grp(Perm(1,2,3), Perm(4,5,6), Perm(1,4)(2,5)(3,6)(7,8))
    val h = Grp(Perm(1,6)(2,4)(3,5)(7,8), Perm(1,2)(3,7)(4,6)(5,8), Perm(2,3,7)(4,5,8))
    g.order should ===(SafeLong(18))
    h.order should ===(SafeLong(24))
    val ginterg = g intersect g
    val hinterh = h intersect h
    ginterg.order should ===(SafeLong(18))
    hinterh.order should ===(SafeLong(24))
    val ginterh = g intersect h
    val hinterg = h intersect g
    ginterh.order should ===(SafeLong(6))
    hinterg.order should ===(SafeLong(6))
  }

}

class HoltSuiteDeterministic extends HoltSuite()(PermSuite.deterministic)

class HoltSuiteRandomized extends HoltSuite()(PermSuite.randomized)
