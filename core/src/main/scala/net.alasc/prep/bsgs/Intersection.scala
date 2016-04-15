package net.alasc.prep.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.FaithfulPermutationAction

/** Defines the subgroup which intersect `chain2`, given in action `action`. */
case class Intersection[G:ClassTag:Eq:Group](val action: FaithfulPermutationAction[G], val chain2: Chain[G])(implicit baseChange: BaseChange, schreierSims: SchreierSims) extends SubgroupDefinition[G] {

  def baseGuideOpt = Opt(BaseGuideSeq(chain2.base))

  def inSubgroup(g: G): Boolean = chain2.sifts(g)

  class Test(level: Int, currentChain2: Chain[G], prev2Inv: G) extends SubgroupTest[G] {

    def test(b: Int, orbitImage: Int, currentG: G, node: Node[G]): Opt[Test] = {
      val b2 = imply(action) { orbitImage <|+| prev2Inv }
      currentChain2 match {
        case node2: Node[G] if node2.inOrbit(b2) =>
          Opt(new Test(level + 1, node2.next, prev2Inv |+| node2.uInv(b2)))
        case _ if node.beta == b2 =>
          Opt(new Test(level + 1, currentChain2, prev2Inv))
        case _ =>
          Opt.empty[Test]
      }
    }

  }

  def firstLevelTest(chain1: Chain[G]): Test =
    new Test(0, chain2, Group[G].id)

}
