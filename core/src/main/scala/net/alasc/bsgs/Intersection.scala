package net.alasc.bsgs

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.syntax.action._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra.PermutationAction

/** Defines the subgroup which intersect `chain2`, given in faithful action `action`. */
case class Intersection[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (val chain2: Chain[G, F])
  (implicit val action: F, baseChange: BaseChange, schreierSims: SchreierSims) extends SubgroupDefinition[G, F] {

  def baseGuideOpt = Opt(BaseGuideSeq(chain2.base))

  def inSubgroup(g: G): Boolean = chain2.sift(g) match {
    case Opt(sifted) => sifted.isId
    case _ => false
  }

  class Test(level: Int, currentChain2: Chain[G, F], prev2Inv: G) extends SubgroupTest[G, F] {

    def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, F]): Opt[Test] = {
      val b2 = orbitImage <|+| prev2Inv
      currentChain2 match {
        case node2: Node[G, F] if node2.inOrbit(b2) =>
          Opt(new Test(level + 1, node2.next, prev2Inv |+| node2.uInv(b2)))
        case _ if node.beta == b2 =>
          Opt(new Test(level + 1, currentChain2, prev2Inv))
        case _ =>
          Opt.empty[Test]
      }
    }

  }

  def firstLevelTest(chain1: Chain[G, F]): Test =
    new Test(0, chain2, Group[G].id)

}
