package net.alasc.bsgs.internal

import scala.annotation.tailrec

import spire.algebra.{Eq, Group}
import spire.util.Opt
import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BaseChange, BaseGuide, BaseGuideIterator, BaseGuideSet, BaseSwap, Chain, ChainRec, GrpChain, GrpChainPermutationAction, Node, SchreierSims, SubgroupDefinition, SubgroupSearch, SubgroupTest, Term}

import metal.syntax._
import spire.std.int._
import spire.syntax.action._
import spire.syntax.group._
import spire.syntax.eq._
import spire.syntax.cfor.cforRange
import net.alasc.finite.Grp
import net.alasc.partitions.Partition
import net.alasc.perms.orbits.Sets.compareBoolean
import net.alasc.perms.{MutableOrbit, Perm, orbits}
import net.alasc.syntax.group._


object Conjugation {

  case class BaseGuideOrbits[G, A <: PermutationAction[G] with Singleton](g: G)(implicit A: A) extends BaseGuide {

    class Iter(var orbitStart: Int = -1, var prevPoint: Int = -1) extends BaseGuideIterator {

      def hasNext = true

      def next(beta: Int, easyPoints: collection.Set[Int], isFixed: (Int) => Boolean): Int =
        if (orbitStart == -1) {
          // we are starting a new orbit
          orbitStart = beta
          prevPoint = beta
          beta
        } else {
          var next = prevPoint <|+| g
          while (isFixed(next) && next != orbitStart)
            next = next <|+| g
          if (next == orbitStart) {
            orbitStart = beta
            prevPoint = beta
            beta
          } else next
        }

    }

    def iterator: BaseGuideIterator = new Iter

    def fullBase: Seq[Int] = {
      val movedPoints = metal.mutable.FixedBitSet.fromIterable(A.movedPoints(g))
      val base = metal.mutable.Buffer.empty[Int]
      while (movedPoints.nonEmpty) {
        val k = movedPoints.min
        var i = A.actr(k, g)
        while (i != k) {
          base += i
          movedPoints -= k
          i = A.actr(i, g)
        }
      }
      base.result().toScala
    }

  }

  /** Definition of a centralizer subgroup following Butler, 1982, Computing in Permutation and Matrix Groups II */
  case class Centralizer[G:Eq:Group, A <: PermutationAction[G] with Singleton](f: G)(implicit val action: A) extends SubgroupDefinition[G, A] {
    def baseGuideOpt: Opt[BaseGuide] = Opt(new BaseGuideOrbits[G, A](f))

    /** Returns whether the coset described by the element `g` is in the group. */
    def inSubgroup(g: G): Boolean = (f |+| g) === (g |+| f)

    class Test(sizes: Array[Int], prevBeta: Int, prevImage: Int) extends SubgroupTest[G, A] {
      def test(b: Int, orbitImage: Int, currentG: G, node: Node[G, A]): Opt[Test] =
        if (prevBeta != -1 && node.beta == prevBeta <|+| f) {
          if (orbitImage == prevImage <|+| f)
            Opt(new Test(sizes, node.beta, orbitImage))
          else
            Opt.empty[Test]
        } else {
          if (sizes(node.beta) == sizes(orbitImage))
            Opt(new Test(sizes, node.beta, orbitImage))
          else
            Opt.empty[Test]
        }
    }

    /** Returns the test for the first level of `guidedChain`.
      * `guidedChain` must be using `action` and have a
      * base guided by `baseGuideOpt`.
      */
    def firstLevelTest(guidedChain: Chain[G, A]): SubgroupTest[G, A] = {
      val n = action.largestMovedPoint(guidedChain.strongGeneratingSet).getOrElse(-1) + 1
      val part = Partition.fromPermutation(n, f)
      val sizes = Array.tabulate(n)(i => part.blockFor(i).size)
      new Test(sizes, -1, -1)
    }
  }

  /** Returns, if it exists, g such that g1 g = g g2.
    * Implements backtrack search in the sense of Butler, 1982, Computing in Permutation and Matrix Groups II
    * Action must be faithful
    *
    * @param grp   The parent group
    * @param g1    First element
    * @param g2    Second element
    * @param g2CentralizerSubgroup (Optional) subgroup of the centralizer of g2 in grp
    * @tparam A    A faithful action
    */
  def findConjugation[G, A <: PermutationAction[G] with Singleton](grp: GrpChain[G, A], g1: G, g2: G, g2CentralizerSubgroup: GrpChain[G, A])
                                                            (implicit baseChange: BaseChange, baseSwap: BaseSwap): Opt[G] = {
    import grp.{classTag, equ, group}
    implicit val action: A = grp.action
    if (grp.isTrivial) {
      if (g1.isId && g2.isId) return Opt(Group[G].id)
      else return Opt.empty[G]
    }
    require(grp.action.isFaithful)
    val baseGuide = new BaseGuideOrbits[G, A](g1)
    val guidedChain: Chain[G, A] = grp match {
      case lhs: GrpChainConjugated[G, A] =>
        import lhs.{g, gInv, originalChain}
        val mut = originalChain.mutableChain
        mut.conjugate(g, gInv)
        baseChange.changeBase(mut, grp.kernel, baseGuide)
        mut.toChain()
      case _ =>
        if (baseGuide.isSatisfiedBy((grp.chain))) grp.chain else {
          val mut = grp.chain.mutableChain
          baseChange.changeBase(mut, grp.kernel, baseGuide)
          mut.toChain()
        }
    }
    val n = grp.largestMovedPoint(action).getOrElseFast(-1) + 1 // cannot be 0, the group is no the identity
    val part1 = Partition.fromPermutation(n, g1)
    val part2 = Partition.fromPermutation(n, g2)
    val sizes1 = Array.tabulate(n)(i => part1.blockFor(i).size)
    val sizes2 = Array.tabulate(n)(i => part2.blockFor(i).size)
    val sOrbit = MutableOrbit.forSize(n)

    // Implements breadth-first search in the cosets `symGrp \ grp`, filtering elements that do not lead to a minimal
    // lexicographic representative at each step in the stabilizer chain.
    def rec(prevBeta: Int, prevImage: Int, curG: G, curChainGrp: Chain[G, A], curK: GrpChain[G, A], sOrbit: MutableOrbit): Opt[G] =
    curChainGrp match {
      case node: Node[G, A] if prevBeta != -1 && (prevBeta <|+| g1) == node.beta =>
        // we look for prevImage <|+| g2 == b <|+| prevG
        // does it exist b <|+| curG == prevImage <|+| g2
        // b == prevImage <|+| g2 <|+| curG.inverse
        val newImage = prevImage <|+| g2
        val b = curG |+|> newImage
        if (node.inOrbit(b)) {
          val bg = b <|+| curG
          rec(node.beta, newImage, node.u(b) |+| curG, node.next, GrpChain.stabilizer(curK, newImage), sOrbit)
        } else Opt.empty[G]
      case node: Node[G, A] =>
        val orbitSize = sizes1(node.beta)
        node.foreachOrbit { b =>
          val bg = b <|+| curG
          if (sizes2(bg) == orbitSize && orbits.Points.isSmallestInOrbit(bg, curK.generators, Opt(sOrbit))(implicitly, spire.std.int.IntAlgebra)) {
            rec(node.beta, bg, node.u(b) |+| curG, node.next, GrpChain.stabilizer(curK, bg), sOrbit) match {
              case Opt(g) => return Opt(g)
              case _ =>
            }
          }
        }
        Opt.empty[G]
      case _: Term[G, A] => if ((g1 |+| curG) === (curG |+| g2)) Opt(curG) else Opt.empty[G]
    }

    rec(-1, -1, Group[G].id, guidedChain, g2CentralizerSubgroup, sOrbit)
  }

}
