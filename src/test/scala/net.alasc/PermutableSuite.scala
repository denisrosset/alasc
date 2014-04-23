package net.alasc

import org.scalatest.FunSuite
import collection.immutable.IndexedSeq

class PermutableSuite extends FunSuite {
  test("Simple sequence") {
    import Dom.OneBased._
    val seq = IndexedSeq(1,2,2,2,2,3,4,5)
    val g1 = Perm(8)(1,3)
    val g2 = Perm(8)(1,3,5,7)(2,4,6,8)
    val id = Perm(8)
    val g = Group.fromGenerators(TrivialAction(id), List(g1, g2), List(8,7,6,5,4,3,2,1))
    case class PermutableIntSeq(val integerSeq: IndexedSeq[Int])
        extends Permutable[PermutableIntSeq, Perm] with PermutableImpl[PermutableIntSeq, Perm] with Ordered[PermutableIntSeq] {

      def compare(that: PermutableIntSeq): Int = {
        integerSeq.indices.foreach { i =>
          val c = integerSeq(i).compare(that.integerSeq(i))
          if (c != 0)
            return c
        }
        0
      }

      def permutedBy(p: Perm) = {
        val inv = p.inverse
        PermutableIntSeq(
          IndexedSeq((0 until integerSeq.size).map(k => integerSeq(inv.image(Dom._0(k))._0)):_*))
      }

      object BruteForcePerms extends BruteForcePermutations {
        val baseGroup = g
      }
      object WithoutSymmetrySubgroupPerms extends WithoutSymmetrySubgroupPermutations {
        val baseGroup = g
      }
      object BigSeqPerms extends BigSeqPermutations {
        val baseGroup = g
      }
    }
    val p = PermutableIntSeq(seq)
    assert (p.BruteForcePerms.minimalRepresentative == 
      p.WithoutSymmetrySubgroupPerms.minimalRepresentative)
    assert (p.BruteForcePerms.minimalRepresentative == 
      p.BigSeqPerms.minimalRepresentative)
    val reps = (0 until p.BigSeqPerms.size.toInt).map(p.BigSeqPerms(_))
    val iter = p.BigSeqPerms.iterator.toSeq
    assert(reps == iter)
    assert((reps zip reps.tail).forall {
      case (a, b) => a < b
    })
  }
}
