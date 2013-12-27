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
    val g = PGroup.fromGenerators(id, List(g1, g2), List(8,7,6,5,4,3,2,1))
    case class PermutableIntSeq(val permutableSequence: IndexedSeq[Int]) extends OrderedPermutable[PermutableIntSeq, Perm, Int] {
      def permutedBy(p: Perm) = {
        val inv = p.inverse
        PermutableIntSeq(
          IndexedSeq((0 until seq.size).map(k => seq(inv.image(Dom._0(k))._0)):_*))
      }
      val permutableGroup = g
      val permutableOrdering = scala.math.Ordering.Int
    }
    val p = PermutableIntSeq(seq)
    val reps = (0 until p.representatives.size.toInt).map(p.representatives(_))
    val iter = p.representatives.iterator.toSeq
    assert(reps == iter)
    assert((reps zip reps.tail).forall {
      case (a, b) => a < b
    })
  }
}
