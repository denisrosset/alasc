package net.alasc

import org.scalatest.FunSuite
import spire.implicits._
import spire.algebra.Order

class PermutableSuite extends FunSuite {
  test("Simple sequence") {
    import Dom.OneBased._
    val seq = IndexedSeq(1,2,2,2,2,3,4,5)
    val g1 = Perm(8)(1,3)
    val g2 = Perm(8)(1,3,5,7)(2,4,6,8)
    val id = Perm(8)
    val g = Group.fromGenerators(TrivialPRepr(id), List(g1, g2), List(8,7,6,5,4,3,2,1))

    trait BaseLexico extends LexicoImpl[IndexedSeq[Int]] {
      type A = Int
      type F = Perm
      def baseGroup(p: IndexedSeq[Int]) = g
      implicit val action = IndexedSeqPermutingAction[Int, F]
      implicit val index: Index[Int, IndexedSeq[Int]] = IndexedSeqIndex[Int]
      implicit val order = implicitly[Order[Int]]
    }

    object BruteForce extends BaseLexico with BruteForceLexicoImpl[IndexedSeq[Int]]
    object WithoutSymmetrySubgroup extends BaseLexico 
        with WithoutSymmetrySubgroupLexicoImpl[IndexedSeq[Int]]
    object BigSeq extends BaseLexico 
        with BigSeqLexicoImpl[IndexedSeq[Int]]
    val p1 = {
      implicit val lex = BruteForce
      seq.lexFirst
    }
    val p2 = {
      implicit val lex = WithoutSymmetrySubgroup
      seq.lexFirst
    }
    val p3 = {
      implicit val lex = BigSeq
      seq.lexFirst
    }
    assert(p1 == p2)
    assert(p1 == p3)
/*
    val reps = (0 until p.BigSeqPerms.size.toInt).map(p.BigSeqPerms(_))
    val iter = p.BigSeqPerms.iterator.toSeq
    assert(reps == iter)
    assert((reps zip reps.tail).forall {
      case (a, b) => a < b
    })*/
  }
}
