package net.alasc.math

import org.scalatest.{FunSuite, NonImplicitAssertions, Matchers, EqMatchers}
import bsgs._
import spire.syntax.order._

class PermsSuite extends FunSuite with NonImplicitAssertions with Matchers {
  test("Perms(n).iterator, Perms(n).apply(...) and Sym(n).elements.toSet return the same elements for n = 2,3,4,5,6") {
    for (n <- 2 to 6) {
      val sym = Sym[Perm](n)
      val perms = Perms(n)
      val seqFromPermsIterator = perms.iterator.toSeq
      import net.alasc.optional.lexPermutationOrder._
      assert((seqFromPermsIterator.iterator zip seqFromPermsIterator.iterator.drop(1)).forall { case (x, y) => (x < y) })
      val seqFromPermsApply = (0 until perms.size.toInt).map(perms(_)).toSeq
      val setFromSym = sym.iterator.toSet
      for (i <- 0 until perms.size.toInt) {
        val perm = perms(i)
        assert(perms.indexOf(perm).getOrElse(sys.error("Perm should be inside")) == i)
      }
      assert(seqFromPermsApply == seqFromPermsIterator)
      assert(seqFromPermsApply.toSet == setFromSym)
    }
  }
}
