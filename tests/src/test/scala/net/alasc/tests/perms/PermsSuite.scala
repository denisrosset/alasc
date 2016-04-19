package net.alasc.tests
package perms

import spire.math.SafeLong

import net.alasc.named.Symmetric
import net.alasc.perms.{Perm, Perms}
import net.alasc.perms.default._

class PermsSuite extends AlascSuite {

  test("Perms(n).iterator, Perms(n).apply(...) and Sym(n).elements.toSet return the same elements for n = 2,3,4,5,6") {
    for (n <- 2 to 6) {
      val sym = Symmetric[Perm](n)
      val perms = Perms(n)
      val seqFromPermsIterator = perms.iterator.toSeq
      import net.alasc.optional.lexPermutationOrder._
      (seqFromPermsIterator.iterator zip seqFromPermsIterator.iterator.drop(1)).forall { case (x, y) => x < y } shouldBe true
      val seqFromPermsApply = (0 until perms.size.toInt).map(perms(_)).toSeq
      val setFromSym = sym.iterator.toSet
      for (i <- 0 until perms.size.toInt) {
        val perm = perms(i)
        perms.indexOf(perm).getOrElse(sys.error("Perm should be inside")) should === (SafeLong(i))
      }
      seqFromPermsApply shouldBe (seqFromPermsIterator)
      seqFromPermsApply.toSet shouldBe (setFromSym)
    }
  }

}
