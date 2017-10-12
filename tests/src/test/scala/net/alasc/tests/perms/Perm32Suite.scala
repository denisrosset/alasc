package net.alasc.tests
package perms

import net.alasc.laws.{AnyRefLaws, PermutationActionLaws}
import net.alasc.perms.sized.Perm32
import net.alasc.laws.Permutations._
import org.scalacheck._

class Perm32Suite extends AlascSuite {

  checkAll("Perm32", AnyRefLaws[Perm32]._eq)
  checkAll("Perm32", PermutationActionLaws[Perm32].faithfulPermutationAction)

}
