package net.alasc.tests.perms

import net.alasc.laws.{AnyRefLaws, PermutationActionLaws}
import net.alasc.perms.sized.Perm16
import net.alasc.tests.AlascSuite
import net.alasc.laws.Permutations._
import net.alasc.perms.sized.instances._

class Perm16Suite extends AlascSuite {

  checkAll("Perm16", AnyRefLaws[Perm16]._eq)
  checkAll("Perm16", PermutationActionLaws[Perm16].faithfulPermutationAction)

}
