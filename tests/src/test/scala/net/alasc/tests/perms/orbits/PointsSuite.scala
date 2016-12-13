package net.alasc.tests
package perms.orbits

import scala.annotation.tailrec

import org.scalacheck.Gen

import net.alasc.algebra.PermutationAction
import net.alasc.domains.Domain
import net.alasc.laws.{Doms, Grps, Permutations}
import net.alasc.perms.Perm
import net.alasc.perms.orbits.Points
import net.alasc.perms.default._
import spire.syntax.action._
import spire.std.int._
import net.alasc.lexico.lexSetIntOrder._
import spire.std.boolean._

class PointsSuite extends AlascSuite {

  import Permutations.permutationGrp

  val domain = Domain(20)

  @tailrec final def slowOrbit[G:PermutationAction](set: Set[Int], generators: Iterable[G]): Set[Int] = {
    val newSet = collection.mutable.BitSet.empty ++ set
    set.foreach { k =>
      generators.foreach { g =>
        newSet += k <|+| g
      }
    }
    if (newSet.size > set.size) slowOrbit(newSet.to[Set], generators) else set
  }

  test("Compute orbit of points") {
    forAll(Doms.inDomain(domain), permutationGrp(domain)) { (point, grp) =>
      Points(point.value, grp.generators) should === (slowOrbit(Set(point.value), grp.generators))
    }
  }

  test("isSmallest") {
    forAll(Doms.inDomain(domain), permutationGrp(domain)) { (point, grp) =>
      Points.isSmallestInOrbit(point.value, grp.generators) should === (point.value === slowOrbit(Set(point.value), grp.generators).min)
    }
  }

}
