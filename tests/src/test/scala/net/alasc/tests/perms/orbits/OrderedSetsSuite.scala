package net.alasc.tests.perms.orbits

import net.alasc.tests.AlascSuite
import scala.annotation.tailrec
import scala.collection.immutable.BitSet

import spire.algebra.Group

import org.scalacheck.Gen

import net.alasc.algebra.PermutationAction
import net.alasc.domains.Domain
import net.alasc.laws.{Grps, Permutations, SetInts}
import net.alasc.perms.{Perm, PermAlgebra}
import net.alasc.perms.orbits.Sets
import net.alasc.perms.default._
import spire.std.int._

import net.alasc.lexico.lexSetIntOrder._
import spire.compat._

import net.alasc.std.set._
import spire.std.boolean._


class OrderedSetsSuite extends AlascSuite {

  val grpGen = Grps.conjugatedFromElements(Permutations.permForSize(8), Permutations.permForSize(200))

  @tailrec final def slowOrbit[G:Group:PermutationAction](set: Set[Set[Int]], generators: Iterable[G]): Set[Set[Int]] = {
    val newSet = collection.mutable.Set.empty[Set[Int]].empty ++ set
    set.foreach { s =>
      generators.foreach { g =>
        newSet += s <|+| g
      }
    }
    if (newSet.size > set.size) slowOrbit(newSet.to[Set], generators) else set
  }

    test("Compute smallest") {
      forAll(SetInts.forSize(200), grpGen) { (set, grp) =>
        val g = Sets.toSmallest(grp, PermAlgebra, set)
        (set <|+| g) should === (slowOrbit(Set(set), grp.generators).min)
      }
    }

    test("isSmallest") {
      forAll(SetInts.forSize(200), grpGen) { (set, grp) =>
        Sets.isSmallestInOrbit(grp, PermAlgebra, set) should === (set === slowOrbit(Set(set), grp.generators).min)
      }
    }

}
