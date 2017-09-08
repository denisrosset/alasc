package net.alasc.laws

import scala.reflect.ClassTag
import spire.algebra._
import org.typelevel.discipline.Laws
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Prop._
import spire.syntax.order._
import spire.syntax.group._
import spire.std.int._
import net.alasc.blackbox.{BBGrpGroup, BBGrpPermutationAction, BBGrpStructure}
import net.alasc.bsgs.FixingPartition
import net.alasc.finite._

object GrpLaws {

  def apply[G:Arbitrary:ClassTag:Eq:Group](implicit gg: Arbitrary[Grp[G]]) = new GrpLaws[G] {
    def classTag = implicitly
    def equ = implicitly
    def group = implicitly
    def arbG = implicitly
    def arbGrpG = implicitly
  }

}


trait GrpLaws[G] extends Laws {

  implicit def classTag: ClassTag[G]
  implicit def equ: Eq[G]
  implicit def group: Group[G]
  implicit def arbG: Arbitrary[G]
  implicit def arbGrpG: Arbitrary[Grp[G]]

  def grpWithoutHashCodeEquals(implicit grpGroup: GrpGroup[G], grpStructure: GrpStructure[G]) =
    new GrpProperties(
      name = "grpBase",
      parent = None,

      "order / iterator.size" -> forAll( (grp: Grp[G]) =>
        grp.iterator.size == grp.order.toInt
      ),

      "iterator / contains" -> forAll( (grp: Grp[G]) =>
        grp.iterator.forall(grp.contains)
      ),

      "isTrivial" -> forAll( (grp: Grp[G]) =>
        grp.isTrivial == (grp.order == 1)
      ),

      "generators" -> forAll { (grp: Grp[G]) =>
        val newGrp: Grp[G] = Grp(grp.generators.toSeq: _*)
        newGrp === grp
      },

      "conjugatedBy" -> forAll { (grp: Grp[G], h: G) =>
        val hInv = h.inverse
        val conjGrp = grp.conjugatedBy(h)
        forAll(Grps.genRandomElement(grp)) { g =>
          conjGrp.contains(hInv |+| g |+| h)
        } && grp.order == conjGrp.order
      },

      "conjugatedBy composition" -> forAll { (grp: Grp[G], h1: G, h2: G) =>
        grp.conjugatedBy(h1).conjugatedBy(h2) === grp.conjugatedBy(h1 |+| h2)
      },

      "randomElement" -> forAll { (grp: Grp[G]) =>
        forAll(Grps.genRandomElement(grp))( g => grp.contains(g) )
      },

      "hasSubgroup / isSubgroupOf" -> forAll { (grp: Grp[G]) =>
        forAll(Grps.genSubgrp(grp))( subGrp => subGrp.isSubgroupOf(grp) && grp.hasSubgroup(subGrp) )
      },

      "union" -> forAll { (grp1: Grp[G], grp2: Grp[G]) =>
        val u = grp1 union grp2
        grp1.isSubgroupOf(u) && grp2.isSubgroupOf(u)
      },

      "smallGeneratingSet" -> forAll { (grp: Grp[G]) =>
        val newGrp: Grp[G] = Grp.fromGenerators(grp.smallGeneratingSet)
        newGrp === grp
      }

    )

  val smallGrpOrder = 32678 // maximal size of a group whose elements can be all enumerated by brute force in reasonable time

  def smallGrp(implicit ev: GrpGroup[G]): Gen[Grp[G]] =
    arbGrpG.arbitrary.flatMap { grp => Grps.forceSmallGroup(grp, smallGrpOrder) }

  def grp(implicit grpGroup: GrpGroup[G], grpStructure: GrpStructure[G]) =
    new GrpProperties(
      name = "grp",
      parent = Some(grpWithoutHashCodeEquals),

      "intersect" -> forAll { (grp1: Grp[G], grp2: Grp[G]) =>
        val int = grp1 intersect grp2
        int.isSubgroupOf(grp1) && int.isSubgroupOf(grp2)
      },

      "intersect (bb)" -> forAll(smallGrp, smallGrp) { (grp1, grp2) =>
        testBBEquals[Set[G], GrpGroup[G]]( _.intersect(grp1, grp2).iterator.toSet )
      },

      // add BB intersect
      "leftCosetsBy" -> forAll(smallGrp) { grp =>
        forAll(Grps.genSubgrp(grp)) { subGrp =>
          val cosets = grp.leftCosetsBy(subGrp)
          val setOfSets = cosets.iterator.map(coset => coset.iterator.toSet).toSet
          val sumSizes = setOfSets.foldLeft(0)(_ + _.size)
          val union = setOfSets.flatten
          (sumSizes == grp.order) && (union == grp.iterator.toSet)
        }
      },

      "rightCosetsBy" -> forAll(smallGrp) { grp =>
        forAll(Grps.genSubgrp(grp)) { subGrp =>
          val cosets = grp.rightCosetsBy(subGrp)
          val setOfSets = cosets.iterator.map(coset => coset.iterator.toSet).toSet
          val sumSizes = setOfSets.foldLeft(0)( _ + _.size )
          val union = setOfSets.flatten
          (sumSizes == grp.order) && (union == grp.iterator.toSet)
        }
      }

    )

  class BB[GG](val gg: GG)

  implicit val bbGrpGroup: BB[GrpGroup[G]] = new BB(new BBGrpGroup[G])

  implicit val bbGrpPermutationAction: BB[GrpPermutationAction[G]] = new BB(new BBGrpPermutationAction[G])

  implicit val bbGrpStructure: BB[GrpStructure[G]] = new BB(new BBGrpStructure[G]()(implicitly, implicitly, new BBGrpGroup[G], implicitly))

  def testBBEquals[R, GG](f: GG => R)(implicit gg: GG, bb: BB[GG]): Boolean = (f(gg) == f(bb.gg))

  def testBBEq[R:Eq, GG](f: GG => R)(implicit gg: GG, bb: BB[GG]): Boolean = (f(gg) === f(bb.gg))

  def grpPermutationAction(implicit gg: GrpGroup[G], gs: GrpStructure[G], gpa: GrpPermutationAction[G], fpab: FaithfulPermutationActionBuilder[G]) =
    new GrpProperties(
      name = "grp",
      parent = Some(grp),

      "kernel" -> forAll(smallGrp) { grp =>
        forAll { pab: PermutationActionBuilder[G] =>
          val action = pab(grp)
          testBBEquals[Set[G], GrpPermutationAction[G]]( _.kernel(grp, action).iterator.toSet )
        }
      },

      "lexElements" -> forAll(smallGrp) { grp =>
        val action = fpab(grp) // faithful action, so lexElements always returns Opt(seq)
        testBBEquals[Seq[G], GrpPermutationAction[G]]( _.lexElements(grp, action).get.toIndexedSeq )
      },

      "fixingPartition" -> forAll { (grp: Grp[G], pab: PermutationActionBuilder[G]) =>
        val action = pab(grp)
        forAll(Partitions.sized) { partition =>
          grp.fixingPartition(action, partition).generators
            .forall( g => FixingPartition.partitionInvariantUnder(partition, action, g))
        }
      },

      "fixingPartition bb test" -> forAll(smallGrp, Partitions.sized) { (grp, partition) =>
        forAll { pab: PermutationActionBuilder[G] =>
            val action = pab(grp)
          testBBEquals[Set[G], GrpPermutationAction[G]]( _.fixingPartition(grp, action, partition).iterator.toSet )
          }
        },

      "stabilizer(b)" -> forAll { (grp: Grp[G], b: Dom, pab: PermutationActionBuilder[G]) =>
        val action = pab(grp)
        grp.stabilizer(action, b).generators.forall(g => action.actr(b, g) === b.value)
      },

      "stabilizer(b) bb test" -> forAll(smallGrp) { grp =>
        forAll { (b: Dom, pab: PermutationActionBuilder[G]) =>
          val action = pab(grp)
          testBBEquals[Set[G], GrpPermutationAction[G]]( _.stabilizer(grp, action, b).iterator.toSet )
        }
      },

      "pointwiseStabilizer" -> forAll { (grp: Grp[G], pab: PermutationActionBuilder[G], domSet: Set[Dom]) =>
        val action = pab(grp)
        val set = domSet.map(_.value)
        grp.pointwiseStabilizer(action, set).generators.forall(g => set.forall(b => action.actr(b, g) == b))
      },

      "pointwiseStabilizer bb test" -> forAll(smallGrp) { grp =>
        forAll { (pab: PermutationActionBuilder[G], domSet: Set[Dom]) =>
          val action = pab(grp)
          val set = domSet.map(_.value)
          testBBEquals[Set[G], GrpPermutationAction[G]]( _.pointwiseStabilizer(grp, action, set).iterator.toSet )
        }
      },

      "setwiseStabilizer" -> forAll { (grp: Grp[G], pab: PermutationActionBuilder[G], domSet: Set[Dom]) =>
        val action = pab(grp)
        val set = domSet.map(_.value)
        grp.setwiseStabilizer(action, set).generators.forall(g => set.map(b => action.actr(b, g)) == set)
      },

      "setwiseStabilizer bb test" -> forAll(smallGrp) { grp =>
        forAll { (pab: PermutationActionBuilder[G], domSet: Set[Dom]) =>
          val action = pab(grp)
          val set = domSet.map(_.value)
          testBBEquals[Set[G], GrpPermutationAction[G]]( _.setwiseStabilizer(grp, action, set).iterator.toSet )
        }
      }

      /* only for Chain stuff
      "stabilizerTransversal" -> forAll(smallGrp) { grp =>
        forAll { (pab: PermutationActionBuilder[G], dom: Dom) =>
          implicit val action = pab(grp)
          val k = dom.value
          val (subgrp, trv) = gpa.stabilizerTransversal(grp, action, k)
          val stabEls1 = grp.iterator.filter(g => (k <|+| g) == k).toSet
          val stabEls2 = subgrp.iterator.toSet
          val els1 = grp.iterator.toSet
          val els2 = (for {
            g <- stabEls2
            b <- trv.orbit
          } yield g |+| trv.u(b)).toSet
          (els1 == els2) && (stabEls1 == stabEls2) && (grp.order == (subgrp.order * trv.orbitSize))

        }
      }*/
    )

  class GrpProperties(
    val name: String,
    val parent: Option[GrpProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    def bases = Seq.empty[(String, Laws#RuleSet)]
  }

}

/*
trait PermGrpLaws extends GrpLaws[Perm] {

  def equ = Perm.algebra
  def group = Perm.algebra

  val domain: Domain

  type D = Dom[domain.type]

  implicit def convertAction(implicit pa: Action[Int, Perm]): Action[D, Perm] =
    new Action[D, Perm] {
      def actr(k: D, g: Perm): D = Dom(domain)(pa.actr(k.value, g))
      def actl(g: Perm, k: D): D = Dom(domain)(pa.actl(g, k.value))
    }

  implicit def arbDom: Arbitrary[D]

  def permGrp(implicit algos: GrpGroup[Perm], permutationActionAlgos: GrpPermutationAction[Perm, Perm.algebra.type]) =
    new GrpProperties(
      name = "permGrp",
      parent = Some(grp(algos)),
/* TODO: restore a variant of find
      "find" -> forAll { (grp: Grp[Perm]) =>
        forAll(Grps.genRandomElement(grp)) { g =>
          val Opt(recov) = grp.find[Perm](Perm.algebra, g)
          recov === g
        }
      },*/

/* TODO: restore
      "someStabilizerTransversal" -> forAll { (grp: Grp[Perm]) =>
        (grp.order < 65536) ==> {
          grp.someStabilizerTransversal match {
            case Opt(subgrp, trv) =>
              val els1 = grp.iterator.toSet
              val els2 = (for {
                g <- subgrp.iterator
                b <- trv.orbit
              } yield g |+| trv.u(b)).toSet
              els1 == els2
            case _ => grp.isTrivial
          }
        }
      },

/*
      "find" -> forAll { (grp: Grp[Perm], g: Perm) =>
        grp.find(Perm.algebra, g) match {
          case Opt(h) => g === h
          case _ => !grp.contains(g)
        }
      },*/

      "base" -> forAll { (grp: Grp[Perm]) =>
        forAll(Grps.genRandomElement(grp)) { g =>
          val doesNotMoveBase = grp.base.forall(!g.movesPoint(_))
          (g.isId) == doesNotMoveBase
        }
      }

  )

}
*/*/