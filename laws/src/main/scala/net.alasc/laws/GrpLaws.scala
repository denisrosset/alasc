package net.alasc.laws

import spire.algebra._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import spire.syntax.all._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.finite._
import net.alasc.perms.Perm
import net.alasc.prep._
import net.alasc.syntax.all._

object GrpLaws {

  def apply[G:Arbitrary:Eq:Group](implicit gg: Arbitrary[Grp[G]]) = new GrpLaws[G, Grp[G]] {
    def equ = implicitly
    def group = implicitly
    def arbG = implicitly
    def arbGrpG = implicitly
  }

}

object PGrpLaws {

  def apply[G:Arbitrary:Eq:Group](implicit gg: Arbitrary[PGrp[G]], d: Arbitrary[Dom]) = new PGrpLaws[G, PGrp[G]] {
    def equ = implicitly
    def group = implicitly
    def arbG = implicitly
    def arbGrpG = implicitly
    def arbDom = implicitly
  }

}

trait GrpLaws[G, GG <: Grp[G]] extends Laws {

  implicit def equ: Eq[G]
  implicit def group: Group[G]
  implicit def arbG: Arbitrary[G]
  implicit def arbGrpG: Arbitrary[GG]

  def grp(implicit builder: GrpBuilder[G]) = 
    new GrpProperties(
      name = "grp",
      parent = None,

      "order / iterator.size" -> forAll( (grp: GG) =>
        grp.order > 2000 || grp.iterator.size == grp.order.toInt
      ),

      "iterator / contains" -> forAll( (grp: GG) =>
        grp.iterator.forall(grp.contains(_))
      ),

      "isTrivial" -> forAll( (grp: GG) =>
        grp.isTrivial == (grp.order == 1)
      ),

      "generators" -> forAll { (grp: GG) =>
        val newGrp = Grp(grp.generators.toSeq: _*)
        newGrp == grp
      },

      "conjugatedBy" -> forAll { (grp: GG, h: G) =>
        val hInv = h.inverse
        val conjGrp = grp.conjugatedBy(h, Opt(hInv))
        forAll(Grps.genRandomElement(grp)) { g =>
          conjGrp.contains(hInv |+| g |+| h)
        } && grp.order == conjGrp.order
      },

      "conjugatedBy composition" -> forAll { (grp: GG, h1: G, h2: G) =>
        val hInv1 = h1.inverse
        val hInv2 = h2.inverse
        grp.conjugatedBy(h1, Opt(hInv1)).conjugatedBy(h2, Opt(hInv2)) == grp.conjugatedBy(h1 |+| h2, Opt(hInv2 |+| hInv1))
      },

      "randomElement" -> forAll { (grp: GG) =>
        forAll(Grps.genRandomElement(grp))( g => grp.contains(g) )
      },

      "hasSubgroup / isSubgroupOf" -> forAll { (grp: GG) =>
        forAll(Grps.genSubgrp(grp))( subGrp => subGrp.isSubgroupOf(grp) && grp.hasSubgroup(subGrp) )
      },

      "union" -> forAll { (grp1: GG, grp2: GG) =>
        val u = grp1 union grp2
        grp1.isSubgroupOf(u) && grp2.isSubgroupOf(u)
      },

      "intersect" -> forAll { (grp1: GG, grp2: GG) =>
        val int = grp1 intersect grp2
        val e1 = grp1.iterator.toSet
        val e2 = grp2.iterator.toSet
        val ei = int.iterator.toSet
        int.isSubgroupOf(grp1) &&
        int.isSubgroupOf(grp2) &&
        ((e1 intersect e2) == ei)
      },

      "leftCosetsBy" -> forAll { (grp: GG) =>
        forAll(Grps.genSubgrp(grp)) { subGrp =>
          val cosets = grp.leftCosetsBy(subGrp)
          val setOfSets = cosets.iterator.map(coset => coset.iterator.toSet).toSet
          val sumSizes = setOfSets.foldLeft(0)( _ + _.size )
          val union = setOfSets.flatten
          (sumSizes == grp.order) && (union == grp.iterator.toSet)
        }
      },

      "rightCosetsBy" -> forAll { (grp: GG) =>
        forAll(Grps.genSubgrp(grp)) { subGrp =>
          val cosets = grp.rightCosetsBy(subGrp)
          val setOfSets = cosets.iterator.map(coset => coset.iterator.toSet).toSet
          val sumSizes = setOfSets.foldLeft(0)( _ + _.size )
          val union = setOfSets.flatten
          (sumSizes == grp.order) && (union == grp.iterator.toSet)
        }
      }

    )

  class GrpProperties(
    val name: String,
    val parent: Option[GrpProperties],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    def bases = Seq.empty[(String, Laws#RuleSet)]
  }

}

trait PGrpLaws[G, GG <: PGrp[G]] extends GrpLaws[G, GG] {

  implicit def arbDom: Arbitrary[Dom]

  def pGrp(implicit builder: PGrpBuilder[G]) =
    new GrpProperties(
      name = "pGrp",
      parent = Some(grp),

      "find" -> forAll { (pGrp: GG) =>
        forAll(Grps.genRandomElement(pGrp)) { g =>
          val Opt(recov) = pGrp.find(pGrp.pRep.permutationAction.to[Perm](g))
          recov === g
        }
      },

      "lexElements" -> forAll { (pGrp: GG) =>
        import net.alasc.optional.lexPermutationOrder._
        val lexSeq = pGrp.lexElements.iterator
          .map( g => pGrp.pRep.permutationAction.to[Perm](g) ).toSeq
        val ordered = (lexSeq zip lexSeq.tail).forall { case (g1, g2) => g1.to[Perm] < g2.to[Perm] }
        (lexSeq.size == pGrp.order) && ordered
      },

      "stabilizer(b)" -> forAll { (pGrp: GG, dom: Dom) =>
        implicit def action: PermutationAction[G] = pGrp.pRep.permutationAction
        val k = dom.value
        val stabEls1 = pGrp.iterator.filter(g => (k <|+| g) == k).toSet
        val stabEls2 = pGrp.stabilizer(k).iterator.toSet
        stabEls1 == stabEls2
      },

      "setwiseStabilizer" -> forAll { (pGrp: GG, set: Set[Dom]) =>
        implicit def action: PermutationAction[G] = pGrp.pRep.permutationAction
        val setInt = set.map(_.value)
        def setStabilized(g: G) =
          setInt.forall(i => setInt.contains(i <|+| g))
        val stabEls1 = pGrp.iterator.filter(setStabilized(_))
        val stabEls2 = pGrp.setwiseStabilizer(setInt).iterator.toSet
        stabEls1 == stabEls2
      },

      "pointwiseStabilizer" -> forAll { (pGrp: GG, set: Set[Dom]) =>
        implicit def action: PermutationAction[G] = pGrp.pRep.permutationAction
        val setInt = set.map(_.value)
        def setStabilized(g: G) =
          setInt.forall(i => i == (i <|+| g))
        val stabEls1 = pGrp.iterator.filter(setStabilized(_))
        val stabEls2 = pGrp.pointwiseStabilizer(setInt).iterator.toSet
        stabEls1 == stabEls2
      },

      "stabilizerTransversal" -> forAll { (pGrp: GG) =>
        pGrp.stabilizerTransversal match {
          case Opt(subgrp, trv) =>
            val els1 = pGrp.iterator.toSet
            val els2 = (for {
              g <- subgrp.iterator
              b <- trv.orbit
            } yield g |+| trv.u(b)).toSet
            els1 == els2
          case _ => pGrp.isTrivial
        }
      },

      "find" -> forAll { (pGrp: GG, g: G) =>
        pGrp.pRep.represents(g) ==> {
          val permEl = pGrp.pRep.permutationAction.to[Perm](g)
          pGrp.find(permEl) match {
            case Opt(h) => g === h
            case _ => !pGrp.contains(g)
          }
        }
      }
    )

}
