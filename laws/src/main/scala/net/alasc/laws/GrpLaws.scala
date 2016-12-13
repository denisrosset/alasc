package net.alasc.laws

import spire.algebra._

import org.typelevel.discipline.Laws
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import spire.syntax.action._
import spire.syntax.order._
import spire.syntax.group._
import spire.util.Opt

import net.alasc.algebra._
import net.alasc.domains.{Dom, Domain}
import net.alasc.finite._
import net.alasc.lexico.lexPermutationOrder
import net.alasc.perms.{Perm, PermGrpBuilder}
import net.alasc.syntax.all._

object GrpLaws {

  def apply[G:Arbitrary:Eq:Group](implicit gg: Arbitrary[Grp[G]]) = new GrpLaws[G] {
    def equ = implicitly
    def group = implicitly
    def arbG = implicitly
    def arbGrpG = implicitly
  }

}

object PermGrpLaws {

  def apply(domain0: Domain)(implicit gg: Arbitrary[Grp[Perm]], p: Arbitrary[Perm], d: Arbitrary[Dom[domain0.type]]) =
    new PermGrpLaws {
      val domain: domain0.type = domain0
      def arbG = implicitly
      def arbGrpG = implicitly
      def arbDom = implicitly
  }

}

trait GrpLaws[G] extends Laws {

  implicit def equ: Eq[G]
  implicit def group: Group[G]
  implicit def arbG: Arbitrary[G]
  implicit def arbGrpG: Arbitrary[Grp[G]]

  def grpWithoutHashCodeEquals(implicit builder: GrpBuilder[G]) =
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
        val hInv1 = h1.inverse
        val hInv2 = h2.inverse
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

  def grp(implicit builder: GrpBuilder[G]) =
    new GrpProperties(
      name = "grp",
      parent = Some(grpWithoutHashCodeEquals),

      "intersect" -> forAll { (grp1: Grp[G], grp2: Grp[G]) =>
        val int = grp1 intersect grp2
        val e1 = grp1.iterator.toSet
        val e2 = grp2.iterator.toSet
        val ei = int.iterator.toSet
        int.isSubgroupOf(grp1) &&
          int.isSubgroupOf(grp2) &&
          ((e1 intersect e2) == ei)
      },

      "leftCosetsBy" -> forAll { (grp: Grp[G]) =>
        (grp.order < 65536) ==> forAll(Grps.genSubgrp(grp)) { subGrp =>
          val cosets = grp.leftCosetsBy(subGrp)
          val setOfSets = cosets.iterator.map(coset => coset.iterator.toSet).toSet
          val sumSizes = setOfSets.foldLeft(0)(_ + _.size)
          val union = setOfSets.flatten
          (sumSizes == grp.order) && (union == grp.iterator.toSet)
        }
      },

      "rightCosetsBy" -> forAll { (grp: Grp[G]) =>
        (grp.order < 65536) ==> forAll(Grps.genSubgrp(grp)) { subGrp =>
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

  def permGrp(implicit builder: PermGrpBuilder) =
    new GrpProperties(
      name = "permGrp",
      parent = Some(grp(builder)),

      "find" -> forAll { (grp: Grp[Perm]) =>
        forAll(Grps.genRandomElement(grp)) { g =>
          val Opt(recov) = grp.find[Perm](g)
          recov === g
        }
      },

      "lexElements" -> forAll { (grp: Grp[Perm]) =>
        (grp.order < 65536) ==> {
          import lexPermutationOrder._
          val lexSeq = grp.lexElements.iterator.toSeq
          val ordered = (lexSeq zip lexSeq.tail).forall { case (g1, g2) => g1 < g2 }
          (lexSeq.size == grp.order) && ordered
        }
      },

      "stabilizer(b)" -> forAll { (grp: Grp[Perm], dom: D) =>
        (grp.order < 65536) ==> {
          val k = dom.value
          val stabEls1 = grp.iterator.filter(g => (k <|+| g) == k).toSet
          val stabEls2 = grp.stabilizer(k).iterator.toSet
          stabEls1 == stabEls2
        }
      },

      "setwiseStabilizer" -> forAll { (grp: Grp[Perm], set: Set[D]) =>
        (grp.order < 65536) ==> {
          val setInt = set.map(_.value)
          def setStabilized(g: Perm) =
            setInt.forall(i => setInt.contains(i <|+| g))
          val stabEls1 = grp.iterator.filter(setStabilized(_)).toSet
          val stabEls2 = grp.setwiseStabilizer(setInt).iterator.toSet
          stabEls1 == stabEls2
        }
      },

      "pointwiseStabilizer" -> forAll { (grp: Grp[Perm], set: Set[D]) =>
        (grp.order < 65536) ==> {
          val setInt = set.map(_.value)
          def setStabilized(g: Perm) =
            setInt.forall(i => i == (i <|+| g))
          val stabEls1 = grp.iterator.filter(setStabilized(_)).toSet
          val stabEls2 = grp.pointwiseStabilizer(setInt).iterator.toSet
          stabEls1 == stabEls2
        }
      },

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

      "stabilizerTransversal" -> forAll { (grp: Grp[Perm], dom: D) =>
        (grp.order < 65536) ==> {
          val k = dom.value
          val (subgrp, trv) = grp.stabilizerTransversal(k)
          val stabEls1 = grp.iterator.filter(g => (k <|+| g) == k).toSet
          val stabEls2 = subgrp.iterator.toSet
          val els1 = grp.iterator.toSet
          val els2 = (for {
            g <- stabEls2
            b <- trv.orbit
          } yield g |+| trv.u(b)).toSet
          (els1 == els2) && (stabEls1 == stabEls2) && (grp.order == (subgrp.order * trv.orbitSize))
        }
      },

      "find" -> forAll { (grp: Grp[Perm], g: Perm) =>
        grp.find(g) match {
          case Opt(h) => g === h
          case _ => !grp.contains(g)
        }
      },

      "base" -> forAll { (grp: Grp[Perm]) =>
        forAll(Grps.genRandomElement(grp)) { g =>
          val doesNotMoveBase = grp.base.forall(!g.movesPoint(_))
          (g.isId) == doesNotMoveBase
        }
      }

  )

}
