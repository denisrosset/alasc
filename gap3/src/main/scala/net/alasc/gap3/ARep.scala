package net.alasc.gap3

import net.alasc.algebra.{Permutation, PermutationAction}
import net.alasc.finite.{Grp, Rep}
import net.alasc.perms.{Cycles, Perm}
import net.alasc.syntax.all._
import cyclo.Cyclo
import scalin.immutable.{DenseMat, Mat, Vec}
import scalin.syntax.all._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.action._
import spire.syntax.group._
import spire.std.int._

trait ARep[G] extends Rep[G, Cyclo] {
  def grp: Grp[G]
  def represents(g: G) = grp.contains(g)
}

object ARep {

  def apply[G:Permutation](grp: Grp[G]): String = {
    val n = PermutationAction.largestMovedPoint(grp.generators).getOrElse(-1) + 1
    val perms: Iterable[Perm] = grp.generators.map(_.toPermutation[Perm])
    def gapPermString(p: Perm): String = p.toPermutation[Cycles].seq.map(_.seq.map(_ + 1).mkString("(",",",")")).mkString
    val gapGenerators = perms.map(gapPermString).mkString("[", ",", "]")

    val n1 = n + 1
    val imp = """LoadPackage("arep");; """
    val gapString = imp + s"G := GroupWithGenerators( $gapGenerators );; A := ARepByImages(G, $gapGenerators, $n);; D := DecompositionMonRep(A);; D.conjugation.element;"

    gapString
  }

  def conjugate[G](rep: Rep[G, Cyclo], mat: Mat[Cyclo]): Rep[G, Cyclo] = new Rep[G, Cyclo] {
    import scalin.immutable.dense._
     def apply(g: G): Mat[Cyclo] = mat.inverse * rep(g) * mat
     def represents(g: G): Boolean = rep.represents(g)
     def dimension: Int = rep.dimension
  }

}

case class TrivialPermARep[G](grp: Grp[G], dimension: Int) extends ARep[G] {
  import scalin.immutable.dense._
  def apply(g: G) = eye[Cyclo](dimension)
}

case class TrivialMonARep[G](grp: Grp[G], dimension: Int) extends ARep[G] {
  import scalin.immutable.dense._
  def apply(g: G) = eye[Cyclo](dimension)
}

case class TrivialMatARep[G](grp: Grp[G], dimension: Int) extends ARep[G] {
  import scalin.immutable.dense._
  def apply(g: G) = eye[Cyclo](dimension)
}

case class RegularARep[G](grp: Grp[G]) extends ARep[G] {
  import grp.group
  require(grp.order.isValidInt)
  val dimension = grp.order.toInt
  val elementSeq = grp.iterator.toVector
  val elementMap = elementSeq.zipWithIndex.toMap
  def apply(g: G) = {
    import scalin.mutable.dense._
    val res = zeros[Cyclo](dimension, dimension)
    cforRange(0 until dimension) { c =>
      val r = elementMap(g |+| elementSeq(c))
      res(r, c) := Cyclo.one
    }
    res.result()
  }
}

case class NaturalARep[G:Permutation](grp: Grp[G], dimension: Int) extends ARep[G] {
  def apply(g: G) = {
    import scalin.mutable.dense._
    val res = zeros[Cyclo](dimension, dimension)
    cforRange(0 until dimension) { i =>
      res(i, i <|+| g) := Cyclo.one
    }
    res.result()
  }
}

trait ARepByImages[G, Image] extends ARep[G] {

  def hint: Option[String]

  def list: Seq[Image]

}

case class ARepByImagesMon[G](grp: Grp[G], list: Seq[Mon], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G, Mon] {
  def apply(g: G) = ???
}

case class ARepByImagesPerm[G](grp: Grp[G], list: Seq[Perm], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G, Perm] {
  def apply(g: G) = ???
}

case class ARepByImagesMat[G](grp: Grp[G], list: Seq[Mat[Cyclo]], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G, Mat[Cyclo]] {
  def apply(g: G) = ???
}

case class ARepByImagesAMat[G](grp: Grp[G], list: Seq[AMat], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G, AMat] {
  def apply(g: G) = ???
}

case class ConjugateARep[G](rep: ARep[G], a: AMat, hint: Option[String] = None) extends ARep[G] {
  import scalin.immutable.dense._
  lazy val aVal = a.value
  lazy val aValInv = aVal.inverse
  def grp = rep.grp
  require(rep.dimension == a.dimension)
  def dimension = rep.dimension
  def apply(g: G) = aValInv * rep(g) * aVal
}

case class DirectSumARep[G](r: ARep[G]*) extends ARep[G] {
  require(r.nonEmpty)
  require(r.tail.forall(rep => rep.grp == r.head.grp))
  def grp = r.head.grp
  import scalin.immutable.dense._
  def dimension = r.foldLeft(0) { (d, r) => d + r.dimension }
  def apply(g: G) = r.map(_.apply(g)).foldLeft(zeros[Cyclo](0, 0)) { (sm, m) => directSum(sm, m) }
}

case class InnerTensorProductARep[G](r: ARep[G]*) extends ARep[G] {
  require(r.nonEmpty)
  require(r.tail.forall(rep => rep.grp == r.head.grp))
  def grp = r.head.grp
  import scalin.immutable.dense._
  def dimension = r.foldLeft(1) { (d, r) => d * r.dimension }
  def apply(g: G) = r.map(_.apply(g)).foldLeft(eye[Cyclo](1)) { (pd, m) => pd kron m }
}
