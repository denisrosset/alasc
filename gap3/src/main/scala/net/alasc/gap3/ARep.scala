package net.alasc.gap3

import java.io.{ByteArrayInputStream, File}

import net.alasc.algebra.{PermutationAction}
import net.alasc.finite.{Grp, GrpGroup, Rep}
import net.alasc.perms.{Cycles, FaithfulPermRep, Perm, PermRep}
import net.alasc.syntax.all._
import cyclo.Cyclo
import scalin.immutable.{DenseMat, Mat, Vec}
import scalin.syntax.all._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.action._
import spire.syntax.group._
import spire.std.int._

import fastparse.core.Parsed

trait MonRep[G] extends Rep[G, Cyclo] {

  def mon(g: G): Mon

  def apply(g: G) = mon(g).mat

}

trait ARep[G] extends Rep[G, Cyclo] {

  def grp: Grp[G]

  def represents(g: G) = grp.contains(g)

}

/*
object ARep {

  val gapDir = new File("/home/rossetd0/software/gap3")
  val gapCmd = "/home/rossetd0/software/gap3/bin/gap-static-linux-i686"
  val gapOptions = "-q"

  def full[G, R <: FaithfulPermRep[G, _] with Singleton]
  (grp: Grp[Rep.Of[G, R]])(implicit witness: shapeless.Witness.Aux[R], permutation: Permutation[Rep.Of[G, R]]): String = {
    val R = witness.value
    val n = R.dimension
    val perms: Iterable[Perm] = grp.generators.map(_.toPermutation[Perm])
    def gapPermString(p: Perm): String = p.toPermutation[Cycles].seq.map(_.seq.map(_ + 1).mkString("(",",",")")).mkString
    val gapGenerators = perms.map(gapPermString).mkString("[", ",", "]")
    val n1 = n + 1
    val imp = """LoadPackage("arep");; """
    val gapInputString = imp + s"G := GroupWithGenerators( $gapGenerators );; A := ARepByImages(G, $gapGenerators, $n);; D := DecompositionMonRep(A);"
    val gapInput = new ByteArrayInputStream(gapInputString.getBytes("UTF-8"))
    import scala.sys.process._
    val out = new java.io.ByteArrayOutputStream
    Process(gapCmd :: gapOptions :: Nil, Some(gapDir)) #< gapInput #> out !
    val gapOutput = new String(out.toByteArray(), "UTF-8")
    out.close()
    gapOutput
  }

  def conjugation[G, R <: FaithfulPermRep[G, _] with Singleton]
    (grp: Grp[Rep.Of[G, R]])(implicit witness: shapeless.Witness.Aux[R], permutation: Permutation[Rep.Of[G, R]]): AMat = {
    val R = witness.value
    val n = R.dimension
    val perms: Iterable[Perm] = grp.generators.map(_.toPermutation[Perm])
    def gapPermString(p: Perm): String = p.toPermutation[Cycles].seq.map(_.seq.map(_ + 1).mkString("(",",",")")).mkString
    val gapGenerators = perms.map(gapPermString).mkString("[", ",", "]")
    val n1 = n + 1
    val imp = """LoadPackage("arep");; """
    val gapInputString = imp + s"G := GroupWithGenerators( $gapGenerators );; A := ARepByImages(G, $gapGenerators, $n);; D := DecompositionMonRep(A);; D.conjugation.element;"
    val gapInput = new ByteArrayInputStream(gapInputString.getBytes("UTF-8"))
    import scala.sys.process._
    val out = new java.io.ByteArrayOutputStream
    Process(gapCmd :: gapOptions :: Nil, Some(gapDir)) #< gapInput #> out !
    val gapOutput = new String(out.toByteArray(), "UTF-8")
    val gapOutputRemoveWarnings = gapOutput.split("\n").filterNot(_.head == '#').mkString("\n")
    out.close()
    val Parsed.Success(mat, _) = GapOutput.aMat.parse(gapOutputRemoveWarnings)
    mat
  }

  def conjugate[G](rep: Rep[G, Cyclo], mat: Mat[Cyclo]): Rep[G, Cyclo] = new Rep[G, Cyclo] {
    import scalin.immutable.dense._
     def apply(g: G): Mat[Cyclo] = mat.inverse * rep(g) * mat
     def represents(g: G): Boolean = rep.represents(g)
     def dimension: Int = rep.dimension
  }

}

case class TrivialPermARep[G](grp: Grp[G]) extends ARep[G] /*with PermRep[G]*/ {
  def dimension = 1
  import scalin.immutable.dense._
  def apply(g: G) = eye[Cyclo](dimension)
}

case class TrivialMonARep[G](grp: Grp[G]) extends ARep[G] with MonRep[G] {
  def dimension = 1
  import scalin.immutable.dense._
  def mon(g: G) = Mon(Perm.id, vec(Cyclo.one))
}

case class TrivialMatARep[G](grp: Grp[G]) extends ARep[G] {
  def dimension = 1
  import scalin.immutable.dense._
  def apply(g: G) = eye[Cyclo](dimension)
}

/*
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
}*/

trait ARepByImages[G] extends ARep[G] {

  require(grp.generators.size === list.size)

  type Image

  def hint: Option[String]

  def list: Seq[Image]

}

case class ARepByImagesMon[G](grp: Grp[G], list: Seq[Mon], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G] {
  type Image = Mon
  def apply(g: G) = ???
}

case class ARepByImagesPerm[G](grp: Grp[G], list: Seq[Perm], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G] {
  type Image = Perm
  def apply(g: G) = ???
}


case class ARepByImagesMat[G](grp: Grp[G], list: Seq[Mat[Cyclo]], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G] {
  type Image = Mat[Cyclo]
  def apply(g: G) = ???
}
/*
case class ARepByImagesAMat[G](generators: Seq[G], list: Seq[AMat], dimension: Int, hint: Option[String] = None)
  extends ARepByImages[G, AMat] {
  def apply(g: G) = ???
}*/
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
  require(r.tail.forall(rep => rep.grp === r.head.grp))
  def grp = r.head.grp
  import scalin.immutable.dense._
  def dimensions = r.map(_.dimension)
  def dimension = r.foldLeft(0) { (d, r) => d + r.dimension }
  def apply(g: G) = r.map(_.apply(g)).foldLeft(zeros[Cyclo](0, 0)) { (sm, m) => directSum(sm, m) }
}

/*
case class InnerTensorProductARep[G](r: ARep[G]*) extends ARep[G] {
  require(r.nonEmpty)
  require(r.tail.forall(rep => rep.grp == r.head.grp))
  def grp = r.head.grp
  import scalin.immutable.dense._
  def dimension = r.foldLeft(1) { (d, r) => d * r.dimension }
  def apply(g: G) = r.map(_.apply(g)).foldLeft(eye[Cyclo](1)) { (pd, m) => pd kron m }
}
*/
*/