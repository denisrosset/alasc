package net.alasc.gap3

import spire.algebra.Order

import cyclo.Cyclo
import scalin.algebra.MatEngine

import net.alasc.finite.{Grp, Rep}
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder, Perm}
import net.alasc.perms._
import net.alasc.perms.default._
import Rep.syntax._
import scalin.immutable.dense._
import scalin.immutable.{DenseMat, DenseVec, Mat, Vec}
import spire.std.int._
import scalin.syntax.all._
import net.alasc.std.set._
import spire.syntax.action._

object CGLMP3 {

  val permParties = Perm(1,7)(2,8)(3,9)(4,10)(5,11)(6,12)
  val cyclicOutputPerm = Perm(1,3,2)(4,6,5)(7,8,9)(10,11,12)
  val complicated = Perm(1,6)(2,5)(3,4)(8,9)(10,12)

  val generators = Seq(permParties, cyclicOutputPerm, complicated)
  //assert(Grp(generators: _*).order == 24)

  val rep: FaithfulPermRep[Perm] = implicitly[FaithfulPermRepBuilder[Perm]].build(Seq(Perm(0,12)))
  val grpInRep = Grp(generators.map(Rep.Of(_, rep)): _*)

  implicit object SetIntOrder extends Order[Set[Int]] {
    override def compare(x: Set[Int], y: Set[Int]): Int = Order[Int].compare(x.size, y.size) match {
      case 0 if x.isEmpty && y.isEmpty => 0
      case 0 => Order[Int].compare(x.min, y.min) match {
        case 0 => compare(x - x.min, y - y.min)
        case c => c
      }
      case c => c
    }
  }

  val sdpBasisSym = implicitly[MatEngine[Int, DenseMat[Int]]].tabulate(13, 13) { (r, c) =>
    val set = Set(r, c) - 0
    if (r > 0 && c > 0 && (r-1)/3 == (c-1)/3 && (r % 3 != c % 3)) -1 else {
      grpInRep.iterator.map(Set(r, c) <|+| _).reduce(spire.math.min(_, _)(SetIntOrder)).toSeq.sorted match {
        case Seq(a, b) => a * 13 + b
        case Seq(a) => a
        case Seq() => 0
      }
    }
  }

  val sdpBasisNoSym = implicitly[MatEngine[Int, DenseMat[Int]]].tabulate(13, 13) { (r, c) =>
      val set = Set(r, c) - 0
    if (r > 0 && c > 0 && (r-1)/3 == (c-1)/3 && (r % 3 != c % 3)) -1 else {
      set.toSeq.sorted match {
        case Seq(a, b) => a * 13 + b
        case Seq(a) => a
        case Seq() => 0
      }
    }
  }

  def seqBasis(basis: Mat[Int]): Seq[Mat[Cyclo]] = {
    val v = basis(::)
    val values = Seq.tabulate(v.length)(v(_)).toSet.filter(_ >= 0).toSeq.sorted
    values.map(ind => basis.map(k => if (k == ind) Cyclo.one else Cyclo.zero))
  }

  val basisNoSym = seqBasis(sdpBasisNoSym)
  val basisSym = seqBasis(sdpBasisSym)

  val aMat = ARep(grpInRep)

  val rec = rowMajor[Int](13, 9)(
    1,  0,0,  0,0,  0,0,  0,0,
    0,  1,0,  0,0,  0,0,  0,0,
    0,  0,1,  0,0,  0,0,  0,0,
    1, -1,-1, 0,0,  0,0,  0,0,
    0,  0,0,  1,0,  0,0,  0,0,
    0,  0,0,  0,1,  0,0,  0,0,
    1,  0,0, -1,-1, 0,0,  0,0,
    0,  0,0,  0,0,  1,0,  0,0,
    0,  0,0,  0,0,  0,1,  0,0,
    1,  0,0,  0,0, -1,-1, 0,0,
    0,  0,0,  0,0,  0,0,  1,0,
    0,  0,0,  0,0,  0,0,  0,1,
    1,  0,0,  0,0,  0,0, -1,-1
  )

  val proj = {
    val ortho = rec.map(Cyclo(_)).t.orthogonalized.t
    val diag = (ortho.t * ortho).inverse
    ortho*diag*ortho.t
  }

  val av = aMat.value

}
