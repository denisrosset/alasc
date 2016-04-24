package net.alasc.finite

import scala.reflect.ClassTag

import spire.NoImplicit
import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.eq._
import spire.syntax.group._
import spire.util.Opt

import shapeless.Witness

import net.alasc.algebra.{Permutation, PermutationAction, PermutationBuilder}
import net.alasc.bsgs.{BaseChange, BuildChain, SchreierSims}
import net.alasc.perms.{FaithfulPermRep, FaithfulPermRepBuilder, PermRep}
import net.alasc.util._

/** Representation of group elements of type `G` on a vector of `dimension` over the field `K`. */
trait Rep[G, K] {
  self =>

  def apply(g: G): scalin.immutable.Mat[K]

  /** Dimension/degree of the representation. */
  def dimension: Int

  /** Tests whether this representation can represent the element `g`. */
  def represents(g: G): Boolean

  type Wrap = Rep.Wrap[G, self.type]

  /** Wraps the given group element, to be represented by this representation. */
  def Wrap(g: G): Wrap =
    new Rep.Wrap[G, self.type](g)

  def wrap(grp: Grp[G])(implicit builder: GrpBuilder[Wrap]): Grp[Wrap] =
    builder.fromGeneratorsAndOrder(grp.generators.map(Wrap(_)), grp.order)

  trait Tag

}

object Rep {

  type Of[G, R <: Rep[G, _] with Singleton] = { type In = R; type Self = G }

  def Of[G](g: G, rep: Rep[G, _]): Of[G, rep.type] = g.asInstanceOf[Of[G, rep.type]]

  abstract class syntax0 {

    implicit def equ[G:Eq, R <: Rep[G, _] with Singleton](implicit ev: NoImplicit[Permutation[Of[G, R]]]): Eq[Of[G, R]] =
      Eq[G].asInstanceOf[Eq[Of[G, R]]]

    implicit def group[G:Group, R <: Rep[G, _] with Singleton](implicit ev: NoImplicit[Permutation[Of[G, R]]]): Group[Of[G, R]] =
      Group[G].asInstanceOf[Group[Of[G, R]]]

    implicit def permutationAction[G, R <: PermRep[G] with Singleton]
    (implicit ev: NoImplicit[Permutation[Of[G, R]]], witness: shapeless.Witness.Aux[R]): PermutationAction[Of[G, R]] =
      witness.value.permutationAction.asInstanceOf[PermutationAction[Of[G, R]]]

  }

  object syntax extends syntax0 {

    implicit def permutation[G:Eq:Group, R <: FaithfulPermRep[G] with Singleton]
    (implicit witness: shapeless.Witness.Aux[R]): Permutation[Of[G, R]] = {
      val permutationG = new Permutation[G] {
        private[this] val action = witness.value.permutationAction
        private[this] val equ = Eq[G]
        private[this] val group = Group[G]

        // Eq
        def eqv(x: G, y: G): Boolean = equ.eqv(x, y)

        // Group
        override def combine(as: TraversableOnce[G]) = group.combine(as)
        override def combineOption(as: TraversableOnce[G]) = group.combineOption(as)
        override def combinen(a: G, n: Int) = group.combinen(a, n)
        override def inverse(a: G) = group.inverse(a)
        override def isId(a: G)(implicit ev: Eq[G]) = group.isId(a)(ev)
        def id = group.id
        def op(x: G, y: G) = group.op(x, y)
        override def opInverse(a: G, b: G) = group.opInverse(a, b)

        // PermutationAction

        def actr(p: Int, g: G) = action.actr(p, g)
        def actl(g: G, p: Int): Int = action.actl(g, p)

        def movedPointsUpperBound(g: G): NNOption = action.movedPointsUpperBound(g)
        override def smallestMovedPoint(g: G) = action.smallestMovedPoint(g)
        override def largestMovedPoint(g: G) = action.largestMovedPoint(g)
        override def movedPoints(g: G): Set[Int] = action.movedPoints(g)
        override def nMovedPoints(g: G): Int = action.nMovedPoints(g)
        override def movesPoint(g: G, i: Int) = action.movesPoint(g, i)
        override def findMovedPoint(g: G): NNOption = action.findMovedPoint(g)

        override def signPerm(g: G): Int = action.signPerm(g)
        override def cycleStructure(g: G): Map[Int, Int] = action.cycleStructure(g)
        override def permutationOrder(g: G): SafeLong = action.permutationOrder(g)

        override def orbit(g: G, i: Int): Set[Int] = action.orbit(g, i)
        override def images(g: G, n: Int): IndexedSeq[Int] = action.images(g, n)
        override def toPermutation[P](g: G)(implicit evP: PermutationBuilder[P]): P = action.toPermutation(g)

      }
      permutationG.asInstanceOf[Permutation[Of[G, R]]]
    }

  }

  import scalin.immutable.Mat

  def apply[G: ClassTag : Eq : Group : FaithfulPermRepBuilder, K, MK <: Mat[K]]
  (generators: (G, Mat[K])*)(implicit K: scalin.algebra.MatField[K, MK],
                             baseChange: BaseChange, schreierSims: SchreierSims): Rep[G, K] = {
    import scalin.syntax.all._
    require(generators.nonEmpty)
    val d = generators.head._2.nRows
    val builder = implicitly[FaithfulPermRepBuilder[G]]
    val rep = builder.build(generators.map(_._1))
    import rep.permutationAction

    implicit object algebra extends Group[(G, Mat[K])] with Eq[(G, Mat[K])] with PermutationAction[(G, Mat[K])] {
      def eqv(lhs: (G, Mat[K]), rhs: (G, Mat[K])) = lhs._1 === rhs._1

      def op(lhs: (G, Mat[K]), rhs: (G, Mat[K])) = (lhs._1 |+| rhs._1, lhs._2 * rhs._2)

      def inverse(lhs: (G, Mat[K])) = (lhs._1.inverse, lhs._2.inverse)

      def id = (Group[G].id, eye[K](d))

      override def nMovedPoints(g: (G, Mat[K])): Int = permutationAction.nMovedPoints(g._1)

      override def smallestMovedPoint(g: (G, Mat[K])): NNOption = permutationAction.smallestMovedPoint(g._1)

      override def movedPointsUpperBound(g: (G, Mat[K])): NNOption = permutationAction.movedPointsUpperBound(g._1)

      override def largestMovedPoint(g: (G, Mat[K])): NNOption = permutationAction.largestMovedPoint(g._1)

      override def movedPoints(g: (G, Mat[K])): Set[Int] = permutationAction.movedPoints(g._1)

      override def actr(p: Int, g: (G, Mat[K])): Int = permutationAction.actr(p, g._1)

      override def actl(g: (G, Mat[K]), p: Int): Int = permutationAction.actl(g._1, p)
    }

    new Rep[G, K] {

      val dimension = d
      val chain = BuildChain.fromGenerators(generators)

      def apply(g: G): Mat[K] = chain.siftOther(g) match {
        case Opt((gid, mat)) => mat
        case _ => sys.error(s"$g cannot be represented")
      }

      def represents(g: G) = chain.siftOther(g).nonEmpty

    }
  }

  /** Wraps a group element of type G such that it is represented by representation `R`. */
  final class Wrap[G, R <: Rep[G, _] with Singleton](val underlying: G) extends AnyVal {

    override def toString = s"Wrap($underlying)"

  }

  abstract class Wrap0 {

    implicit def wrapEq[G, R <: Rep[G, _] with Singleton](implicit G: Eq[G]): Eq[Wrap[G, R]] =
      new Eq[Wrap[G, R]] {

        override def eqv(x: Wrap[G, R], y: Wrap[G, R]): Boolean = G.eqv(x.underlying, y.underlying)

      }

    implicit def wrapGroup[G, R <: Rep[G, _] with Singleton](implicit G: Group[G]): Group[Wrap[G, R]] =
      new Group[Wrap[G, R]] {

        override def inverse(lhs: Wrap[G, R]): Wrap[G, R] = new Wrap[G, R](G.inverse(lhs.underlying))

        override def id: Wrap[G, R] = new Wrap[G, R](G.id)

        override def op(x: Wrap[G, R], y: Wrap[G, R]): Wrap[G, R] =
          new Wrap[G, R](G.op(x.underlying, y.underlying))

      }

  }

  object Wrap extends Wrap0 {

    case class Predicate[G, R <: FaithfulPermRep[G] with Singleton](p: G => Boolean)
      extends Function1[Wrap[G, R], Boolean] {

      def apply(r: Wrap[G, R]): Boolean = p(r.underlying)

    }

    implicit def repedPermutation[G, R <: FaithfulPermRep[G] with Singleton]
    (implicit equ: Eq[G], group: Group[G], w: Witness.Aux[R]): Permutation[Wrap[G, R]] =
      new Permutation[Wrap[G, R]] {

        val rep: R = w.value

        override def eqv(x: Wrap[G, R], y: Wrap[G, R]): Boolean = equ.eqv(x.underlying, y.underlying)

        override def actl(g: Wrap[G, R], p: Int): Int = rep.permutationAction.actl(g.underlying, p)

        override def actr(p: Int, g: Wrap[G, R]): Int = rep.permutationAction.actr(p, g.underlying)

        override def inverse(a: Wrap[G, R]): Wrap[G, R] =
          new Wrap[G, R](group.inverse(a.underlying))

        override def id: Wrap[G, R] = new Wrap[G, R](group.id)

        override def op(x: Wrap[G, R], y: Wrap[G, R]): Wrap[G, R] =
          new Wrap[G, R](group.op(x.underlying, y.underlying))

        override def smallestMovedPoint(g: Wrap[G, R]): NNOption = rep.permutationAction.smallestMovedPoint(g.underlying)

        override def movedPoints(g: Wrap[G, R]): Set[Int] = rep.permutationAction.movedPoints(g.underlying)

        override def nMovedPoints(g: Wrap[G, R]): Int = rep.permutationAction.nMovedPoints(g.underlying)

        override def largestMovedPoint(g: Wrap[G, R]): NNOption = rep.permutationAction.largestMovedPoint(g.underlying)

        override def movedPointsUpperBound(g: Wrap[G, R]): NNOption = NNSome(rep.dimension - 1)

      }

  }

}
