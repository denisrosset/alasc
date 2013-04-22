package com.faacets.perm

/** Symmetric group defined on the domain 0 .. degree-1. */
final case class SymmetricGroup(val degree: Int) extends PermutationGroup with PermutationHasSubgroup {
  import scala.collection.immutable.SortedSet
  import scala.util.Random
  import scala.math.Ordering.Implicits._

  type Group = SymmetricGroup
  type Subgroup = SymmetricSubgroup
  type Element = SymmetricGroupElement

  trait DomainSetOrdering extends Ordering[SortedSet[Domain]] {
    def compare(s1: SortedSet[Domain], s2: SortedSet[Domain]) = {
      val firstNotEqual = (s1 zip s2).find(x => x._1 != x._2)
      firstNotEqual match {
        case None => 0
        case Some((i1, i2)) => Ordering.Int.compare(i1, i2)
      }
    }
  }

  implicit object DomainSet extends DomainSetOrdering

  def assertValid = degree > 0
  def identity = SymmetricGroupElement(Permutation.identity(degree))
  /* The generators are shifts, n = degree - 1 in total. */
  def generatorsIterator = {
    val idPerm = Permutation.identity(degree)
      (0 to degree - 2).map(i => SymmetricGroupElement(idPerm.withSwap(i, i + 1))).iterator
  }
  def order = (1 to degree).foldLeft(BigInt(1))(_*_)
  /* The symmetric group contains all permutation of domainSize == degree. */
  def contains(e: Element): Boolean = true
  def elementsIterator = Permutation.all(degree).map(SymmetricGroupElement(_))

  def randomElement()(implicit gen: Random = Random) =
    SymmetricGroupElement(Permutation.random(degree))

  def setStabilizer(s: SortedSet[Domain]): Subgroup = {
    val all = SortedSet.empty[Domain] ++ domain
    val snot = all.diff(s)
    SetwiseStabilizerSubgroup(SortedSet(s, snot))
  }

  type Cell = SortedSet[Domain]

  def subgroupFixing[D](els: Seq[D]) = {
    val cells = SortedSet.empty[SortedSet[Domain]] ++ els.zipWithIndex.groupBy( (_:(D,Int))._1 ).values.map(_.map(_._2)).map( SortedSet.empty[Domain] ++ _ )
    SetwiseStabilizerSubgroup(cells)
  }

  abstract class SymmetricSubgroup extends PermutationSubgroup {
    symmetricSubgroup: Subgroup =>
    type Group <: SymmetricSubgroup
    type Element <: SymmetricSubgroupElement
    def setStabilizer(s: Cell): Subgroup = ???
    def intersect(subgroup: Subgroup): Subgroup = ???
    abstract class SymmetricSubgroupElement extends PermutationSubgroupElement {
      self: symmetricSubgroup.Element =>
    }
  }

  final case class SetwiseStabilizerSubgroup(val cells: SortedSet[Cell]) extends SymmetricSubgroup {
    type Group = SetwiseStabilizerSubgroup
    type Element = SetwiseStabilizerElement

    val mySuperGroup = SymmetricGroup.this
    val syms = cells.toList.map(s => SymmetricGroup(s.size))
    def assertValid = {
      // verifies that all sets are disjoint
      for (s1 <- cells; s2 <- cells if s1 != s2)
        assert(s1.forall(!s2(_)))
      // verifies that the union of all cells is the
      val allCells = cells.foldLeft(SortedSet.empty[Domain])(_ union _)
      assert(allCells.sameElements(mySuperGroup.domain))
    }
    def generatorsIterator = for {
      pid <- List(identity.seList).iterator
      i <- syms.indices
      g <- syms(i).generators
    } yield SetwiseStabilizerElement(pid.updated(i, g))

    override def intersect(sub: Subgroup): Subgroup = sub match {
      case ssg: SetwiseStabilizerSubgroup => {
        val newCells = SortedSet.empty[Cell] ++ (cells, ssg.cells).productIterator.map {
          case (x:Cell,y:Cell) => x.intersect(y)
        }.filter(_.size > 0)
        SetwiseStabilizerSubgroup(newCells)
      }
      case _ => ???
    }
    override def setStabilizer(s: Cell): Subgroup = {
      val s1 = SymmetricGroup.this.setStabilizer(s)
      intersect(s1)
    }
    def order = syms.foldLeft(BigInt(1))(_*_.order)
    def identity = SetwiseStabilizerElement(syms.map(_.identity))
    def randomElement()(implicit gen: Random = Random): Element =
      SetwiseStabilizerElement(syms.map(_.randomElement))
    def elementsIterator: Iterator[Element] = (for {
      p:List[SymmetricGroup#Element] <- combineList(syms.map(_.elements))
    } yield SetwiseStabilizerElement(p)).iterator
    def contains(e: Element): Boolean = true
    case class SetwiseStabilizerElement(val seList: List[SymmetricGroup#Element]) extends SymmetricSubgroupElement {
      val group = SetwiseStabilizerSubgroup.this
      def isIdentity = seList.forall(_.isIdentity)
      def assertValid = seList.map(_.assertValid)
      def equal(that: Element) = (seList zip that.seList).forall {
        case (x:SymmetricGroup#Element, y:SymmetricGroup#Element) =>
          x.p == y.p
      }
      def image(k: Domain): Domain = {
        for ((cell, se) <- (cells zip seList) if cell.contains(k)) {
          val seq = cell.toSeq
          return seq(se.image(seq.indexOf(k)))
        }
        throw new IllegalArgumentException("Cannot find domain in permutation.")
      }
      def images: Array[Domain] = {
        val a = new Array[Domain](degree)
        for ((cell, se) <- (cells zip seList)) {
          val seq = cell.toSeq
          for (i <- seq.indices)
            a(seq(i)) = seq(se.image(i))
        }
        a
      }
      def superElement = SymmetricGroupElement(explicit)
      def inverse = SetwiseStabilizerElement(seList.map(_.inverse))
      def *(that: Element): Element = SetwiseStabilizerElement(
          (seList zip that.seList).map { 
            case (x:SymmetricGroup#Element, y:SymmetricGroup#Element) => {
              val x1 = x.asInstanceOf[x.group.Element]
              val y1 = y.asInstanceOf[x.group.Element]
              (x1*y1).asInstanceOf[SymmetricGroup#Element]
            }
          } )
    }
  }
  final case class SymmetricGroupElement(val p: Permutation) extends PermutationElement {
    self: Element =>
    val group = SymmetricGroup.this
    require(p.domainSize == group.degree)
    def equal(that: SymmetricGroupElement) = p == that.p
    def assertValid = p.assertValid
    def isIdentity = p.isIdentity
    def image(k: Domain) = p.image(k)
    def images = p.images
    def inverse = SymmetricGroupElement(p.inverse)
    def *(that: SymmetricGroupElement) = SymmetricGroupElement(p*that.p)
  }
}
