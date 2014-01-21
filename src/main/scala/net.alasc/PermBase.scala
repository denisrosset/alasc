package net.alasc

import scala.util.Random

implicit class RichPerm(val perm: Perm) {

}

trait PermElementLike extends FiniteElementLike {
  def cyclesToTextUsingSymbols(symbols: Seq[String]) = cycles.filter(_.length > 1).map(_.map( d => symbols(d._0) ).mkString("(",",",")")).mkString("")
  def cyclesToText = cycles.filter(_.length > 1).map(_.map(_._1).mkString("(",",",")")).mkString("")
  private[this] def orderRec(mul: PermElementLike, acc: Int): Int = if(mul.isIdentity) acc else orderRec(this.permMul(mul), acc + 1)
  def permMul(that: PermElementLike): PermElementLike
  def order = orderRec(this, 1)
  def size: Int
  def image(k: Dom): Dom
  def images: DomArray
  def invImage(k: Dom): Dom
  def toExplicit: Perm
  def support = domain.filter( k => k != image(k) )
  def domain: Iterable[Dom] = (0 until size).toIterable.map(Dom._0(_))
  def cycle[P](start: Dom): List[Dom] = {
    def walk(el: Dom): List[Dom] =
      if (el == start)
        Nil
      else
        el :: walk(image(el))
    start :: walk(image(start))
  }

  def cycles: List[List[Dom]] = {
    var checked = scala.collection.mutable.BitSet(size)
    var i = Dom(size)
    var cycleList = List.empty[List[Dom]]
    while (i >= Dom(1)) {
      if(!checked(i._0)) {
        def visit(e: Dom, stopWhen: Dom): List[Dom] = {
          checked(e._0) = true
          if (image(e) == stopWhen)
            e :: Nil
          else
            e :: visit(image(e), stopWhen)
        }
        val cycle = visit(i, i)
        val mn = cycle.min
        val (bef, aft) = cycle.span(_ != mn)
        cycleList = (aft ++ bef) :: cycleList
      }
      i = Dom._0(i._0 - 1)
    }
    cycleList.sortBy(_.head)
  }
  def isDefinedAt(k: Dom) = (k._0 >= 0 && k._0 < size)
  def intCompare(that: PermElementLike) = {
    val firstNotEqual = domain.find(k => image(k) != that.image(k))
    firstNotEqual match {
      case None => 0
      case Some(k) if image(k) <= that.image(k) => -1
      case _ => 1
    }
  }
}
trait PermElement[E <: PermElement[E]] extends PermElementLike with FiniteElement[E] {
  self: E =>
  def permMul(that: PermElementLike) = this*(that.asInstanceOf[E])
  def powers: List[E] = {
    def powerRec(listOfPowers: List[E]): List[E] = listOfPowers match {
      case hd :: tl if hd.isIdentity => listOfPowers
      case hd :: tl => powerRec(self * hd :: listOfPowers)
      case Nil => sys.error("Should not happen")
    }
    powerRec(self :: Nil)
  }
}

trait PermGroup[E <: PermElement[E]] extends FiniteGroup[E] {
/*
Degree of the permutation group, i.e. size of the domain.
*/
  def degree: Int
/*
Domain of the permutation group.
*/
  def domain = (0 until degree).toIterator.map(Dom._0(_))
/*
Instantiates a group element from an explicit permutation `p`. Returns
`Some(e)` if `p` can be represented by `e` or `None`.
*/
  def fromExplicit(p: Perm): Option[E]

  def toGroup: PGroup[E] = PGroup.fromRandomElementsAndOrder(identity, randomElement, order)
}
