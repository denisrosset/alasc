package net.alasc
package bsgs

case class Base(list: List[Dom]) {
  def conjugatedBy(e: PermElementLike) = {
    Base(list.map( b => e.image(b) ))
  }
}

trait BaseStrategy {
  def get(generators: List[PermElementLike], identity: PermElementLike): Base
}

case class PrescribedBase(base: Base) extends BaseStrategy {
  def get(generators: List[PermElementLike], identity: PermElementLike) = base
}

object EmptyBase extends BaseStrategy {
  def get(elements: List[PermElementLike], identity: PermElementLike): Base = elements match {
    case Nil => Base(List(Dom._0(0)))
    case g :: tail => {
      for (i <- 0 until g.size)
        if (g.image(Dom._0(i)) != Dom._0(i))
          return Base(List(Dom._0(i)))
      Base(List(Dom._0(0)))
    }
  }
}

object FullBase extends BaseStrategy {
  def get(elements: List[PermElementLike], identity: PermElementLike) = Base({
    val n = identity.size
    (0 until n).toList.map(Dom._0(_))
  })
}
