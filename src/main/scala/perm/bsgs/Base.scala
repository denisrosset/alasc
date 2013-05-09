package com.faacets
package perm
package bsgs

trait BaseStrategy {
  def get(generators: List[PermElementLike]): List[Dom]
}

case class PrescribedBase(base: List[Dom]) extends BaseStrategy {
  def get(generators: List[PermElementLike]) = base
}

object EmptyBase extends BaseStrategy {
  def get(elements: List[PermElementLike]): List[Dom] = {
    elements match {
      case Nil => List(Dom._0(0))
      case g :: tail => {
        for (i <- 0 until g.size)
          if (g.image(Dom._0(i)) != Dom._0(i))
            return List(Dom._0(i))
        List(Dom._0(0))
      }
    }
  }
}

object FullBase extends BaseStrategy {
  def get(elements: List[PermElementLike]) = {
    val n = elements.head.size
      (0 until n).toList.map(Dom._0(_))
  }
}

