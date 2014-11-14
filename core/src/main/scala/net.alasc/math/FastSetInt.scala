package net.alasc.math

import scala.annotation.tailrec

abstract class FastSetInt extends Set[Int] { self =>
  def start: Int
  def hasNext(i: Int): Boolean
  def next(i: Int): Int
  def iterator = if (isEmpty) Iterator.empty else new Iterator[Int] {
    private[this] var current = -1
    def hasNext = current == -1 || self.hasNext(current)
    def next: Int = {
      if (current == -1)
        current = start
      else
        current = self.next(current)
      current
    }
  }
  override def size: Int = {
    @tailrec def rec(current: Int, acc: Int): Int =
      if (hasNext(current)) rec(next(current), acc + 1) else acc

    if (isEmpty) 0 else rec(start, 1)
  }
  override def foreach[U](f: Int => U): Unit = {
    @tailrec def rec(current: Int): Unit = {
      f(current)
      if (hasNext(current)) rec(next(current))
    }
    if (nonEmpty) rec(start)
  }
  override def forall(p: Int => Boolean): Boolean = {
    @tailrec def rec(current: Int): Boolean =
      if (!p(current)) false else {
        if (hasNext(current)) rec(next(current)) else true
      }

    if (isEmpty) true else rec(start)
  }
  override def exists(p: Int => Boolean): Boolean = {
    @tailrec def rec(current: Int): Boolean =
      if (p(current)) true else {
        if (hasNext(current)) rec(next(current)) else false
      }

    if (isEmpty) false else rec(start)
  }
  def +(i: Int) = iterator.toSet + i
  def -(i: Int) = iterator.toSet - i
}
