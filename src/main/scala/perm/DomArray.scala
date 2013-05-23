package com.faacets
package perm

import scala.language.implicitConversions

class DomArray private[perm] (val array: Array[Dom]) extends AnyVal {
  def apply(d: Dom): Dom = array(d._0)
  def length = array.length
  def zeroBased = array.asInstanceOf[Array[Int]]
  def oneBased = Array.tabulate[Int](length)(array(_)._1)
}

object DomArray {
  def apply(images: Dom*) = new DomArray(Array(images:_*))
  def fromArray(arr: Array[Dom]) = new DomArray(arr.clone)
  def zeroBased(arr: Array[Int]) = new DomArray(arr.clone.asInstanceOf[Array[Dom]])
}
