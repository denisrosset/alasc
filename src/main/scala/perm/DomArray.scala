package com.faacets
package perm

import scala.language.implicitConversions

class DomArray private[perm] (val array: Array[Int]) extends AnyVal {
  def apply(i0: Int): Dom = Dom._0(array(i0))
  def length = array.length
  def zeroBased = array
  def oneBased: Array[Int] = array.map(_+1)
}

object DomArray {
  def apply(images: Dom*) = new DomArray(Array(images.map(_._0):_*))
  def zeroBased(arr: Array[Int]) = new DomArray(arr.clone)
  def oneBased(arr: Array[Int]) = new DomArray(arr.map(_-1))
}
