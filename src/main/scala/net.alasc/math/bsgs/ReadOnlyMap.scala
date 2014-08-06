package net.alasc.math
package bsgs

import scala.{ specialized => spec }

trait ReadOnlyMap[@spec(Int) K, +V] extends PartialFunction[K, V] with Iterable[(K, V)] {
  def size: Int
  def isDefinedAt(key: K): Boolean
  def apply(key: K): V
  def get(key: K): Option[V] = if (isDefinedAt(key)) Some(apply(key)) else None
  def iterator: Iterator[(K, V)]
  def keysIterator: Iterator[K] = iterator.map(_._1)
  def valuesIterator: Iterator[V] = iterator.map(_._2)
  // TODO def foreach
}
