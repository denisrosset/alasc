package net.alasc

/** Minimal trait for a Map that cannot be updated. */
trait ReadOnlyMap[A, +B] extends PartialFunction[A, B] with Iterable[(A, B)] {
  def size: Int
  def isDefinedAt(key: A): Boolean
  def apply(key: A): B
  def get(key: A): Option[B]
  def iterator: Iterator[(A, B)]
  def keysIterator: Iterator[A]
  def valuesIterator: Iterator[B]
}

trait ReadOnlyMapLike[A, +B] extends ReadOnlyMap[A, B] {
  def get(key: A) = isDefinedAt(key) match {
    case true => Some(apply(key))
    case false => None
  }
  def keysIterator: Iterator[A] = iterator.map(_._1)
  def valuesIterator: Iterator[B] = iterator.map(_._2)
}
