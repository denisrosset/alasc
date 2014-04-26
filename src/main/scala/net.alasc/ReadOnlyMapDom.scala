package net.alasc

/** Minimal trait for a Map that cannot be updated. */
trait ReadOnlyMapDom[+B] extends PartialFunction[Dom, B] with Iterable[(Dom, B)] {
  def size: Int
  def isDefinedAt(key: Dom): Boolean
  def apply(key: Dom): B
  def get(key: Dom): Option[B]
  def iterator: Iterator[(Dom, B)]
  def keysIterator: Iterator[Dom]
  def valuesIterator: Iterator[B]
}

trait ReadOnlyMapDomImpl[+B] extends ReadOnlyMapDom[B] {
  def get(key: Dom) = isDefinedAt(key) match {
    case true => Some(apply(key))
    case false => None
  }
  def keysIterator: Iterator[Dom] = iterator.map(_._1)
  def valuesIterator: Iterator[B] = iterator.map(_._2)
}
