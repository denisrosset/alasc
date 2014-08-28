package net.alasc.coll
package big

import scala.{ specialized => spec }

trait IndexedSeq[A] extends Any with Iterable[A] with HasIndexing[BigInt, A]

trait IndexedSet[A] extends Any with IndexedSeq[A] with Set[A]
