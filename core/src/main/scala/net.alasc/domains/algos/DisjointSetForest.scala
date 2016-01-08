package net.alasc.domains
package algos

import scala.annotation.tailrec

import scala.collection.BitSet
import scala.collection.mutable
import scala.collection.immutable

import spire.algebra.PartialOrder

import net.alasc.util._

/** Implementation of a disjoint-set data structure, using forests.
  * 
  * See http://en.wikipedia.org/wiki/Disjoint-set_data_structure
  * 
  * We implement path compression, but not union by rank.
  */
class DisjointSetForest(val parent: Array[Int]) {

  def size = parent.length

  /** Finds the representative of the set in which `x` is contained. */
  def find(x: Int): Int =
    if (parent(x) != x) {
      val res = find(parent(x))
      parent(x) = res
      res
    } else x

  /** Combines the trees containing `x` and `y`. The representative of the union is chosen as the 
    * minimal representative of `x` and `y`.
    */
  def union(x: Int, y: Int): Unit = {
    val xRoot = find(x)
    val yRoot = find(y)
    if (xRoot < yRoot)
      parent(yRoot) = xRoot
    else if (xRoot > yRoot)
      parent(xRoot) = yRoot
  }

  def partition(domain: Domain): Partition.In[domain.type] = {
    (0 until size).foreach(find(_))
    Partition.fromSeq(domain)(parent)
  }

}

object DisjointSetForest {

  // MakeSet from http://en.wikipedia.org/wiki/Disjoint-set_data_structure
  def apply(size: Int) = new DisjointSetForest(Array.tabulate(size)(identity))

  def apply(partition: Partition) = new DisjointSetForest(Array.tabulate(partition.size)(partition.representative(_)))
  
}
