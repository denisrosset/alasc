package net.alasc.math

import scala.collection.immutable.BitSet
import spire.algebra._
import spire.syntax.groupAction._
import scala.collection.mutable.{Set => MutableSet, BitSet => MutableBitSet}

object OrbitInstances {
  implicit def setElementOrbit[P, G](implicit scalarAction: GroupAction[P, G]): GroupAction[Set[P], G] = new SetElementOrbit
  implicit def bitSetElementOrbit[G](implicit intAction: GroupAction[Int, G]): GroupAction[BitSet, G] = new BitSetElementOrbit
  implicit def setIterableOrbit[P, G](implicit scalarAction: GroupAction[P, G]): GroupAction[Set[P], Iterable[G]] = new SetIterableOrbit
  implicit def bitSetIterableOrbit[G](implicit intAction: GroupAction[Int, G]): GroupAction[BitSet, Iterable[G]] = new BitSetIterableOrbit
}

class SetElementOrbit[P, G](implicit scalarAction: GroupAction[P, G]) extends GroupAction[Set[P], G] {
  def actl(g: G, set: Set[P]) = {
    var res = set
    var toCheck = set
    while (!toCheck.isEmpty) {
      var newAdded = Set.empty[P]
      for (k <- toCheck) {
        val image = g |+|> k
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res
  }
  def actr(set: Set[P], g: G) = {
    var res = set
    var toCheck = set
    while (!toCheck.isEmpty) {
      var newAdded = Set.empty[P]
      for (k <- toCheck) {
        val image = k <|+| g
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res
  }
}

class BitSetElementOrbit[G](implicit intAction: GroupAction[Int, G]) extends GroupAction[BitSet, G] {
  def actl(g: G, set: BitSet) = {
    var res = MutableBitSet.empty ++ set
    var toCheck: scala.collection.BitSet = set
    while (!toCheck.isEmpty) {
      var newAdded = MutableBitSet.empty
      for (k <- toCheck) {
        val image = g |+|> k
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res.toImmutable
  }
  def actr(set: BitSet, g: G) = {
    var res = MutableBitSet.empty ++ set
    var toCheck: scala.collection.BitSet = set
    while (!toCheck.isEmpty) {
      var newAdded = MutableBitSet.empty
      for (k <- toCheck) {
        val image = k <|+| g
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res.toImmutable
  }
}

class SetIterableOrbit[P, G](implicit scalarAction: GroupAction[P, G]) extends GroupAction[Set[P], Iterable[G]] {
  def actl(gs: Iterable[G], set: Set[P]) = {
    var res = set
    var toCheck = set
    while (!toCheck.isEmpty) {
      var newAdded = Set.empty[P]
      for (k <- toCheck; g <- gs) {
        val image = g |+|> k
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res
  }
  def actr(set: Set[P], gs: Iterable[G]) = {
    var res = set
    var toCheck = set
    while (!toCheck.isEmpty) {
      var newAdded = Set.empty[P]
      for (k <- toCheck; g <- gs) {
        val image = k <|+| g
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res
  }
}

class BitSetIterableOrbit[P, G](implicit intAction: GroupAction[Int, G]) extends GroupAction[BitSet, Iterable[G]] {
  def actl(gs: Iterable[G], set: BitSet) = {
    var res = MutableBitSet.empty ++ set
    var toCheck: scala.collection.BitSet = set
    while (!toCheck.isEmpty) {
      var newAdded = MutableBitSet.empty
      for (k <- toCheck; g <- gs) {
        val image = g |+|> k
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res.toImmutable
  }
  def actr(set: BitSet, gs: Iterable[G]) = {
    var res = MutableBitSet.empty ++ set
    var toCheck: scala.collection.BitSet = set
    while (!toCheck.isEmpty) {
      var newAdded = MutableBitSet.empty
      for (k <- toCheck; g <- gs) {
        val image = k <|+| g
        if (!res.contains(image)) {
          res += image
          newAdded += image
        }
      }
      toCheck = newAdded
    }
    res.toImmutable
  }
}
