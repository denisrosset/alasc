package net.alasc

package object coll {
  type HasValueByKey[-K, +V] = Function1[K, V]
}
