package net.alasc
package util

/** Cache for unique objects `UniquenessCache`
  * 
  * This is a base class for companion objects who keep track of copies
  * of objects they construct, as to not construct a new copy of an object
  * if it already exists in the memory. The implementation here was taken from the
  * Scala standard library.
  */
abstract class UniquenessCache[K, V >: Null]
{
  import java.lang.ref.WeakReference
  import java.util.WeakHashMap
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private val rwl = new ReentrantReadWriteLock()
  private val rlock = rwl.readLock
  private val wlock = rwl.writeLock
  private val map = new WeakHashMap[K, WeakReference[V]]

  protected def valueFromKey(k: K): V
  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V = {
      rlock.lock
      try {
        val reference = map get name
        if (reference == null) null
        else reference.get  // will be null if we were gc-ed
      }
      finally rlock.unlock
    }
    def updateCache(): V = {
      wlock.lock
      try {
        val res = cached()
        if (res != null) res
        else {
          // If we don't remove the old String key from the map, we can
          // wind up with one String as the key and a different String as
          // as the name field in the Symbol, which can lead to surprising
          // GC behavior and duplicate Symbols. See SI-6706.
          map remove name
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      }
      finally wlock.unlock
    }

    val res = cached()
    if (res == null) updateCache()
    else res
  }
  def unapply(other: V): Option[K] = keyFromValue(other)
}
