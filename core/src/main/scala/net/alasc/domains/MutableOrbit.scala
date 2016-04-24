package net.alasc.domains

import spire.algebra.Order
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra.PermutationAction

/** Mutable BitSet-like structure to explore orbits of permutations. Every element of the domain
  * is stored with two bits:
  *
  * - `00` the element is not part of the orbit,
  * - `01` the element is part of the orbit, and has already been checked,
  * - `11` the element is part of the orbit, and is part of the current check.
  * - `10` the element is part of the orbit, and is part of the next check.
  *
  * `k` being checked means that all the elements `k <|+| g` have been added to the orbit.
  */
class MutableOrbit(val words: Array[Long]) extends AnyVal {

  import MutableOrbit.{logWL, wordLength}

  def status(k: Int): Int = {
    val w = k >>> logWL
    ((words(w) >>> (k * 2)) & 3L).toInt
  }

  @inline def notInOrbit = 0L
  @inline def alreadyChecked = 1L
  @inline def inCurrentCheck = 3L
  @inline def inNextCheck = 2L

  override def toString = {
    def withStatus(s: Long): String = (0 until words.length * wordLength).filter(status(_) == s).mkString(",")
    "Checked: " + withStatus(alreadyChecked) + " Current: " + withStatus(inCurrentCheck) + " Next: " + withStatus(inNextCheck)
  }

  /** Steps into the next check.
    *
    * Sends 00-> 00, 01 -> 01, 11 -> 01 and 10 -> 11.
    *
    */
  def step(): Unit = {
    /** Sends 00 -> 00, 01,10,11 -> 01. */
    @inline def mask(wrd: Long): Long = (wrd | (wrd >>> 1)) & 0x5555555555555555L // 0101 ... 0101
    cforRange(0 until words.length) { w =>
      val word = words(w)
      val lowBits = mask(word)
      val highBits = (word & ~(word << 1)) & 0xAAAAAAAAAAAAAAAAL // 1010 ... 1010
      words(w) = lowBits | highBits
    }
  }

  def clear(): Unit = {
    cforRange(0 until words.length) { w => words(w) = 0L }
  }


  /** Checks whether `k` is part of the orbit. */
  def inOrbit(k: Int): Boolean = {
    val w = k >>> logWL
    val word = words(w)
    ((word | (word >>> 1)) & (1L << (k * 2))) != 0L
  }

  /** Adds the new element `k` to the orbit. Requires (but does not check) that `k` is not part of the orbit. */
  def addNew(k: Int): Unit = {
    val w = k >>> logWL
    words(w) = words(w) | (inNextCheck << (k * 2))
  }

  /** Adds the set to the orbit. Requires (but does not check) that the added elements are not part of the orbit. */
  def addNew(set: Set[Int]): Unit = {
    // see https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
    // "Inserts" a 0 bit after each of the 32 low bits of x
    @inline def expand(i: Int): Long = {
      var x =    i.toLong & 0x00000000FFFFFFFFL
      x = (x ^ (x << 16)) & 0x0000FFFF0000FFFFL
      x = (x ^ (x <<  8)) & 0x00FF00FF00FF00FFL
      x = (x ^ (x <<  4)) & 0x0F0F0F0F0F0F0F0FL
      x = (x ^ (x <<  2)) & 0x3333333333333333L
          (x ^ (x <<  1)) & 0x5555555555555555L
    }
    set match {
      case bs1: scala.collection.immutable.BitSet.BitSet1 =>
        if (words.length > 0) {
          val w = expand(bs1.elems.toInt) << 1 // we want to set the odd bits
          words(0) = words(0) | w
        }
        if (words.length > 1) {
          val w = expand((bs1.elems >>> 32).toInt) << 1
          words(1) = words(1) | w
        }
      case bs: scala.collection.BitSet =>
        val array = bs match {
          case bsn: scala.collection.immutable.BitSet.BitSetN => bsn.elems
          case _ => bs.toBitMask
        }
        cforRange(0 until words.length) { w =>
          val wrd = array(w/2)
          val asint = if (w % 2 == 0) wrd.toInt else (wrd >>> 32).toInt
          words(w) = words(w) | (expand(asint) << 1)
        }
      case _ =>
        set.foreach(addNew(_))
    }
  }

  /** Returns the next element to check on or after `k` if it exists, or `-1` otherwise. */
  def nextInCurrentCheck(k: Int): Int = {
    /** Sends 00,01,10 -> 00, 11 -> 01. */
    @inline def mask(wrd: Long): Long = (wrd & (wrd >>> 1)) & 0x5555555555555555L // 0101 ... 0101
    var w = k >>> logWL
    if (w >= words.length) return -1
    var word = mask(words(w) & ((-1L) << (k * 2)))
    while (true) {
      if (word != 0)
        return (w * wordLength) + java.lang.Long.numberOfTrailingZeros(word)/2
      w += 1
      if (w == words.length) return -1
      word = mask(words(w))
    }
    -1
  }

  /** Returns a bitmask appropriate for a bitset. */
  def toBitMask: Array[Long] = {
    // see https://fgiesen.wordpress.com/2009/12/13/decoding-morton-codes/
    @inline def compact(wrd: Long): Int = {
      var w: Long = wrd | (wrd >>> 1) // either bit can be set for a point in the orbit
                                                // Below: we show only the 32 lower-order bits
      w &= 0x5555555555555555L                  // x = -f-e -d-c -b-a -9-8 -7-6 -5-4 -3-2 -1-0
      w = (w ^ (w >>> 1)) & 0x3333333333333333L // x = --fe --dc --ba --98 --76 --54 --32 --10
      w = (w ^ (w >>> 2)) & 0x0f0f0f0f0f0f0f0fL // x = ---- fedc ---- ba98 ---- 7654 ---- 3210
      w = (w ^ (w >>> 4)) & 0x00ff00ff00ff00ffL // x = ---- ---- fedc ba98 ---- ---- 7654 3210
      w = (w ^ (w >>> 8)) & 0x0000ffff0000ffffL // x = ---- ---- ---- ---- fedc ba98 7654 3210
      (w ^ (w >>> 16)).toInt
    }
    @inline def toLongUnsigned(i: Int): Long = i.toLong & 0xFFFFFFFFL
    if (words.length == 0) return new Array[Long](0)
    val array = new Array[Long]((words.length - 1) / 2 + 1)
    cforRange(0 until array.length - 1) { i =>
      val w1 = words(i * 2)
      val w2 = words(i * 2 + 1)
      array(i) = toLongUnsigned(compact(w1)) + (toLongUnsigned(compact(w2)) << 32)
    }
    val w = (array.length - 1) * 2
    val w1 = words(w)
    val w2 = if (w + 1 < words.length) words(w + 1) else 0L
    array(array.length - 1) = toLongUnsigned(compact(w1)) + (toLongUnsigned(compact(w2)) << 32)
    array
  }

}

object MutableOrbit {

  final val logWL = 5
  final val wordLength = 32

  def nWordsForSize(n: Int): Int = if (n == 0) 0 else (n - 1) / wordLength + 1

  def forSize(n: Int): MutableOrbit = new MutableOrbit(new Array[Long](nWordsForSize(n)))

  /** Tests whether `point` is the smallest point in its orbit under `generators`. An optional
    * empty [[MutableOrbit]] can be provided; it will be cleared when the function returns.
    *
    * @param domainSize        Domain size used to allocate a [[MutableOrbit]] if none provided
    * @param emptyMutableOrbit (Optional) Provided empty mutable orbit to avoid allocations
    */
  @inline def isSmallestPointInOrbit[G](domainSize: Int, point: Int, generators: Iterable[G],
                                        emptyMutableOrbit: MutableOrbit = new MutableOrbit(null))
                                       (implicit G: PermutationAction[G], order: Order[Int]): Boolean = {
    val orbit = if (emptyMutableOrbit.words eq null) forSize(domainSize) else emptyMutableOrbit
    orbit.addNew(point)
    var start = point
    while (start != -1) {
      val smallerImage = generators.exists { g => // we want to find an image < point
        var el = start
        var existsSmaller = false
        while (el != -1 && !existsSmaller) {
          val image = el <|+| g
          if (!orbit.inOrbit(image)) {
            orbit.addNew(image)
            existsSmaller = order.lt(image, point)
          }
          el = orbit.nextInCurrentCheck(el + 1)
        }
        existsSmaller
      }
      if (smallerImage) {
        if (emptyMutableOrbit.words ne null) orbit.clear()
        return false
      }
      orbit.step()
      start = orbit.nextInCurrentCheck(0)
    }
    if (emptyMutableOrbit.words ne null) orbit.clear()
    true
  }

  @inline def iterateOrbit[G:PermutationAction](orbit: MutableOrbit, firstStart: Int, generators: Iterable[G]): Unit = {
    var start = firstStart
    while (start != -1) {
      generators.foreach { g =>
        var el = start
        while (el != -1) {
          val image = el <|+| g
          if (!orbit.inOrbit(image))
            orbit.addNew(image)
          el = orbit.nextInCurrentCheck(el + 1)
        }
      }
      orbit.step()
      start = orbit.nextInCurrentCheck(0)
    }
  }

  @inline def orbitBitMask[G:PermutationAction](domainSize: Int, point: Int, generators: Iterable[G]): Array[Long] =
    orbitBitMask(domainSize, point, generators, new MutableOrbit(null))

  /** Returns the orbit of `point` under the elements `generators`. An optional
    * empty [[MutableOrbit]] can be provided; it will be cleared when the function returns.
    *
    * @param domainSize        Domain size used to allocate a [[MutableOrbit]] if none provided
    * @param emptyMutableOrbit (Optional) Provided empty mutable orbit to avoid allocations
    */
  @inline def orbitBitMask[G:PermutationAction](domainSize: Int, point: Int, generators: Iterable[G],
                                                emptyMutableOrbit: MutableOrbit): Array[Long] = {
    val orbit = if (emptyMutableOrbit.words eq null) forSize(domainSize) else emptyMutableOrbit
    orbit.addNew(point)
    iterateOrbit(orbit, point, generators)
    val bm = orbit.toBitMask
    if (emptyMutableOrbit.words ne null) orbit.clear()
    bm
  }

  @inline def orbitBitMask[G:PermutationAction](domainSize: Int, points: Set[Int], generators: Iterable[G]): Array[Long] =
    orbitBitMask(domainSize, points, generators, new MutableOrbit(null))

  @inline def orbitBitMask[G:PermutationAction](domainSize: Int, points: Set[Int], generators: Iterable[G],
                                                emptyMutableOrbit: MutableOrbit): Array[Long] = {
    if (points.isEmpty) return new Array[Long](0)
    val orbit = if (emptyMutableOrbit.words eq null) forSize(domainSize) else emptyMutableOrbit
    orbit.addNew(points)
    iterateOrbit(orbit, points.min, generators)
    val bm = orbit.toBitMask
    if (emptyMutableOrbit.words ne null) orbit.clear()
    bm
  }

}
