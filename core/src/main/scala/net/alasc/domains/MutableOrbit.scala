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

}

object MutableOrbit {

  final val logWL = 5
  final val wordLength = 32

  def nWordsForSize(n: Int): Int = if (n == 0) 0 else (n - 1) / wordLength + 1

  def forSize(n: Int): MutableOrbit = new MutableOrbit(new Array[Long](nWordsForSize(n)))

  /** Tests whether `point` is the smallest point in its orbit under `generators`. An optional
    * empty [[MutableOrbit]] can be provided; it will be cleared when the function returns.
    *
 *
    * @param domainSize        Domain size used to allocate a [[MutableOrbit]] if none provided
    * @param emptyMutableOrbit (Optional) Provided empty mutable orbit to avoid allocations
    */
  @inline def isSmallestPointInOrbit[G]
    (domainSize: Int, point: Int, generators: Iterable[G],
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

}
