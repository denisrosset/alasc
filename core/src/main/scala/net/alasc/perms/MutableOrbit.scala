package net.alasc.perms

import spire.syntax.cfor._
import spire.math.{min, max}

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
final class MutableOrbit(var words: Array[Long], var nWords: Int) {

  import MutableOrbit.{logWL, wordLength, compact, expand, notInOrbit, alreadyChecked, inCurrentCheck, inNextCheck}
  import metal.util.nextPowerOfTwo

  def status(k: Int): Int = {
    val w = k >>> logWL
    if (w >= nWords) notInOrbit.toInt else ((words(w) >>> (k * 2)) & 3L).toInt
  }


  override def toString = {
    def withStatus(s: Long): String = (0 until words.length * nWords).filter(status(_) == s).mkString(",")
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
    cforRange(0 until nWords) { w =>
      val word = words(w)
      val lowBits = mask(word)
      val highBits = (word & ~(word << 1)) & 0xAAAAAAAAAAAAAAAAL // 1010 ... 1010
      words(w) = lowBits | highBits
    }
  }

  def clear(): Unit = {
    cforRange(0 until nWords) { w => words(w) = 0L }
    nWords = 0
  }


  /** Checks whether `k` is part of the orbit. */
  def inOrbit(k: Int): Boolean = {
    val w = k >>> logWL
    if (w >= nWords) false else {
      val word = words(w)
      ((word | (word >>> 1)) & (1L << (k * 2))) != 0L
    }
  }

  def resizeTo(maxWords: Int): Unit = {
    val newWords = new Array[Long](maxWords)
    java.lang.System.arraycopy(words, 0, newWords, 0, nWords)
    words = newWords
  }

  /** Adds the new element `k` to the orbit. Requires (but does not check) that `k` is not part of the orbit. */
  def addNew(k: Int): Unit = {
    val w = k >>> logWL
    if (w >= words.length)
      resizeTo(nextPowerOfTwo(w + 1))
    words(w) = words(w) | (inNextCheck << (k * 2))
    nWords = max(nWords, w + 1)
  }

  /** Adds the set to the orbit. Requires (but does not check) that the added elements are not part of the orbit. */
  def addNew(set: Set[Int]): Unit = {
    set match {
      case bs1: scala.collection.immutable.BitSet.BitSet1 =>
        val word0 = bs1.elems.toInt
        val word1 = (bs1.elems >>> 32).toInt
        if (word1 != 0L) {
          if (words.length < 2)
            resizeTo(2)
          words(1) |= expand(word1) * inNextCheck
          nWords = max(nWords, 2)
        }
        if (word0 != 0L) {
          if (words.length < 1)
            resizeTo(2)
          words(0) |= expand(word0) * inNextCheck
          nWords = max(nWords, 1)
        }
      case bs: scala.collection.BitSet =>
        val array = bs match {
          case bsn: scala.collection.immutable.BitSet.BitSetN => bsn.elems
          case _ => bs.toBitMask
        }
        if (array.length == 0) return
        var w2 = array.length - 1
        while (array(w2) == 0L) {
          w2 -= 1
          if (w2 == -1) return
        }
        if (2*w2 + 1 >= words.length)
          resizeTo(nextPowerOfTwo(2*w2 + 2))
        nWords = max(nWords, 2*w2 + 2) // optimality of nWords can be off by 1
        while (w2 >= 0) {
          val word2 = array(w2)
          val word0 = word2.toInt
          val word1 = (word2 >>> 32).toInt
          words(2*w2) |= expand(word0) * inNextCheck
          words(2*w2 + 1) |= expand(word1) * inNextCheck
          w2 -= 1
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
    if (w >= nWords) return -1
    var word = mask(words(w) & ((-1L) << (k * 2)))
    while (true) {
      if (word != 0)
        return (w * wordLength) + java.lang.Long.numberOfTrailingZeros(word)/2
      w += 1
      if (w == nWords) return -1
      word = mask(words(w))
    }
    -1
  }

  def nBitMaskWords: Int = if (nWords == 0) 0 else (nWords - 1)/2 + 1

  @inline def toLongUnsigned(i: Int): Long = i.toLong & 0xFFFFFFFFL

  @inline def bitMaskWord(w2: Int): Long = {
    val word0 = words(w2 * 2)
    val word1 = if (w2 * 2 + 1 == nWords) 0L else words(w2 * 2 + 1)
    toLongUnsigned(compact(word0)) + (toLongUnsigned(compact(word1)) << 32)
  }

  /** Returns a bitmask appropriate for a bitset. */
  def toBitMask: Array[Long] = {
    if (nWords == 0) return new Array[Long](0)
    val array = new Array[Long](nBitMaskWords)
    cforRange(0 until array.length) { w2 =>
      array(w2) = bitMaskWord(w2)
    }
    array
  }

  def toMetalBitSet: metal.immutable.BitSet = {
    val bm = toBitMask
    metal.immutable.BitSet.fromBitmaskNoCopy(bm, bm.length)
  }

}

object MutableOrbit {

  def &=(lhs: metal.mutable.BitSet, rhs: MutableOrbit): lhs.type = {
    cforRange(0 until min(lhs.nWords, rhs.nBitMaskWords)) { w =>
      lhs.words(w) &= rhs.bitMaskWord(w)
    }
    if (rhs.nBitMaskWords < lhs.nWords)
      java.util.Arrays.fill(lhs.words, rhs.nBitMaskWords, lhs.nWords, 0L)
    lhs
  }

  def &~=(lhs: metal.mutable.BitSet, rhs: MutableOrbit): lhs.type = {
    cforRange(0 until min(lhs.nWords, rhs.nBitMaskWords)) { w =>
      lhs.words(w) &= ~rhs.bitMaskWord(w)
    }
    lhs
  }

  final val logWL = 5
  final val wordLength = 32
  final val defaultMaxWords = 4

  final val notInOrbit = 0L
  final val alreadyChecked = 1L
  final val inCurrentCheck = 3L
  final val inNextCheck = 2L

  def empty: MutableOrbit = new MutableOrbit(new Array[Long](defaultMaxWords), 0)

  def nWordsForSize(n: Int): Int = if (n == 0) 0 else (n - 1) / wordLength + 1

  def forSize(n: Int): MutableOrbit = new MutableOrbit(new Array[Long](nWordsForSize(n)), 0)

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

}
