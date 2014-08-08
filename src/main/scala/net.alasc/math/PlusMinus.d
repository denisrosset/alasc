
/*

  def plus(lhs: Perm, n: Int): Perm =
    if (n < 0)
      minus(lhs, -n)
    else if (n == 0)
      lhs
    else {
      var maxSup = lhs.supportMax + n
      if (maxSup <= Perm16.Algebra.supportMaxElement) {
        var res = 0L
        while (maxSup >= n) {
          res += Perm16Encoding.encode(maxSup, lhs.image(maxSup - n) + n)
          maxSup -= 1
        }
        new Perm16(new Perm16Val(res))
      } else if (maxSup <= Perm32.Algebra.supportMaxElement) {
        val res = new Perm32
        while (maxSup >= n) {
          res.encode(maxSup, lhs.image(maxSup - n) + n)
          maxSup -= 1
        }
        res
      } else {
        val array = new Array[Int](maxSup + 1)
        while (maxSup >= n) {
          array(maxSup) = lhs.image(maxSup - n) + n
          maxSup -= 1
        }
        while (maxSup >= 0) {
          array(maxSup) = maxSup
          maxSup -= 1
        }
        new PermArray(array)
      }
    }

  def minus(lhs: Perm, n: Int): Perm =
    if (n < 0)
      plus(lhs, -n)
    else if (n == 0)
      lhs
    else {
      if (n > lhs.supportMin)
        sys.error(s"Cannot shift down by $n the permutation, because ${lhs.supportMin} is in the support.")
      var maxSup = lhs.supportMax - n
      if (maxSup <= Perm16.Algebra.supportMaxElement) {
        var res = 0L
        while (maxSup >= 0) {
          res += Perm16Encoding.encode(maxSup, lhs.image(maxSup + n) - n)
          maxSup -= 1
        }
        new Perm16(new Perm16Val(res))
      } else if (maxSup <= Perm32.Algebra.supportMaxElement) {
        val res = new Perm32
        while (maxSup >= 0) {
          res.encode(maxSup, lhs.image(maxSup + n) - n)
          maxSup -= 1
        }
        res
      } else {
        val array = new Array[Int](maxSup + 1)
        while (maxSup >= 0) {
          array(maxSup) = lhs.image(maxSup + n) - n
          maxSup -= 1
        }
        new PermArray(array)
      }
    }
}
 */


/*


  def minus(n: Int): Perm32 =
    if (n >= Perm32.Algebra.supportMaxElement)
      sys.error(s"Does not support shifts of more than ${Perm32.Algebra.supportMaxElement} positions.")
    else if (n == 0)
      lhs
    else if (n < 0)
      plus(-n)
    else if (n >= long1Start) {
      assert(long0 == 0)
      new Perm32(0L, long2, long1).specMinus(n - long1Start)
    } else {
      assert((long0 & LongBits.rightFill(n * maskWidth)) == 0)
      val nBits = n * maskWidth
      val leftShift = (numPerLong - n) * maskWidth
      val rFill = rightFill(nBits)
      new Perm32(long2 >>> nBits,
        ((long2 & rFill) << leftShift) + (long1 >>> nBits),
        ((long1 & rFill) << leftShift) + (long0 >>> nBits))
    }

  def plus(n: Int): Perm32 =
    if (n >= Perm32.Algebra.supportMaxElement)
      sys.error(s"Does not support shifts of more than ${Perm32.Algebra.supportMaxElement} positions.")
    else if (n == 0)
      lhs
    else if (n < 0)
      minus(-n)
    else if (n >= long1Start) {
      assert(long2 == 0)
      new Perm32(long1, long0, 0L).specPlus(n - long1Start)
    } else {
      val nBits = n * maskWidth
      val rightShift = (numPerLong - n) * maskWidth
      new Perm32(((long2 << nBits) & longMask) + (long1 >>> rightShift),
        ((long1 << nBits) & longMask) + (long0 >>> rightShift),
        (long0 << nBits) & longMask)
    }
}
 */
