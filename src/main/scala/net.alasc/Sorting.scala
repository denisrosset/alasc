package net.alasc

object Sorting {
  // taken from src/library/scala/util/Sorting.scala in Scala repository, and
  // specialized for Int type to avoid boxing
  def stableSortIntAccordingTo(a: Array[Int], lo: Int, hi: Int, scratch: Array[Int], f: (Int, Int) => Boolean) {
    if (lo < hi) {
      val mid = (lo+hi) / 2
      stableSortIntAccordingTo(a, lo, mid, scratch, f)
      stableSortIntAccordingTo(a, mid+1, hi, scratch, f)
      var k, t_lo = lo
      var t_hi = mid + 1
      while (k <= hi) {
        if ((t_lo <= mid) && ((t_hi > hi) || (!f(a(t_hi), a(t_lo))))) {
          scratch(k) = a(t_lo)
          t_lo += 1
        } else {
          scratch(k) = a(t_hi)
          t_hi += 1
        }
        k += 1
      }
      k = lo
      while (k <= hi) {
        a(k) = scratch(k)
        k += 1
      }
    }
  }
  def quickSortIntAccordingTo(x: Array[Int], off: Int, len: Int, leftLessRight: (Int, Int) => Boolean) {
    def swap(a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
    }
    def vecswap(_a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(a, b)
        i += 1
        a += 1
        b += 1
      }
    }
    def med3(a: Int, b: Int, c: Int) = {
      if (leftLessRight(x(a), x(b))) {
        if (leftLessRight(x(b), x(c))) b else if (leftLessRight(x(a), x(c))) c else a
      } else {
        if (leftLessRight(x(c), x(b))) b else if (leftLessRight(x(c), x(a))) c else a
      }
    }
    def sort2(off: Int, len: Int) {
      // Insertion sort on smallest arrays
      if (len < 7) {
        var i = off
        while (i < len + off) {
          var j = i
          while (j > off && leftLessRight(x(j), x(j-1))) {
            swap(j, j-1)
            j -= 1
          }
          i += 1
        }
      } else {
        // Choose a partition element, v
        var m = off + (len >> 1)        // Small arrays, middle element
        if (len > 7) {
          var l = off
          var n = off + len - 1
          if (len > 40) {        // Big arrays, pseudomedian of 9
            val s = len / 8
            l = med3(l, l+s, l+2*s)
            m = med3(m-s, m, m+s)
            n = med3(n-2*s, n-s, n)
          }
          m = med3(l, m, n) // Mid-size, med of 3
        }
        val v = x(m)

        // Establish Invariant: v* (<v)* (>v)* v*
        var a = off
        var b = a
        var c = off + len - 1
        var d = c
        var done = false
        while (!done) {
          while (b <= c && (leftLessRight(x(b), v) || x(b) == v)) {
            if (x(b) == v) {
              swap(a, b)
              a += 1
            }
            b += 1
          }
          while (c >= b && !leftLessRight(x(c), v)) {
            if (x(c) == v) {
              swap(c, d)
              d -= 1
            }
            c -= 1
          }
          if (b > c) {
            done = true
          } else {
            swap(b, c)
            c -= 1
            b += 1
          }
        }

        // Swap partition elements back to middle
        val n = off + len
        var s = math.min(a-off, b-a)
        vecswap(off, b-s, s)
        s = math.min(d-c, n-d-1)
        vecswap(b,   n-s, s)

        // Recursively sort non-partition-elements
        s = b - a
        if (s > 1)
          sort2(off, s)
        s = d - c
        if (s > 1)
          sort2(n-s, s)
      }
    }
    sort2(off, len)
  }
}
