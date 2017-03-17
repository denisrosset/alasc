package net.alasc.util

import scala.reflect.ClassTag

import spire.algebra.Order

/**
  * Simple implementation of insertion sort, lifted from spire.math.
  *
  * Works for small arrays but due to O(n^2) complexity is not generally good.
  */
object ParInsertionSort {

  final def sort[@specialized A:Order:ClassTag, B:ClassTag](data: Array[A], external: Array[B]): Unit =
    sort(data, external, 0, data.length)

  final def sort[@specialized A, B](data: Array[A], external: Array[B], start: Int, end: Int)(implicit o:Order[A], ct:ClassTag[A]): Unit = {

    var i = start + 1
    while (i < end) {
      val item = data(i)
      val itemB = external(i)
      var hole = i
      while (hole > start && o.gt(data(hole - 1), item)) {
        data(hole) = data(hole - 1)
        external(hole) = external(hole - 1)
        hole -= 1
      }
      data(hole) = item
      external(hole) = itemB
      i += 1
    }
  }

}

/**
  * In-place quicksort implementation. It is not stable, but does not allocate
  * extra space (other than stack). Lifted from spire.math, adapted to parallel sorting.
  */
object ParQuickSort {
  @inline final def limit: Int = 16

  final def sort[@specialized A:Order:ClassTag, B](data: Array[A], external: Array[B]): Unit = qsort(data, external, 0, data.length - 1)

  final def qsort[@specialized A, B](data: Array[A], external: Array[B], left: Int, right: Int)(implicit o:Order[A], ct:ClassTag[A]): Unit = {

    if (right - left < limit) return ParInsertionSort.sort(data, external, left, right + 1)

    val pivot = left + (right - left) / 2
    val next = partition(data, external, left, right, pivot)
    qsort(data, external, left, next - 1)
    qsort(data, external, next + 1, right)
  }

  final def partition[@specialized A, B](data: Array[A], external: Array[B], left: Int, right: Int, pivot: Int)(implicit o:Order[A], ct:ClassTag[A]): Int = {

    val value = data(pivot)

    //swap(pivot, right)
    var tmp = data(pivot); data(pivot) = data(right); data(right) = tmp
    var tmpB = external(pivot); external(pivot) = external(right); external(right) = tmpB

    var store = left
    var i = left
    while (i < right) {
      if (o.lt(data(i), value)) {
        //swap(i, store)
        tmp = data(i); data(i) = data(store); data(store) = tmp
        tmpB = external(i); external(i) = external(store); external(store) = tmpB
        store += 1
      }
      i += 1
    }
    //swap(store, right)
    tmp = data(store); data(store) = data(right); data(right) = tmp
    tmpB = external(store); external(store) = external(right); external(right) = tmpB
    store

  }

}
