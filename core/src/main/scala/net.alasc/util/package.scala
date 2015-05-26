package net.alasc

import scala.reflect.ClassTag

package object util extends NNOptionTopLevel with OptionTuple2NNTopLevel with RichIteratorOps {
  def arrayGrow[T: ClassTag](array: Array[T], minSize: Int = 8): Array[T] = {
    val newSize = scala.math.max(array.length * 2, minSize)
    val newArray = new Array[T](newSize)
    Array.copy(array, 0, newArray, 0, array.length)
    newArray
  }
  def arrayTrim[T: ClassTag](array: Array[T], size: Int): Array[T] =
    if (array.length == size) array else {
      require(size < array.length)
      val newArray = new Array[T](size)
      Array.copy(array, 0, newArray, 0, size)
      newArray
    }
}
