package net.alasc.util

class GenericArray[T](val array: Array[_ <: AnyRef]) extends AnyVal {

  @inline final def apply(idx: Int): T = array(idx).asInstanceOf[T]

  @inline final def length: Int = array.length

}

object GenericArray {

  def apply[T](iterable: Iterable[T]): GenericArray[T] = iterable match {
    case wa: collection.mutable.WrappedArray.ofRef[_] => new GenericArray[T](wa.array)
    case _ =>
      val array = new Array[AnyRef](iterable.size)
      var i = 0
      var it = iterable.iterator
      while (it.hasNext) {
        array(i) = it.next.asInstanceOf[AnyRef]
        i += 1
      }
      new GenericArray[T](array)
  }

}

/*
abstract class ExtractArray0 {

  trait Extractor[@specialized T] {
    def apply(iterable: Iterable[T]): Array[T]
  }

  implicit def OfOther[T:ClassTag]: Extractor[T] = new Extractor[T] {
    def apply(iterable: Iterable[T]): Array[T] = iterable match {
      case wa: collection.mutable.WrappedArray.ofRef[T] => wa.array
      case _ => iterable.toArray
    }
  }

}
*/
/*
object ExtractArray extends ExtractArray0 {

  implicit object OfInt extends Extractor[Int] {
    def apply(iterable: Iterable[Int]): Array[Int] = iterable match {
      case wa: collection.mutable.WrappedArray.ofInt => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfDouble extends Extractor[Double] {
    def apply(iterable: Iterable[Double]): Array[Double] = iterable match {
      case wa: collection.mutable.WrappedArray.ofDouble => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfLong extends Extractor[Long] {
    def apply(iterable: Iterable[Long]): Array[Long] = iterable match {
      case wa: collection.mutable.WrappedArray.ofLong => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfFloat extends Extractor[Float] {
    def apply(iterable: Iterable[Float]): Array[Float] = iterable match {
      case wa: collection.mutable.WrappedArray.ofFloat => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfChar extends Extractor[Char] {
    def apply(iterable: Iterable[Char]): Array[Char] = iterable match {
      case wa: collection.mutable.WrappedArray.ofFloat => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfByte extends Extractor[Byte] {
    def apply(iterable: Iterable[Byte]): Array[Byte] = iterable match {
      case wa: collection.mutable.WrappedArray.ofFloat => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfShort extends Extractor[Short] {
    def apply(iterable: Iterable[Short]): Array[Short] = iterable match {
      case wa: collection.mutable.WrappedArray.ofFloat => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfBoolean extends Extractor[Boolean] {
    def apply(iterable: Iterable[Boolean]): Array[Boolean] = iterable match {
      case wa: collection.mutable.WrappedArray.ofFloat => wa.array
      case _ => iterable.toArray
    }
  }

  implicit object OfUnit extends Extractor[Unit] {
    def apply(iterable: Iterable[Unit]): Array[Unit] = iterable match {
      case wa: collection.mutable.WrappedArray.ofFloat => wa.array
      case _ => iterable.toArray
    }
  }

  def apply[@specialized T](iterable: Iterable[T])(implicit extractor: Extractor[T]): Array[T] = extractor(iterable)


}
*/
