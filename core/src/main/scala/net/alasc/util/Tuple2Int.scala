package net.alasc.util

class Tuple2Int(val encoding: Long) extends AnyVal {
  import Tuple2Int._
  def _1: Int = (encoding & rightMask).toInt
  def _2: Int = ((encoding & leftMask) >> 32).toInt
  def isEmpty: Boolean = false
  def get: Tuple2Int = this
}

object Tuple2Int {
  def unapply(tuple2: Tuple2Int): Tuple2Int = tuple2
  def apply(_1: Int, _2: Int): Tuple2Int = new Tuple2Int(((_1.toLong) & rightMask) + (_2.toLong << 32))
  @inline def leftMask: Long  = (0xFFFFFFFFL) << 32
  @inline def rightMask: Long = 0xFFFFFFFFL
}

class OptionTuple2NN(val encoding: Long) extends AnyVal {
  import OptionTuple2NN._
  def _1: Int = {
    assert(encoding >= 0)
    (encoding & rightMask).toInt
  }
  def _2: Int = {
    assert(encoding >= 0)
    ((encoding & leftMask) >> 32).toInt
  }
  def isEmpty: Boolean = encoding < 0
  def nonEmpty: Boolean = encoding >= 0
  def get: Tuple2Int = Tuple2Int(_1, _2)
}

object OptionTuple2NN {
  def unapply(optionTuple2NN: OptionTuple2NN): OptionTuple2NN = optionTuple2NN
  @inline def leftMask: Long  = (0xFFFFFFFFL) << 32
  @inline def rightMask: Long = 0xFFFFFFFFL
}

trait OptionTuple2NNTopLevel {
  import OptionTuple2NN._

  final def SomeTuple2NN(_1: Int, _2: Int): OptionTuple2NN = {
    require(_1 >= 0 && _2 >= 0)
    new OptionTuple2NN(((_1.toLong) & rightMask) + (_2.toLong << 32))
  }
  final def NoneTuple2NN: OptionTuple2NN = new OptionTuple2NN(-1L)
}
