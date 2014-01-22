package net.alasc


abstract class ArrayPerm[@specialized(Int, Short, Byte) T] extends PermLike {
  import scala.util.hashing.MurmurHash3
  import Dom.ZeroBased._
  val array: Array[T]

  protected def fromInt(k: Int): T
  protected def toInt(t: T): Int

  protected def makeArray(n: Int): Array[T]

  protected def arrayProduct(thatArray: Array[T]): Array[T] = {
    assert(array.length == thatArray.length)
    val newArray = makeArray(array.length)
    var i = 0
    while (i < array.length) {
      newArray(i) = thatArray(toInt(array(i)))
      i += 1
    }
    newArray
  }

  protected def arrayInverse: Array[T] = {
    val newArray = makeArray(array.length)
    var i = 0
    while (i < array.length) {
      newArray(toInt(array(i))) = fromInt(i)
      i += 1
    }
    newArray
  }

  protected def arrayEqual(thatArray: Array[T]): Boolean = {
    assert(array.length == thatArray.length)
    var i = 0
    while (i < array.length) {
      if (array(i) != thatArray(i))
        return false
      i += 1
    }
    true
  }

  final def image(k: Dom) = toInt(array(k))

  final override def hash = {
    var h = Perm.hashSeed
    var i = 0
    while (i < array.length) {
      h = MurmurHash3.mix(h, toInt(array(i)))
      i += 1
    }
    MurmurHash3.finalizeHash(h, array.length)
  }

  final def size = array.length

  def isIdentity: Boolean = {
    var i = 0
    while (i < array.length) {
      if (image(i) !== i)
        return false
      i += 1
    }
    true
  }
}

trait ArrayPermBuilder extends PermBuilder with PermBuilderLike {
  val maxSize: Int
}

trait ArrayPermBuilderLike extends ArrayPermBuilder {
  def apply(n: Int) = fromImages(n)(k => k)
}

final class IntPerm private[alasc](val array: Array[Int]) extends ArrayPerm[Int] {
  import Dom.ZeroBased._
  def builder = IntPerm

  final def makeArray(n: Int) = new Array[Int](n)
  final def toInt(k: Int) = k
  final def fromInt(k: Int) = k

  final def *(perm: Perm) = perm match {
    case that: IntPerm => new IntPerm(arrayProduct(that.array))
    case  _ => builder.fromImages(size)(k => perm.image(image(k)))
  }

  final def inverse = new IntPerm(arrayInverse)

  def ===(perm: Perm): Boolean = perm match {
    case that: IntPerm => arrayEqual(that.array)
    case _ => (0 until size).forall( k => image(k) == perm.image(k) )
  }
}

object IntPerm extends ArrayPermBuilderLike {
  val maxSize = Int.MaxValue
  def fromImages(size: Int)(f: Dom => Dom): IntPerm = {
    import Dom.ZeroBased._
    new IntPerm(Array.tabulate(size)(k => f(k)))
  }
  def fromPreimages(size: Int)(f: Dom => Dom): IntPerm = {
    import Dom.ZeroBased._
    val array = new Array[Int](size)
    array.indices.foreach( i => array(f(i)) = i )
    new IntPerm(array)
  }
}

final class ShortPerm private[alasc](val array: Array[Short]) extends ArrayPerm[Short] {
  import Dom.ZeroBased._
  def builder = ShortPerm

  final def makeArray(n: Int) = new Array[Short](n)
  final def toInt(k: Short) = k & 0xFFFF
  final def fromInt(k: Int) = k.toShort

  final def *(perm: Perm) = perm match {
    case that: ShortPerm => new ShortPerm(arrayProduct(that.array))
    case  _ => builder.fromImages(size)(k => perm.image(image(k)))
  }

  final def inverse = new ShortPerm(arrayInverse)
  def ===(perm: Perm): Boolean = perm match {
    case that: ShortPerm => arrayEqual(that.array)
    case _ => (0 until size).forall( k => image(k) == perm.image(k) )
  }
}

object ShortPerm extends ArrayPermBuilderLike {
  val maxSize = 65536

  def fromImages(size: Int)(f: Dom => Dom): ShortPerm = {
    import Dom.ZeroBased._
    new ShortPerm(Array.tabulate(size)(k => f(k).toShort))
  }

  def fromPreimages(size: Int)(f: Dom => Dom): ShortPerm = {
    import Dom.ZeroBased._
    val array = new Array[Short](size)
    array.indices.foreach( i => array(f(i)) = i.toShort )
    new ShortPerm(array)
  }
}

final class BytePerm private[alasc](val array: Array[Byte])  extends ArrayPerm[Byte] {
  import Dom.ZeroBased._
  def builder = BytePerm

  final def makeArray(n: Int) = new Array[Byte](n)
  final def toInt(k: Byte) = k & 0xFF
  final def fromInt(k: Int) = k.toByte

  final def *(perm: Perm) = perm match {
    case that: BytePerm => new BytePerm(arrayProduct(that.array))
    case  _ => builder.fromImages(size)(k => perm.image(image(k)))
  }

  final def inverse = new BytePerm(arrayInverse)
  def ===(perm: Perm): Boolean = perm match {
    case that: BytePerm => arrayEqual(that.array)
    case _ => (0 until size).forall( k => image(k) == perm.image(k) )
  }
}

object BytePerm extends ArrayPermBuilderLike {
  val maxSize = 256
  def unsafe(array: Array[Byte]): BytePerm = {
    assert(array.length <= maxSize)
    new BytePerm(array)
  }
  def fromImages(size: Int)(f: Dom => Dom): BytePerm = {
    import Dom.ZeroBased._
    new BytePerm(Array.tabulate(size)(k => f(k).toByte))
  }
  def fromPreimages(size: Int)(f: Dom => Dom): BytePerm = {
    import Dom.ZeroBased._
    val array = new Array[Byte](size)
    array.indices.foreach( i => array(f(i)) = i.toByte )
    new BytePerm(array)
  }
}

final class CompactPerm private[alasc](val size: Int, val images: Long) extends PermLike {
  import Dom.ZeroBased._
  def builder = CompactPerm
  final def image(k: Dom) = (images >>> (4 * k)).toInt & 15

  final def *(perm: Perm) = perm match {
    case that: CompactPerm =>
      val n = size
      var newImages: Long = 0
      var i = 0
      while (i < n) {
        newImages += that.image(image(i)).toLong << (4 * i)
        i += 1
      }
      new CompactPerm(size, newImages)
    case  _ => builder.fromImages(size)(k => perm.image(image(k)))
  }

  final def inverse = {
    val n = size
    var newImages: Long = 0
    var i = 0
    while (i < n) {
      newImages += i.toLong << (4 * image(i))
      i += 1
    }
    new CompactPerm(size, newImages)
  }
  def ===(perm: Perm) = perm match {
    case that: CompactPerm => images == that.images
    case _ => (0 until size).forall( k => image(k) == perm.image(k) )
  }
  def isIdentity = CompactPerm.apply(size).images == images
}

object CompactPerm extends PermBuilder with PermBuilderLike {
  val maxSize = 16
  val cachedIdentity: Array[Long] = (1 to 16).map( size => fromImages(size)(k => k).images ).toArray
  def apply(n: Int) = new CompactPerm(n, cachedIdentity(n - 1))
  def fromImages(size: Int)(f: Dom => Dom): CompactPerm = {
    import Dom.ZeroBased._
    var newImages: Long = 0
    val n = size
    var i = 0
    while (i < n) {
      newImages += f(i).toLong << (4 * i)
      i += 1
    }
    new CompactPerm(size, newImages)
  }
  def fromPreimages(size: Int)(f: Dom => Dom): CompactPerm = {
    import Dom.ZeroBased._
    var newImages: Long = 0
    val n = size
    var i = 0
    while (i < n) {
      newImages += i.toLong << (4 * f(i))
      i += 1
    }
    new CompactPerm(size, newImages)
  }
}

final class IndexPerm private[alasc](val size: Int, val index: Int, val byteArray: Array[Byte])
    extends PermLike {

  private[this] var computedInverse = this
  private[this] var computedProducts = Array.empty[IndexPerm]

  protected def setComputed(cInverse: IndexPerm, cProducts: Array[IndexPerm]) {
    computedInverse = cInverse
    computedProducts = cProducts
  }

  import Dom.ZeroBased._
  def builder = IndexPerm

  final def image(k: Dom) = byteArray(k).toInt

  final override val hash = BytePerm.unsafe(byteArray).hash

  final def *(perm: Perm) = perm match {
    case that: IndexPerm => computedProducts(that.index)
    case  _ => builder.fromImages(size)(k => perm.image(image(k)))
  }

  final def inverse = computedInverse

  def ===(perm: Perm): Boolean = perm match {
    case that: IndexPerm => (size == that.size) && (index == that.index)
    case _ => (0 until size).forall( k => image(k) == perm.image(k) )
  }
  val isIdentity = BytePerm.unsafe(byteArray).isIdentity
}

object IndexPerm extends PermBuilder with PermBuilderLike {
  import Dom.ZeroBased._

  val maxSize = 5
  require(maxSize <= 5)

  def imagesKey(size: Int, f: Dom => Dom) = {
    import Dom.ZeroBased._
    var key = 0
    val n = size
    var i = 0
    while (i < n) {
      key += f(i) << (3 * i)
      i += 1
    }
    key * 8 + size
  }

  def preimagesKey(size: Int, f: Dom => Dom) = {
    import Dom.ZeroBased._
    var key = 0
    val n = size
    var i = 0
    while (i < n) {
      key += i << (3 * f(i))
      i += 1
    }
    key * 8 + size
  }

  import scala.collection.mutable.HashMap

  protected val poolMap = HashMap.empty[Int, IndexPerm]

  val pool: Array[Array[IndexPerm]] = Array.tabulate(maxSize + 1) { n =>
    val identity = Array.tabulate(n)( k => k.toByte )
    identity.permutations.zipWithIndex.map {
      case (images, index) => {
        val newIndexPerm = new IndexPerm(n, index, images)
        poolMap += imagesKey(n, newIndexPerm.image) -> newIndexPerm
        newIndexPerm
      }
    }.toArray
  }

  def fromImages(n: Int)(f: Dom => Dom): IndexPerm = poolMap(imagesKey(n, f))
  def fromPreimages(n: Int)(f: Dom => Dom): IndexPerm = poolMap(preimagesKey(n, f))

  def apply(n: Int) = identity(n)

  val identity: Array[IndexPerm] = Array.tabulate(maxSize + 1) {
    case 0 => fromImages(1)(k => k)
    case n => fromImages(n)(k => k)
  }

  for (n <- 1 to maxSize) {
    for (i <- pool(n)) {
      val inverse = fromPreimages(n)(i.image)
      val products = pool(n).map(
        j => fromImages(n)(k => j.image(i.image(k)))
      )
      i.setComputed(inverse, products)
    }
  }
}

