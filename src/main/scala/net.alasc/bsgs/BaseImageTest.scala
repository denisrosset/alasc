package net.alasc
package bsgs

trait BaseImageTest {
  def apply(baseImage: Dom): (Boolean, BaseImageTest)
}

object TrivialBaseImageTest extends BaseImageTest {
  def apply(baseImage: Dom) = (true, this)
}
