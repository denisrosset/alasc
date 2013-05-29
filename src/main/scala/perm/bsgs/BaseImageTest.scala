package com.faacets
package perm

trait BaseImageTest {
  def apply(baseImage: Dom): (Boolean, BaseImageTest)
}

object TrivialBaseImageTest extends BaseImageTest {
  def apply(baseImage: Dom) = (true, this)
}
