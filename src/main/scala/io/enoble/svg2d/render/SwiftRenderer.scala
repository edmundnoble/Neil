package io
package enoble
package svg2d
package render

import io.enoble.svg2d.ast.{FastMonoid, FinalSVG}

final case class SwiftRenderer[A](stringyMonoid: FastMonoid[String, A]) extends FinalSVG[A]  {
  override type Paths = Nothing
  override val empty = stringyMonoid.monoid.empty
  override def append(f1: A, f2: A): A =
    stringyMonoid.monoid.combine(f1, f2)
  override def circle(x: Double, y: Double, r: Double) = ???
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = ???
  override def text(text: String, x: Double, y: Double) = ???
  override def includePath(paths: Nothing) = ???
  override lazy val path = ???
}
