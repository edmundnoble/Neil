package io
package enoble
package svg2d
package render

import io.enoble.svg2d.ast.{FastMonoid, FinalPath, FinalSVG}
import shapeless.HNil

final case class SwiftRenderer[A](stringyMonoid: FastMonoid[String, A]) extends FinalSVG[A]  {
  override type Paths = HNil
  override val empty = stringyMonoid.empty
  override def append(f1: A, f2: A): A =
    stringyMonoid.append(f1, f2)
  override def circle(x: Double, y: Double, r: Double) = empty
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = empty
  override def text(text: String, x: Double, y: Double) = empty
  override def includePath(paths: HNil) = empty
  override val path: FinalPath[HNil] = FinalPath.const(HNil)
  override def rect(x: Double, y: Double, w: Double, h: Double): A = empty
}
