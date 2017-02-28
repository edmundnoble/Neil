package io
package enoble
package svg2d
package render

import io.enoble.svg2d.ast.{FastMonoid, FinalPath, FinalSVG}
import shapeless.HNil

final case class ObjectiveCRenderer[A](stringyMonoid: FastMonoid[String, A]) extends FinalSVG[A] {
  override type Paths = HNil
  override val empty = stringyMonoid.monoid.empty
  override def append(f1: A, f2: A): A =
    stringyMonoid.monoid.combine(f1, f2)
  override def circle(x: Double, y: Double, r: Double) = empty
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = empty
  override def text(text: String, x: Double, y: Double) = empty
  override def includePath(paths: HNil) = empty
  override val path: FinalPath[HNil] = FinalPath.const(HNil)
}
