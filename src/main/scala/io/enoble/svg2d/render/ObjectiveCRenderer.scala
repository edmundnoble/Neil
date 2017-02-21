package io
package enoble
package svg2d
package render

import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.data.{ObjectiveCCode, SwiftCode}

import scalaz.Monoid

object ObjectiveCRenderer extends FinalSVG[ObjectiveCCode] {
  override type Paths = Nothing
  override val empty = ObjectiveCCode(Vector.empty)
  override def append(f1: ObjectiveCCode, f2: ObjectiveCCode): ObjectiveCCode =
    ObjectiveCCode(f1.fragments ++ f2.fragments)
  override def circle(x: Double, y: Double, r: Double) = ???
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = ???
  override def text(text: String, x: Double, y: Double) = ???
  override def includePath(paths: Nothing) = ???
  override lazy val path = ???
}
