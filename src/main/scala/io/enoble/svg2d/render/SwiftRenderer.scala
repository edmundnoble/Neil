package io.enoble.svg2d.render

import io.enoble.svg2d.Main.Swift
import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.data.SwiftCode

import scalaz.Monoid

object SwiftRenderer extends FinalSVG[SwiftCode]  {
  override type Paths = Nothing
  override val empty = SwiftCode(Vector.empty)
  override def append(f1: SwiftCode, f2: SwiftCode): SwiftCode =
    SwiftCode(f1.fragments ++ f2.fragments)
  override def circle(x: Double, y: Double, r: Double) = ???
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = ???
  override def text(text: String, x: Double, y: Double) = ???
  override def path(paths: Nothing) = ???
  override lazy val path = ???
}
