package io.enoble.svg2d.render

import io.enoble.svg2d.Main.Swift
import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.data.SwiftCode

import scalaz.Monoid

object SwiftRenderer extends FinalSVG[SwiftCode]  {
  override type Paths = Nothing
  override implicit def monoid[AA >: SwiftCode] = SwiftCode.swiftMonoid.asInstanceOf[Monoid[AA]]
  override def circle(x: Double, y: Double, r: Double) = ???
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = ???
  override def text(text: String, x: Double, y: Double) = ???
  override def path(paths: Nothing) = ???
  override val path = ???
}
