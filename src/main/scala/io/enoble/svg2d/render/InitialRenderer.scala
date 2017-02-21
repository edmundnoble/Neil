package io
package enoble
package svg2d
package render

import io.enoble.svg2d.ast._
import io.enoble.svg2d.data.Renderable

case class InitialCode(fragments: Vector[InitialSVG]) extends Renderable {
  override def asString: String = fragments.mkString("\n")
}

object InitialRenderer extends FinalSVG[InitialCode] {
  override type Paths = Vector[InitialPath]
  override val empty: InitialCode = InitialCode(Vector.empty)
  override def append(fst: InitialCode, snd: InitialCode) = InitialCode(fst.fragments ++ snd.fragments)
  override def circle(x: Double, y: Double, r: Double) = InitialCode(Vector(DrawCircle(x, y, r)))
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = InitialCode(Vector(DrawEllipse(x, y, rx, ry)))
  override def text(text: String, x: Double, y: Double) = InitialCode(Vector(DrawText(text, x, y)))
  override def includePath(paths: Vector[InitialPath]) = InitialCode(Vector(DrawPath(paths)))
  override val path = InitialPathRenderer
}
