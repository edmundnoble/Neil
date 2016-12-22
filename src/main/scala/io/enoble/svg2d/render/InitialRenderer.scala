package io.enoble.svg2d.render

import io.enoble.svg2d.ast._
import io.enoble.svg2d.data.Renderable

import scalaz.Monoid

case class InitialCode(fragments: Vector[InitialSVG]) extends Renderable {
  override def asString: String = fragments.mkString("\n")
}

object InitialCode {
  val monoid = new Monoid[InitialCode] {
    override def zero = InitialCode(Vector.empty)
    override def append(f1: InitialCode, f2: => InitialCode) = InitialCode(f1.fragments ++ f2.fragments)
  }
}

object InitialRenderer extends FinalSVG[InitialCode] {
  override type Paths = Vector[InitialPath]
  override implicit def monoid[AA >: InitialCode]: Monoid[AA] =
    InitialCode.monoid.asInstanceOf[Monoid[AA]]
  override def circle(x: Double, y: Double, r: Double) = InitialCode(Vector(DrawCircle(x, y, r)))
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double) = InitialCode(Vector(DrawEllipse(x, y, rx, ry)))
  override def text(text: String, x: Double, y: Double) = InitialCode(Vector(DrawText(text, x, y)))
  override def path(paths: Vector[InitialPath]) = InitialCode(Vector(DrawPath(paths)))
  override val path = InitialPathRenderer
}
