package io
package enoble
package svg2d
package render

import cats.Monoid
import io.enoble.svg2d.ast._
import io.enoble.svg2d.ast.InitialSVG._

case class InitialCode(fragments: Vector[InitialSVG]) {
  def asString: String = fragments.mkString("\n")
}

object InitialCode {
  def initialCodeMonoid: Monoid[InitialCode] = new Monoid[InitialCode] {
    override def empty: InitialCode = InitialCode(Vector.empty)
    override def combine(x: InitialCode, y: InitialCode): InitialCode = InitialCode(x.fragments ++ y.fragments)
  }
}

final case class InitialRenderer[A](stringy: FastMonoid[InitialSVG, A]) extends FinalSVG[A] {

  import stringy._

  override type Paths = Vector[InitialPath]

  override val empty: A =
    monoid.empty

  override def append(fst: A, snd: A): A =
    monoid.combine(fst, snd)

  override def circle(x: Double, y: Double, r: Double): A =
    in(DrawCircle(x, y, r))

  override def ellipse(x: Double, y: Double, rx: Double, ry: Double): A =
    in(DrawEllipse(x, y, rx, ry))

  override def text(text: String, x: Double, y: Double): A =
    in(DrawText(text, x, y))

  override def includePath(paths: Vector[InitialPath]): A =
    in(DrawPath(paths: _*))

  override val path: FinalPath[Vector[InitialPath]] = InitialPathRenderer
}
