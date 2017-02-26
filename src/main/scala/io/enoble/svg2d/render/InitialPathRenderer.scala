package io
package enoble
package svg2d
package render

import io.enoble.svg2d.ast._

object InitialPathRenderer extends FinalPath[Vector[InitialPath]] {
  override val empty = Vector.empty[InitialPath]

  override def append(fst: Vector[InitialPath], snd: Vector[InitialPath]): Vector[InitialPath] = fst ++ snd

  override def closePath() = Vector(ClosePath)

  override def moveTo(x: Double, y: Double): Vector[InitialPath] =
    Vector(MoveTo(x, y))

  override def moveToRel(dx: Double, dy: Double): Vector[InitialPath] =
    Vector(MoveToRel(dx, dy))

  override def lineTo(x: Double, y: Double): Vector[InitialPath] =
    Vector(LineTo(x, y))

  override def lineToRel(dx: Double, dy: Double): Vector[InitialPath] =
    Vector(LineToRel(dx, dy))

  override def verticalLineTo(y: Double): Vector[InitialPath] =
    Vector(VerticalLineTo(y))

  override def verticalLineToRel(dy: Double): Vector[InitialPath] =
    Vector(VerticalLineToRel(dy))

  override def horizLineTo(x: Double): Vector[InitialPath] =
    Vector(HorizLineTo(x))

  override def horizLineToRel(dx: Double): Vector[InitialPath] =
    Vector(HorizLineToRel(dx))

  override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): Vector[InitialPath] =
    Vector(Cubic(x1, y1, x2, y2, x, y))

  override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): Vector[InitialPath] =
    Vector(CubicRel(x1, y1, x2, y2, dx, dy))

  override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): Vector[InitialPath] =
    Vector(SmoothCubic(x2, y2, x, y))

  override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): Vector[InitialPath] =
    Vector(SmoothCubicRel(x2, y2, dx, dy))

  override def quad(x1: Double, y1: Double, x: Double, y: Double): Vector[InitialPath] =
    Vector(Quad(x1, y1, x, y))

  override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): Vector[InitialPath] =
    Vector(QuadRel(x1, y1, dx, dy))

  override def smoothQuad(x: Double, y: Double): Vector[InitialPath] =
    Vector(SmoothQuad(x, y))

  override def smoothQuadRel(dx: Double, dy: Double): Vector[InitialPath] =
    Vector(SmoothQuadRel(dx, dy))

  override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): Vector[InitialPath] =
    Vector(Elliptic(rx, ry, rotX, largeArc, sweep, x, y))

  override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): Vector[InitialPath] =
    Vector(EllipticRel(rx, ry, rotX, largeArc, sweep, dx, dy))
}
