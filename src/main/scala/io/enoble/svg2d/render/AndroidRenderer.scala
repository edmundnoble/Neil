package io.enoble.svg2d.render

import io.enoble.svg2d.ast._
import io.enoble.svg2d.data.AndroidCode

import scalaz.Monoid

object AndroidRenderer extends FinalSVG[AndroidCode] {
  override type Paths = Vector[InitialPath]
  override implicit def monoid[AA >: AndroidCode]: Monoid[AA] = AndroidCode.androidMonoid.asInstanceOf[Monoid[AA]]
  override def circle(x: Double, y: Double, r: Double) =
    AndroidCode(s"c.drawCircle($x, $y, $r, p);")
  override def ellipse(x: Double, y: Double, rx: Double, ry: Double): AndroidCode = {
    val left = x - (rx / 2)
    val top = y + (ry / 2)
    val right = x + (rx / 2)
    val bottom = y - (ry / 2)
    AndroidCode(s"RectF bounds = new RectF($left, $top, $right, $bottom)", "c.drawOval(bounds, p)")
  }
  override def text(text: String, x: Double, y: Double): AndroidCode = {
    AndroidCode(s"c.drawText($text, $x, $y, p)")
  }

  type Coords = (Double, Double)
  override def path(paths: Paths): AndroidCode = {

    def trackCoords(x: Double, y: Double): String =
      s"""
      x = $x;
      x = $y;
     """

    def foldCmd[T](s: Vector[T], f: T => String): Vector[String] = s.foldLeft(Vector.empty[String])((a, b) => a :+ f(b))

    new AndroidCode(Vector("{\n" +
      "Path path = new Path()") ++ {
      var xNow = 0.0
      var yNow = 0.0
      def changeCoords(dx: Double, dy: Double) = {
        xNow += dx
        yNow += dy
      }
      def setCoords(x: Double, y: Double) = {
        xNow = x
        yNow = y
      }
      paths.foldLeft(Vector.empty[String]) { (sofar, cmd) =>
        sofar ++ (cmd match {
          case ClosePath() => Vector("path.close()")
          case MoveTo(coords) => foldCmd[Coords](coords, { case (x, y) => setCoords(x, y); s"path.moveTo($x, $y)" })
          case MoveToRel(coords) => foldCmd[Coords](coords, { case (x, y) => changeCoords(x, y); s"path.rMoveTo($x, $y)" })
          case LineTo(coords) => foldCmd[Coords](coords, { case (x, y) => setCoords(x, y); s"path.lineTo($x, $y)" })
          case LineToRel(coords) => foldCmd[Coords](coords, { case (x, y) => changeCoords(x, y); s"path.rLineTo($x, $y)" })
          case VerticalLineTo(coords) => foldCmd[Double](coords, y => {
            yNow = y
            s"path.lineTo($xNow, $y)"
          })
          case VerticalLineToRel(coords) => foldCmd[Double](coords, y => {
            yNow += y
            s"path.rLineTo(0, $y)"
          })
          case HorizLineTo(coords) => foldCmd[Double](coords, x => {
            xNow = x
            s"path.lineTo($x, $yNow)"
          })
          case HorizLineToRel(coords) => foldCmd[Double](coords, x => {
            xNow += x
            s"path.rLineTo($x, 0)"
          })
          case Cubic(args) => foldCmd[(Coords, Coords, Coords)](args, { case ((x1, y1), (x2, y2), (x, y)) =>
            setCoords(x, y); s"path.cubicTo($x1, $y1, $x2, $y2, $x, $y)"
          })
          case CubicRel(args) => foldCmd[(Coords, Coords, Coords)](args, { case ((x1, y1), (x2, y2), (x, y)) =>
            changeCoords(x, y); s"path.rCubicTo($x1, $y1, $x2, $y2, $x, $y)"
          })
          case SmoothCubic(args) => ??? // TODO
          case SmoothCubicRel(args) => ??? // TODO
          case Quad(args) => foldCmd[(Coords, Coords)](args, { case ((x1, y1), (x, y)) =>
            setCoords(x, y); s"path.quadTo($x1, $y1, $x, $y)"
          })
          case QuadRel(args) => foldCmd[(Coords, Coords)](args, { case ((x1, y1), (x, y)) =>
            changeCoords(x, y); s"path.rQuadTo($x1, $y1, $x, $y)"
          })
          case Elliptic(_) => ??? // TODO
          case EllipticRel(_) => ??? // TODO
        })
      }
    } :+ "}"
    )

  }

  override val path = InitialPathRenderer
}
