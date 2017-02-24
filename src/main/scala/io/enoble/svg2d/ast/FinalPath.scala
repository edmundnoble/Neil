package io
package enoble
package svg2d
package ast

import cats.Cartesian
import cats.functor.Invariant

trait FinalPath[A] {
  val empty: A

  def append(fst: A, snd: A): A

  def closePath(): A

  def moveTo(x: Double, y: Double): A

  def moveToRel(dx: Double, dy: Double): A

  def lineTo(x: Double, y: Double): A

  def lineToRel(dx: Double, dy: Double): A

  def verticalLineTo(y: Double): A

  def verticalLineToRel(dy: Double): A

  def horizLineTo(x: Double): A

  def horizLineToRel(dx: Double): A

  def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): A

  def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): A

  def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): A

  def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): A

  def quad(x1: Double, y1: Double, x: Double, y: Double): A

  def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): A

  def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): A

  def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): A

  def renderInitial(ins: Seq[InitialPath]): A =
    ins.foldLeft(empty) { (a, ip) =>
      append(a, ip match {
        case ClosePath() => closePath()
        case MoveTo(x, y) => moveTo(x, y)
        case MoveToRel(dx, dy) => moveToRel(dx, dy)
        case LineTo(x, y) => lineTo(x, y)
        case LineToRel(dx, dy) => lineToRel(dx, dy)
        case VerticalLineTo(y) => verticalLineTo(y)
        case VerticalLineToRel(dy) => verticalLineToRel(dy)
        case HorizLineTo(x) => horizLineTo(x)
        case HorizLineToRel(dx) => horizLineToRel(dx)
        case Cubic(x1, y1, x2, y2, x, y) => cubic(x1, y1, x2, y2, x, y)
        case CubicRel(x1, y1, x2, y2, dx, dy) => cubicRel(x1, y1, x2, y2, dx, dy)
        case SmoothCubic(x2, y2, x, y) => smoothCubicRel(x2, y2, x, y)
        case SmoothCubicRel(x2, y2, dx, dy) => smoothCubicRel(x2, y2, dx, dy)
        case Quad(x1, y1, x, y) => quad(x1, y1, x, y)
        case QuadRel(x1, y1, dx, dy) => quadRel(x1, y1, dx, dy)
        case Elliptic(rx, ry, rotX, largeArc, sweep, x, y) => elliptic(rx, ry, rotX, largeArc, sweep, x, y)
        case EllipticRel(rx, ry, rotX, largeArc, sweep, dx, dy) => ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)
      })
    }

}

object FinalPath {
  implicit def instance: Invariant[FinalPath] with Cartesian[FinalPath] =
    new Invariant[FinalPath] with Cartesian[FinalPath] {
      override def imap[A, B](fa: FinalPath[A])(f: (A) => B)(g: (B) => A): FinalPath[B] =
        new FinalPath[B] {
          override val empty: B = f(fa.empty)

          override def append(fst: B, snd: B): B =
            f(fa.append(g(fst), g(snd)))

          override def closePath(): B =
            f(fa.closePath())

          override def moveTo(points: Vector[(Double, Double)]): B =
            f(fa.moveTo(points))

          override def moveToRel(points: Vector[(Double, Double)]): B =
            f(fa.moveToRel(points))

          override def lineTo(points: Vector[(Double, Double)]): B =
            f(fa.lineTo(points))

          override def lineToRel(points: Vector[(Double, Double)]): B =
            f(fa.lineToRel(points))

          override def verticalLineTo(y: Vector[Double]): B =
            f(fa.verticalLineTo(y))

          override def verticalLineToRel(y: Vector[Double]): B =
            f(fa.verticalLineToRel(y))

          override def horizLineTo(y: Vector[Double]): B =
            f(fa.horizLineTo(y))

          override def horizLineToRel(y: Vector[Double]): B =
            f(fa.horizLineToRel(y))

          override def cubic(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): B =
            f(fa.cubic(params))

          override def cubicRel(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): B =
            f(fa.cubicRel(params))

          override def smoothCubic(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): B =
            f(fa.smoothCubic(params))

          override def smoothCubicRel(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): B =
            f(fa.smoothCubicRel(params))

          override def quad(params: Vector[((Double, Double), (Double, Double))]): B =
            f(fa.quad(params))

          override def quadRel(params: Vector[((Double, Double), (Double, Double))]): B =
            f(fa.quadRel(params))

          override def elliptic(params: Vector[EllipticParam]): B =
            f(fa.elliptic(params))

          override def ellipticRel(params: Vector[EllipticParam]): B =
            f(fa.ellipticRel(params))

        }

      override def product[A, B](fa: FinalPath[A], fb: FinalPath[B]): FinalPath[(A, B)] =
        new FinalPath[(A, B)] {
          override val empty: (A, B) =
            (fa.empty, fb.empty)

          override def append(fst: (A, B), snd: (A, B)): (A, B) =
            (fa.append(fst._1, snd._1), fb.append(fst._2, snd._2))

          override def closePath(): (A, B) =
            (fa.closePath(), fb.closePath())

          override def moveTo(points: Vector[(Double, Double)]): (A, B) =
            (fa.moveTo(points), fb.moveTo(points))

          override def moveToRel(points: Vector[(Double, Double)]): (A, B) =
            (fa.moveToRel(points), fb.moveToRel(points))

          override def lineTo(points: Vector[(Double, Double)]): (A, B) =
            (fa.lineTo(points), fb.lineTo(points))

          override def lineToRel(points: Vector[(Double, Double)]): (A, B) =
            (fa.lineToRel(points), fb.lineToRel(points))

          override def verticalLineTo(y: Vector[Double]): (A, B) =
            (fa.verticalLineTo(y), fb.verticalLineTo(y))

          override def verticalLineToRel(y: Vector[Double]): (A, B) =
            (fa.verticalLineToRel(y), fb.verticalLineToRel(y))

          override def horizLineTo(y: Vector[Double]): (A, B) =
            (fa.horizLineTo(y), fb.horizLineTo(y))

          override def horizLineToRel(y: Vector[Double]): (A, B) =
            (fa.horizLineToRel(y), fb.horizLineToRel(y))

          override def cubic(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): (A, B) =
            (fa.cubic(params), fb.cubic(params))

          override def cubicRel(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): (A, B) =
            (fa.cubicRel(params), fb.cubicRel(params))

          override def smoothCubic(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): (A, B) =
            (fa.smoothCubic(params), fb.smoothCubic(params))

          override def smoothCubicRel(params: Vector[((Double, Double), (Double, Double), (Double, Double))]): (A, B) =
            (fa.smoothCubicRel(params), fb.smoothCubicRel(params))

          override def quad(params: Vector[((Double, Double), (Double, Double))]): (A, B) =
            (fa.quad(params), fb.quad(params))

          override def quadRel(params: Vector[((Double, Double), (Double, Double))]): (A, B) =
            (fa.quadRel(params), fb.quadRel(params))

          override def elliptic(params: Vector[EllipticParam]): (A, B) =
            (fa.elliptic(params), fb.elliptic(params))

          override def ellipticRel(params: Vector[EllipticParam]): (A, B) =
            (fa.ellipticRel(params), fb.ellipticRel(params))
        }
    }

}
