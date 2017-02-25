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
        case ClosePath => closePath()
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

          override def moveTo(x: Double, y: Double): B =
            f(fa.moveTo(x, y))

          override def moveToRel(dx: Double, dy: Double): B =
            f(fa.moveToRel(dx, dy))

          override def lineTo(x: Double, y: Double): B =
            f(fa.lineTo(x, y))

          override def lineToRel(dx: Double, dy: Double): B =
            f(fa.lineToRel(dx, dy))

          override def verticalLineTo(y: Double): B =
            f(fa.verticalLineTo(y))

          override def verticalLineToRel(dy: Double): B =
            f(fa.verticalLineToRel(dy))

          override def horizLineTo(x: Double): B =
            f(fa.horizLineTo(x))

          override def horizLineToRel(dx: Double): B =
            f(fa.horizLineToRel(dx))

          override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): B =
            f(fa.cubic(x1, y1, x2, y2, x, y))

          override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): B =
            f(fa.cubicRel(x1, y1, x2, y2, dx, dy))

          override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): B =
            f(fa.smoothCubic(x2, y2, x, y))

          override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): B =
            f(fa.smoothCubicRel(x2, y2, dx, dy))

          override def quad(x1: Double, y1: Double, x: Double, y: Double): B =
            f(fa.quad(x1, y1, x, y))

          override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): B =
            f(fa.quadRel(x1, y1, dx, dy))

          override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): B =
            f(fa.elliptic(rx, ry, rotX, largeArc, sweep, x, y))

          override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): B =
            f(fa.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy))
        }

      override def product[A, B](fa: FinalPath[A], fb: FinalPath[B]): FinalPath[(A, B)] =
        new FinalPath[(A, B)] {
          override val empty: (A, B) =
            (fa.empty, fb.empty)

          override def append(fst: (A, B), snd: (A, B)): (A, B) =
            (fa.append(fst._1, snd._1), fb.append(fst._2, snd._2))

          override def closePath(): (A, B) =
            (fa.closePath(), fb.closePath())

          override def moveTo(x: Double, y: Double): (A, B) =
            (fa.moveTo(x, y), fb.moveTo(x, y))

          override def moveToRel(dx: Double, dy: Double): (A, B) =
            (fa.moveToRel(dx, dy), fb.moveToRel(dx, dy))

          override def lineTo(x: Double, y: Double): (A, B) =
            (fa.lineTo(x, y), fb.lineTo(x, y))

          override def lineToRel(dx: Double, dy: Double): (A, B) =
            (fa.lineToRel(dx, dy), fb.lineToRel(dx, dy))

          override def verticalLineTo(y: Double): (A, B) =
            (fa.verticalLineTo(y), fb.verticalLineTo(y))

          override def verticalLineToRel(dy: Double): (A, B) =
            (fa.verticalLineToRel(dy), fb.verticalLineToRel(dy))

          override def horizLineTo(x: Double): (A, B) =
            (fa.horizLineTo(x), fb.horizLineTo(x))

          override def horizLineToRel(dx: Double): (A, B) =
            (fa.horizLineToRel(dx), fb.horizLineToRel(dx))

          override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): (A, B) =
            (fa.cubic(x1, y1, x2, y2, x, y), fb.cubic(x1, y1, x2, y2, x, y))

          override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): (A, B) =
            (fa.cubicRel(x1, y1, x2, y2, dx, dy), fb.cubicRel(x1, y1, x2, y2, dx, dy))

          override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): (A, B) =
            (fa.smoothCubic(x2, y2, x, y), fb.smoothCubic(x2, y2, x, y))

          override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): (A, B) =
            (fa.smoothCubicRel(x2, y2, dx, dy), fb.smoothCubicRel(x2, y2, dx, dy))

          override def quad(x1: Double, y1: Double, x: Double, y: Double): (A, B) =
            (fa.quad(x1, y1, x, y), fb.quad(x1, x1, x, y))

          override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): (A, B) =
            (fa.quadRel(x1, x1, dx, dy), fb.quadRel(x1, x1, dx, dy))

          override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): (A, B) =
            (fa.elliptic(rx, ry, rotX, largeArc, sweep, x, y), fb.elliptic(rx, ry, rotX, largeArc, sweep, x, y))

          override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): (A, B) =
            (fa.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy), fb.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy))
        }
    }

}
