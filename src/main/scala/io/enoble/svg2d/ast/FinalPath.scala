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

  def moveTo(points: Vector[Coords]): A

  def moveToRel(points: Vector[Coords]): A

  def lineTo(points: Vector[Coords]): A

  def lineToRel(points: Vector[Coords]): A

  def verticalLineTo(y: Vector[Double]): A

  def verticalLineToRel(y: Vector[Double]): A

  def horizLineTo(y: Vector[Double]): A

  def horizLineToRel(y: Vector[Double]): A

  def cubic(params: Vector[(Coords, Coords, Coords)]): A

  def cubicRel(params: Vector[(Coords, Coords, Coords)]): A

  def smoothCubic(params: Vector[(Coords, Coords, Coords)]): A

  def smoothCubicRel(params: Vector[(Coords, Coords, Coords)]): A

  def quad(params: Vector[(Coords, Coords)]): A

  def quadRel(params: Vector[(Coords, Coords)]): A

  def elliptic(params: Vector[EllipticParam]): A

  def ellipticRel(params: Vector[EllipticParam]): A

  def renderInitial(ins: Seq[InitialPath]): A =
    ins.foldLeft(empty) { (a, ip) =>
      append(a, ip match {
        case ClosePath() => closePath()
        case MoveTo(points) => moveTo(points)
        case MoveToRel(points) => moveToRel(points)
        case LineTo(points) => lineTo(points)
        case LineToRel(points) => lineToRel(points)
        case VerticalLineTo(y) => verticalLineTo(y)
        case VerticalLineToRel(y) => verticalLineToRel(y)
        case HorizLineTo(x) => horizLineTo(x)
        case HorizLineToRel(x) => horizLineToRel(x)
        case Cubic(params) => cubic(params)
        case CubicRel(params) => cubicRel(params)
        case SmoothCubic(params) => smoothCubicRel(params)
        case SmoothCubicRel(params) => smoothCubicRel(params)
        case Quad(params) => quad(params)
        case QuadRel(params) => quadRel(params)
        case Elliptic(params) => elliptic(params)
        case EllipticRel(params) => ellipticRel(params)
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
