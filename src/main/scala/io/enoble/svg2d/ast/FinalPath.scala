package io
package enoble
package svg2d
package ast

import cats.Cartesian
import cats.functor.Invariant
import io.enoble.svg2d.ast.InitialPath._
import shapeless._

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

  def smoothQuad(x: Double, y: Double): A

  def smoothQuadRel(dx: Double, dy: Double): A

  def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): A

  def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): A

}

object FinalPath {

  def renderInitial[A](path: FinalPath[A])(ins: Seq[InitialPath]): A =
    ins.foldLeft(path.empty) { (a, ip) =>
      path.append(a, ip match {
        case ClosePath => path.closePath()
        case MoveTo(x, y) => path.moveTo(x, y)
        case MoveToRel(dx, dy) => path.moveToRel(dx, dy)
        case LineTo(x, y) => path.lineTo(x, y)
        case LineToRel(dx, dy) => path.lineToRel(dx, dy)
        case VerticalLineTo(y) => path.verticalLineTo(y)
        case VerticalLineToRel(dy) => path.verticalLineToRel(dy)
        case HorizLineTo(x) => path.horizLineTo(x)
        case HorizLineToRel(dx) => path.horizLineToRel(dx)
        case Cubic(x1, y1, x2, y2, x, y) => path.cubic(x1, y1, x2, y2, x, y)
        case CubicRel(x1, y1, x2, y2, dx, dy) => path.cubicRel(x1, y1, x2, y2, dx, dy)
        case SmoothCubic(x2, y2, x, y) => path.smoothCubic(x2, y2, x, y)
        case SmoothCubicRel(x2, y2, dx, dy) => path.smoothCubicRel(x2, y2, dx, dy)
        case Quad(x1, y1, x, y) => path.quad(x1, y1, x, y)
        case QuadRel(x1, y1, dx, dy) => path.quadRel(x1, y1, dx, dy)
        case SmoothQuad(x, y) => path.smoothQuad(x, y)
        case SmoothQuadRel(dx, dy) => path.smoothQuadRel(dx, dy)
        case Elliptic(rx, ry, rotX, largeArc, sweep, x, y) => path.elliptic(rx, ry, rotX, largeArc, sweep, x, y)
        case EllipticRel(rx, ry, rotX, largeArc, sweep, dx, dy) => path.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)
      })
    }

  def const[A](a: A): FinalPath[A] = new FinalPath[A] {
    override val empty: A = a

    override def append(fst: A, snd: A): A = a

    override def closePath(): A = a

    override def moveTo(x: Double, y: Double): A = a

    override def moveToRel(dx: Double, dy: Double): A = a

    override def lineTo(x: Double, y: Double): A = a

    override def lineToRel(dx: Double, dy: Double): A = a

    override def verticalLineTo(y: Double): A = a

    override def verticalLineToRel(dy: Double): A = a

    override def horizLineTo(x: Double): A = a

    override def horizLineToRel(dx: Double): A = a

    override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): A = a

    override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): A = a

    override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): A = a

    override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): A = a

    override def quad(x1: Double, y1: Double, x: Double, y: Double): A = a

    override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): A = a

    override def smoothQuad(x: Double, y: Double): A = a

    override def smoothQuadRel(dx: Double, dy: Double): A = a

    override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): A = a

    override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): A = a
  }

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

          override def smoothQuad(x: Double, y: Double): B =
            f(fa.smoothQuad(x, y))

          override def smoothQuadRel(dx: Double, dy: Double): B =
            f(fa.smoothQuadRel(dx, dy))

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

          override def smoothQuad(x: Double, y: Double): (A, B) =
            (fa.smoothQuad(x, y), fb.smoothQuad(x, y))

          override def smoothQuadRel(dx: Double, dy: Double): (A, B) =
            (fa.smoothQuadRel(dx, dy), fb.smoothQuadRel(dx, dy))

          override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): (A, B) =
            (fa.elliptic(rx, ry, rotX, largeArc, sweep, x, y), fb.elliptic(rx, ry, rotX, largeArc, sweep, x, y))

          override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): (A, B) =
            (fa.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy), fb.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy))
        }
    }

  def sequenceOption[A](opt: Option[FinalPath[A]]): FinalPath[Option[A]] = opt.fold[FinalPath[Option[A]]](FinalPath.const(None))((f: FinalPath[A]) => new FinalPath[Option[A]] {
    override val empty: Option[A] = Some(f.empty)

    override def append(fst: Option[A], snd: Option[A]): Option[A] =
      Some(f.append(fst.getOrElse(f.empty), snd.getOrElse(f.empty)))

    override def closePath(): Option[A] =
      Some(f.closePath())

    override def moveTo(x: Double, y: Double): Option[A] =
      Some(f.moveTo(x, y))

    override def moveToRel(dx: Double, dy: Double): Option[A] =
      Some(f.moveToRel(dx, dy))

    override def lineTo(x: Double, y: Double): Option[A] =
      Some(f.lineTo(x, y))

    override def lineToRel(dx: Double, dy: Double): Option[A] =
      Some(f.lineToRel(dx, dy))

    override def verticalLineTo(y: Double): Option[A] =
      Some(f.verticalLineTo(y))

    override def verticalLineToRel(dy: Double): Option[A] =
      Some(f.verticalLineToRel(dy))

    override def horizLineTo(x: Double): Option[A] =
      Some(f.horizLineTo(x))

    override def horizLineToRel(dx: Double): Option[A] =
      Some(f.horizLineToRel(dx))

    override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): Option[A] =
      Some(f.cubic(x1, y1, x2, y2, x, y))

    override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): Option[A] =
      Some(f.cubicRel(x1, y1, x2, y2, dx, dy))

    override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): Option[A] =
      Some(f.smoothCubic(x2, y2, x, y))

    override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): Option[A] =
      Some(f.smoothCubicRel(x2, y2, dx, dy))

    override def quad(x1: Double, y1: Double, x: Double, y: Double): Option[A] =
      Some(f.quad(x1, y1, x, y))

    override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): Option[A] =
      Some(f.quadRel(x1, y1, dx, dy))

    override def smoothQuad(x: Double, y: Double): Option[A] = Some(f.smoothQuad(x, y))

    override def smoothQuadRel(dx: Double, dy: Double): Option[A] =
      Some(f.smoothQuadRel(dx, dy))

    override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): Option[A] =
      Some(f.elliptic(rx, ry, rotX, largeArc, sweep, x, y))

    override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): Option[A] =
      Some(f.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy))
  })

  def hconsSequence[H <: HList, A](next: FinalPath[A], finalPath: FinalPath[H]): FinalPath[A :: H] = new FinalPath[A :: H] {
    override val empty: A :: H =
      next.empty :: finalPath.empty

    override def append(fst: A :: H, snd: A :: H): A :: H =
      next.append(fst.head, snd.head) :: finalPath.append(fst.tail, snd.tail)

    override def closePath(): A :: H =
      next.closePath() :: finalPath.closePath()

    override def moveTo(x: Double, y: Double): A :: H =
      next.moveTo(x, y) :: finalPath.moveTo(x, y)

    override def moveToRel(dx: Double, dy: Double): A :: H =
      next.moveToRel(dx, dy) :: finalPath.moveToRel(dx, dy)

    override def lineTo(x: Double, y: Double): A :: H =
      next.lineTo(x, y) :: finalPath.lineTo(x, y)

    override def lineToRel(dx: Double, dy: Double): A :: H =
      next.lineToRel(dx, dy) :: finalPath.lineToRel(dx, dy)

    override def verticalLineTo(y: Double): A :: H =
      next.verticalLineTo(y) :: finalPath.verticalLineTo(y)

    override def verticalLineToRel(dy: Double): A :: H =
      next.verticalLineToRel(dy) :: finalPath.verticalLineToRel(dy)

    override def horizLineTo(x: Double): A :: H =
      next.horizLineTo(x) :: finalPath.horizLineTo(x)

    override def horizLineToRel(dx: Double): A :: H =
      next.horizLineToRel(dx) :: finalPath.horizLineToRel(dx)

    override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): A :: H =
      next.cubic(x1, y1, x2, y2, x, y) :: finalPath.cubic(x1, y1, x2, y2, x, y)

    override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): A :: H =
      next.cubicRel(x1, y1, x2, y2, dx, dy) :: finalPath.cubicRel(x1, y1, x2, y2, dx, dy)

    override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): A :: H =
      next.smoothCubic(x2, y2, x, y) :: finalPath.smoothCubic(x2, y2, x, y)

    override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): A :: H =
      next.smoothCubicRel(x2, y2, dx, dy) :: finalPath.smoothCubicRel(x2, y2, dx, dy)

    override def quad(x1: Double, y1: Double, x: Double, y: Double): A :: H =
      next.quad(x1, y1, x, y) :: finalPath.quad(x1, y1, x, y)

    override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): A :: H =
      next.quadRel(x1, y1, dx, dy) :: finalPath.quadRel(x1, y1, dx, dy)

    override def smoothQuad(x: Double, y: Double): A :: H =
      next.smoothQuad(x, y) :: finalPath.smoothQuad(x, y)

    override def smoothQuadRel(dx: Double, dy: Double): A :: H =
      next.smoothQuadRel(dx, dy) :: finalPath.smoothQuadRel(dx, dy)

    override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): A :: H =
      next.elliptic(rx, ry, rotX, largeArc, sweep, x, y) :: finalPath.elliptic(rx, ry, rotX, largeArc, sweep, x, y)

    override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): A :: H =
      next.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy) :: finalPath.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)
  }

  val hnilSequence: FinalPath[HNil] = new FinalPath[HNil] {
    override val empty: HNil = HNil

    override def append(fst: HNil, snd: HNil): HNil = HNil

    override def closePath(): HNil = HNil

    override def moveTo(x: Double, y: Double): HNil = HNil

    override def moveToRel(dx: Double, dy: Double): HNil = HNil

    override def lineTo(x: Double, y: Double): HNil = HNil

    override def lineToRel(dx: Double, dy: Double): HNil = HNil

    override def verticalLineTo(y: Double): HNil = HNil

    override def verticalLineToRel(dy: Double): HNil = HNil

    override def horizLineTo(x: Double): HNil = HNil

    override def horizLineToRel(dx: Double): HNil = HNil

    override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): HNil = HNil

    override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): HNil = HNil

    override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): HNil = HNil

    override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): HNil = HNil

    override def quad(x1: Double, y1: Double, x: Double, y: Double): HNil = HNil

    override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): HNil = HNil

    override def smoothQuad(x: Double, y: Double): HNil = HNil

    override def smoothQuadRel(dx: Double, dy: Double): HNil = HNil

    override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): HNil = HNil

    override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): HNil = HNil
  }

}
