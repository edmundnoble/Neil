package io
package enoble
package svg2d
package ast

import cats.Cartesian
import cats.functor.Invariant

trait FinalPath[A] {
  val empty: A

  def append(fst: A, snd: A): A

  final def combineAll(as: A*): A =
    if (as.isEmpty) empty
    else as.reduceLeft(append)

  val closePath: A

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

  def const[A](a: A): FinalPath[A] = new FinalPath[A] {
    override val empty: A = a

    override def append(fst: A, snd: A): A = a

    override val closePath: A = a

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

          override val closePath: B =
            f(fa.closePath)

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

          override val closePath: (A, B) =
            (fa.closePath, fb.closePath)

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

    override val closePath: Option[A] =
      Some(f.closePath)

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

    override def smoothQuad(x: Double, y: Double): Option[A] =
      Some(f.smoothQuad(x, y))

    override def smoothQuadRel(dx: Double, dy: Double): Option[A] =
      Some(f.smoothQuadRel(dx, dy))

    override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): Option[A] =
      Some(f.elliptic(rx, ry, rotX, largeArc, sweep, x, y))

    override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): Option[A] =
      Some(f.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy))
  })

}

abstract class PathFun {
  def apply[A](pathCtx: FinalPath[A]): A
}

object PathFun {
  def moveTo(f: Double, s: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.moveTo(f, s)
  }
  def moveToRel(f: Double, s: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.moveToRel(f, s)
  }
  def lineTo(f: Double, s: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.moveTo(f, s)
  }
  def lineToRel(f: Double, s: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.moveToRel(f, s)
  }
  def horizLineTo(x: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.horizLineTo(x)
  }
  def horizLineToRel(dx: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.horizLineToRel(dx)
  }
  def verticalLineTo(y: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.verticalLineTo(y)
  }
  def verticalLineToRel(dy: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.verticalLineToRel(dy)
  }
  def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.cubic(x1, y1, x2, y2, x, y)
  }
  def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.cubicRel(x1, y1, x2, y2, dx, dy)
  }
  def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.smoothCubic(x2, y2, x, y)
  }
  def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.smoothCubicRel(x2, y2, dx, dy)
  }
  def quad(x1: Double, y1: Double, x: Double, y: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.quad(x1, y1, x, y)
  }
  def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.quadRel(x1, y1, dx, dy)
  }
  def smoothQuad(x: Double, y: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.smoothQuad(x, y)
  }
  def smoothQuadRel(dx: Double, dy: Double): PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.smoothQuadRel(dx, dy)
  }
  def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): PathFun =
    new PathFun {
      def apply[A](pathCtx: FinalPath[A]): A = pathCtx.elliptic(rx, ry, rotX, largeArc, sweep, x, y)
    }
  def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): PathFun =
    new PathFun {
      def apply[A](pathCtx: FinalPath[A]): A = pathCtx.ellipticRel(rx, ry, rotX, largeArc, sweep, dx, dy)
    }
  val closePath: PathFun = new PathFun {
    def apply[A](pathCtx: FinalPath[A]): A = pathCtx.closePath
  }
}

