package io
package enoble
package svg2d
package ast

import cats.Cartesian
import cats.functor.Invariant

trait FinalSVG[A] {
  type Paths

  val path: FinalPath[Paths]

  val empty: A

  def append(fst: A, snd: A): A

  final def combineAll(as: A*): A =
    if (as.isEmpty) empty
    else as.reduceLeft((x, y) => append(y, x))

  def circle(x: Double, y: Double, r: Double): A

  def ellipse(x: Double, y: Double, rx: Double, ry: Double): A

  def rect(x: Double, y: Double, w: Double, h: Double): A

  def text(text: String, x: Double, y: Double): A

  def includePath(paths: Paths): A

}

object FinalSVG {
  def const[A](a: A): FinalSVG[A] = new FinalSVG[A] {
    override type Paths = A
    override val path: FinalPath[Paths] = FinalPath.const(a)
    override val empty: A = a

    override def append(fst: A, snd: A): A = a

    override def circle(x: Double, y: Double, r: Double): A = a

    override def ellipse(x: Double, y: Double, rx: Double, ry: Double): A = a

    override def rect(x: Double, y: Double, w: Double, h: Double): A = a

    override def text(text: String, x: Double, y: Double): A = a

    override def includePath(paths: Paths): A = a
  }

  implicit def instance: Invariant[FinalSVG] with Cartesian[FinalSVG] = new Invariant[FinalSVG] with Cartesian[FinalSVG] {
    override def imap[A, B](fa: FinalSVG[A])(f: (A) => B)(g: (B) => A): FinalSVG[B] =
      new FinalSVG[B] {
        override type Paths =
          fa.Paths

        override val path: FinalPath[Paths] =
          fa.path

        override val empty: B =
          f(fa.empty)

        override def append(fst: B, snd: B): B =
          f(fa.append(g(fst), g(snd)))

        override def circle(x: Double, y: Double, r: Double): B =
          f(fa.circle(x, y, r))

        override def ellipse(x: Double, y: Double, rx: Double, ry: Double): B =
          f(fa.ellipse(x, y, rx, ry))

        override def text(text: String, x: Double, y: Double): B =
          f(fa.text(text, x, y))

        override def includePath(paths: Paths): B =
          f(fa.includePath(paths))

        override def rect(x: Double, y: Double, w: Double, h: Double): B =
          f(fa.rect(x, y, w, h))
      }

    override def product[A, B](fa: FinalSVG[A], fb: FinalSVG[B]): FinalSVG[(A, B)] =
      new FinalSVG[(A, B)] {
        override type Paths =
          (fa.Paths, fb.Paths)

        override def includePath(paths: (fa.Paths, fb.Paths)): (A, B) =
          (fa.includePath(paths._1),
            fb.includePath(paths._2))

        override def ellipse(x: Double, y: Double, rx: Double, ry: Double): (A, B) =
          (fa.ellipse(x, y, rx, ry),
            fb.ellipse(x, y, rx, ry))

        override def text(text: String, x: Double, y: Double): (A, B) =
          (fa.text(text, x, y),
            fb.text(text, x, y))

        override def circle(x: Double, y: Double, r: Double): (A, B) =
          (fa.circle(x, y, r),
            fb.circle(x, y, r))

        override def append(fst: (A, B), snd: (A, B)): (A, B) =
          (fa.append(fst._1, snd._1),
            fb.append(fst._2, snd._2))

        override val empty: (A, B) =
          (fa.empty,
            fb.empty)

        override val path: FinalPath[Paths] =
          FinalPath.instance.product(fa.path, fb.path)

        override def rect(x: Double, y: Double, w: Double, h: Double): (A, B) =
          (fa.rect(x, y, w, h),
            fb.rect(x, y, w, h))
      }
  }

  def sequenceOption[A](opt: Option[FinalSVG[A]]): FinalSVG[Option[A]] =
    opt.fold[FinalSVG[Option[A]]](FinalSVG.const(None))(f => instance.imap[A, Option[A]](f)(Some(_))(_.getOrElse(f.empty)))

}
