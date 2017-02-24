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

  def circle(x: Double, y: Double, r: Double): A

  def ellipse(x: Double, y: Double, rx: Double, ry: Double): A

  def text(text: String, x: Double, y: Double): A

  def includePath(paths: Paths): A

  def renderInitial(ins: Seq[InitialSVG]): A = {
    def render(initia: InitialSVG): A = initia match {
      case DrawCircle(x, y, r) => circle(x, y, r)
      case DrawEllipse(x, y, rx, ry) => ellipse(x, y, rx, ry)
      case DrawText(t, x, y) => text(t, x, y)
      case DrawPath(paths) => includePath(path.renderInitial(paths))
    }
    if (ins.isEmpty) empty
    else ins.tail.foldLeft(render(ins.head))((b, i) => append(b, render(i)))
  }
}

object FinalSVG {
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
      }
  }
}
