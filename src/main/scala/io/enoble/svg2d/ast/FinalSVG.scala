package io
package enoble
package svg2d
package ast

import cats.Cartesian
import cats.functor.Invariant
import io.enoble.svg2d.ast.InitialSVG.{DrawCircle, DrawEllipse, DrawPath, DrawText}
import shapeless._

trait FinalSVG[A] {
  type Paths

  val path: FinalPath[Paths]

  val empty: A

  def append(fst: A, snd: A): A

  def circle(x: Double, y: Double, r: Double): A

  def ellipse(x: Double, y: Double, rx: Double, ry: Double): A

  def text(text: String, x: Double, y: Double): A

  def includePath(paths: Paths): A

}

object FinalSVG {
  final def renderInitial[A](finalSVG: FinalSVG[A])(ins: Seq[InitialSVG]): A = {
    def render(initia: InitialSVG): A = initia match {
      case DrawCircle(x, y, r) => finalSVG.circle(x, y, r)
      case DrawEllipse(x, y, rx, ry) => finalSVG.ellipse(x, y, rx, ry)
      case DrawText(t, x, y) => finalSVG.text(t, x, y)
      case paths: DrawPath => finalSVG.includePath(FinalPath.renderInitial(finalSVG.path)(paths.commands))
    }
    if (ins.isEmpty) finalSVG.empty
    else ins.tail.foldLeft(render(ins.head))((b, i) => finalSVG.append(b, render(i)))
  }

  def const[A](a: A): FinalSVG[A] = new FinalSVG[A] {
    override type Paths = A
    override val path: FinalPath[Paths] = FinalPath.const(a)
    override val empty: A = a

    override def append(fst: A, snd: A): A = a

    override def circle(x: Double, y: Double, r: Double): A = a

    override def ellipse(x: Double, y: Double, rx: Double, ry: Double): A = a

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

  def hconsSequence[H <: HList, HP <: HList, A](next: FinalSVG[A], finalPath: FinalSVG[H] { type Paths = HP }): FinalSVG[A :: H] { type Paths = next.Paths :: finalPath.Paths } = new FinalSVG[A :: H] {
    override val empty: A :: H =
      next.empty :: finalPath.empty

    override def append(fst: A :: H, snd: A :: H): A :: H =
      next.append(fst.head, snd.head) :: finalPath.append(fst.tail, snd.tail)

    override type Paths = next.Paths :: finalPath.Paths

    override val path: FinalPath[Paths] =
      FinalPath.hconsSequence(next.path, finalPath.path)

    override def circle(x: Double, y: Double, r: Double): A :: H =
      next.circle(x, y, r) :: finalPath.circle(x, y, r)

    override def ellipse(x: Double, y: Double, rx: Double, ry: Double): A :: H =
      next.ellipse(x, y, rx, ry) :: finalPath.ellipse(x, y, rx, ry)

    override def text(text: String, x: Double, y: Double): A :: H =
      next.text(text, x, y) :: finalPath.text(text, x, y)

    override def includePath(paths: Paths): A :: H =
      next.includePath(paths.head) :: finalPath.includePath(paths.tail)
  }

  val hnilSequence: FinalSVG[HNil] { type Paths = HNil } = new FinalSVG[HNil] {
    override val empty: HNil = HNil

    override def append(fst: HNil, snd: HNil): HNil = HNil

    override type Paths = HNil

    override val path: FinalPath[HNil] = FinalPath.hnilSequence

    override def circle(x: Double, y: Double, r: Double): HNil = HNil

    override def ellipse(x: Double, y: Double, rx: Double, ry: Double): HNil = HNil

    override def text(text: String, x: Double, y: Double): HNil = HNil

    override def includePath(paths: HNil): HNil = HNil
  }

  def sequenceOption[A](opt: Option[FinalSVG[A]]): FinalSVG[Option[A]] =
    opt.fold[FinalSVG[Option[A]]](FinalSVG.const(None))((f: FinalSVG[A]) => new FinalSVG[Option[A]] {
      override type Paths = f.Paths
      override val path: FinalPath[f.Paths] = f.path
      override val empty: Option[A] = Some(f.empty)

      override def append(fst: Option[A], snd: Option[A]): Option[A] =
        Some(f.append(fst.getOrElse(f.empty), snd.getOrElse(f.empty)))

      override def circle(x: Double, y: Double, r: Double): Option[A] = Some(f.circle(x, y, r))

      override def ellipse(x: Double, y: Double, rx: Double, ry: Double): Option[A] = Some(f.ellipse(x, y, rx, ry))

      override def text(text: String, x: Double, y: Double): Option[A] = Some(f.text(text, x, y))

      override def includePath(paths: f.Paths): Option[A] = Some(f.includePath(paths))
    })

}
