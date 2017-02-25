package io
package enoble
package svg2d.render

import cats.Eval
import cats.data.{State, StateT}
import cats.implicits._
import io.enoble.svg2d.ast._
import svg2d.Coords

import scala.annotation.tailrec

object AndroidRenderer {

  final case class PathState(here: Coords, indentation: Int)

  implicit class fastMonoidInterpolation(val sc: StringContext) extends AnyVal {
    def fm[A](args: Any*)(implicit fastMonoid: FastMonoid[String, A]): A = {
      import fastMonoid.monoid
      val partIt = sc.parts.iterator
      val argIt = args.iterator
      var now: A = fastMonoid.in(partIt.next())
      while (partIt.hasNext) {
        now = now |+| fastMonoid.in(String.valueOf(argIt.next())) |+| fastMonoid.in(partIt.next())
      }
      now
    }
  }

}

final case class AndroidRenderer[A](stringyMonoid: FastMonoid[String, A]) extends FinalSVG[A] {
  self =>

  implicit val implicitStringyMonoid = stringyMonoid

  import AndroidRenderer._
  import stringyMonoid._

  override val empty: A = monoid.empty

  override def append(f1: A, f2: A): A = {
    monoid.combine(f1, f2)
  }

  // TODO: WHY DO I HAVE TO DO THIS TO MY POOR NEWLINES
  override def circle(x: Double, y: Double, r: Double): A = {
    fm"c.drawCircle($x, $y, $r, p);${"\n"}"
  }

  override def ellipse(x: Double, y: Double, rx: Double, ry: Double): A = {
    val left = x - (rx / 2)
    val top = y + (ry / 2)
    val right = x + (rx / 2)
    val bottom = y - (ry / 2)
    append(
      fm"{${"\n"}    RectF bounds = new RectF($left, $top, $right, $bottom);${"\n"}",
      in("    c.drawOval(bounds, p);\n}\n")
    )
  }

  override def text(text: String, x: Double, y: Double): A = {
    outputLine(fm"c.drawText($text, $x, $y, p);")
  }

  @tailrec
  private def indent(a: A, level: Int = 0): A =
    if (level <= 0) a
    else indent(
      append(in("    "), a),
      level - 1
    )

  private def outputLine(code: A, indentation: Int = 0): A =
    append(
      indent(code, indentation),
      in("\n")
    )

  override type Paths = State[PathState, A]

  override val path: FinalPath[Paths] =
    new FinalPath[Paths] {
      override val empty: Paths = State.pure(self.empty)

      @inline
      private def outputLineS(code: Coords => A, stateChange: Coords => Coords): Paths =
        State[PathState, A] { state =>
          val newHere = stateChange(state.here)
          (state.copy(here = newHere),
            self.append(indent(code(state.here), state.indentation), in("\n"))
          )
        }

      @inline
      private def addCoords(p1: Coords, p2: Coords): Coords =
        (p1._1 + p2._1, p1._2 + p2._2)

      @inline
      private def addThirdCoords(p1: (Coords, Coords, Coords), p2: (Coords, Coords, Coords)): (Coords, Coords, Coords) =
        (p1._1, p1._2, (p1._3._1 + p2._3._1, p1._3._2 + p2._3._2))

      @inline
      private def addSecondCoords(p1: (Coords, Coords), p2: (Coords, Coords)): (Coords, Coords) =
        (p1._1, (p1._2._1 + p2._2._1, p1._2._2 + p2._2._2))

      @inline
      private def addXCoords(dx: Double, coords: Coords): Coords =
        (coords._1 + dx, coords._2)

      @inline
      private def addYCoords(dy: Double, coords: Coords): Coords =
        (coords._1, coords._2 + dy)

      @inline
      private def setXCoords(x: Double, coords: Coords): Coords =
        (x, coords._2)

      @inline
      private def setYCoords(y: Double, coords: Coords): Coords =
        (coords._1, y)

      override def append(fst: Paths, snd: Paths): Paths =
        StateT.catsDataMonadForStateT[Eval, PathState].map2(fst, snd)(self.append)

      override def closePath(): Paths =
        State[PathState, A](state => (state, outputLine(in("path.close();"), state.indentation)))

      override def moveTo(x: Double, y: Double): Paths =
        outputLineS(_ => fm"path.moveTo($x, $y);", _ => (x, y))

      override def moveToRel(dx: Double, dy: Double): Paths =
        outputLineS(_ => fm"path.rMoveTo($dx, $dy);", addCoords(_, (dx, dy)))

      override def lineTo(x: Double, y: Double): Paths =
        outputLineS(_ => fm"path.lineTo($x, $y);", _ => (x, y))

      override def lineToRel(dx: Double, dy: Double): Paths =
        outputLineS(_ => fm"path.rLineTo($dx, $dy);", addCoords(_, (dx, dy)))

      override def verticalLineTo(y: Double): Paths =
        outputLineS({ case (x, _) => fm"path.lineTo($x, $y);" }, setYCoords(y, _))

      override def verticalLineToRel(dy: Double): Paths =
        outputLineS(_ => fm"path.rLineTo(0.0, $dy);", addYCoords(dy, _))

      override def horizLineTo(x: Double): Paths =
        outputLineS({ case (_, y) => fm"path.lineTo($x, $y);" }, setXCoords(x, _))

      override def horizLineToRel(dx: Double): Paths =
        outputLineS(_ => fm"path.rLineTo($dx, 0.0);", addXCoords(dx, _))

      override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): Paths =
        outputLineS(_ => fm"path.cubicTo($x1, $y1, $x2, $y2, $x, $y);", _ => (x, y))

      override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): Paths =
        outputLineS(_ => fm"path.rCubicTo($x1, $y1, $x2, $y2, $dx, $dy);", addCoords(_, (dx, dy)))

      override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): Paths =
        State.pure(in(s"???"))

      override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): Paths =
        State.pure(in(s"???"))

      override def quad(x1: Double, y1: Double, x: Double, y: Double): Paths =
        outputLineS(_ => fm"path.quadTo($x1, $y1, $x, $y);", _ => (x, y))

      override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): Paths =
        outputLineS(_ => fm"path.rQuadTo($x1, $y1, $dx, $dy);", addCoords((dx, dy), _))

      override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): State[PathState, A] =
        State.pure(in(s"???"))

      override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): State[PathState, A] =
        State.pure(in(s"???"))

    }

  override def includePath(paths: Paths): A = {
    val result = paths.runA(PathState(here = (0, 0), indentation = 1)).value
    val intro: A =
      append(
        outputLine(in("{")),
        outputLine(in("Path path = new Path();"), indentation = 1)
      )
    val outro: A =
      outputLine(in("}"))
    append(intro, append(result, outro))
  }
}
