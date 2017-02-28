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

  final case class PathState(hereX: Double, hereY: Double,
                             lastSecondCubicControlX: Double, lastSecondCubicControlY: Double,
                             lastQuadraticControlX: Double, lastQuadraticControlY: Double,
                             indentation: Int) {
    @inline def setHere(x: Double = hereX, y: Double = hereY) =
      copy(
        hereX = x, hereY = y,
        lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN,
        lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN
      )

    @inline def addToHere(dx: Double = 0, dy: Double = 0) = copy(
      hereX = hereX + dx, hereY = hereY + dy,
      lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN,
      lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN
    )
  }

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

  override def circle(x: Double, y: Double, r: Double): A =
    outputLine(fm"c.drawCircle($x, $y, $r, p);")

  override def ellipse(x: Double, y: Double, rx: Double, ry: Double): A = {
    val left = x - (rx / 2)
    val top = y + (ry / 2)
    val right = x + (rx / 2)
    val bottom = y - (ry / 2)
    outputLine(fm"c.drawOval(new RectF($left, $top, $right, $bottom), p);")
  }

  override def rect(x: Double, y: Double, w: Double, h: Double): A =
    outputLine(fm"c.drawRect($x, ${y + h}, ${x + w}, $y, p);")

  // TODO: WHY DO I HAVE TO DO THIS TO MY POOR ESCAPEES
  override def text(text: String, x: Double, y: Double): A =
    outputLine(fm"c.drawText(${"\""}$text${"\""}, $x, $y, p);")

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
      private def outputLineS(code: PathState => A, stateChange: PathState => PathState): Paths =
        State { state =>
          val newState = stateChange(state)
          (newState,
            indent(code(newState), level = newState.indentation) |+| in("\n")
          )
        }

      override def append(fst: Paths, snd: Paths): Paths =
        StateT.catsDataMonadForStateT[Eval, PathState].map2(fst, snd)(self.append)

      override def closePath(): Paths =
        State[PathState, A](state => (state, outputLine(in("path.close();"), state.indentation)))

      override def moveTo(x: Double, y: Double): Paths =
        outputLineS(_ => fm"path.moveTo($x, $y);", _.setHere(x, y))

      override def moveToRel(dx: Double, dy: Double): Paths =
        outputLineS(_ => fm"path.rMoveTo($dx, $dy);", _.addToHere(dx, dy))

      override def lineTo(x: Double, y: Double): Paths =
        outputLineS(_ => fm"path.lineTo($x, $y);", _.setHere(x, y))

      override def lineToRel(dx: Double, dy: Double): Paths =
        outputLineS(_ => fm"path.rLineTo($dx, $dy);", _.addToHere(dx, dy))

      override def verticalLineTo(y: Double): Paths =
        outputLineS(s => fm"path.lineTo(${s.hereX}, ${s.hereY});", _.setHere(y = y))

      override def verticalLineToRel(dy: Double): Paths =
        outputLineS(_ => fm"path.rLineTo(0.0, $dy);", _.addToHere(dy = dy))

      override def horizLineTo(x: Double): Paths =
        outputLineS(s => fm"path.lineTo(${s.hereX}, ${s.hereY});", _.setHere(x = x))

      override def horizLineToRel(dx: Double): Paths =
        outputLineS(_ => fm"path.rLineTo($dx, 0.0);", _.addToHere(dx = dx))

      override def cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double): Paths =
        outputLineS(
          _ => fm"path.cubicTo($x1, $y1, $x2, $y2, $x, $y);",
          _.copy(
            lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN,
            lastSecondCubicControlX = x2, lastSecondCubicControlY = y2,
            hereX = x, hereY = y
          )
        )

      override def cubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double): Paths =
        outputLineS(_ => fm"path.rCubicTo($x1, $y1, $x2, $y2, $dx, $dy);", { state =>
          val newX = state.hereX + dx
          val newY = state.hereY + dy
          val newState =
            state.copy(
              lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN,
              lastSecondCubicControlX = x2 + state.hereX, lastSecondCubicControlY = y2 + state.hereY,
              hereX = newX, hereY = newY
            )
          newState
        })

      override def smoothCubic(x2: Double, y2: Double, x: Double, y: Double): Paths =
        State { state =>
          val newState =
            state.copy(
              lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN,
              lastSecondCubicControlX = x2, lastSecondCubicControlY = y2,
              hereX = x, hereY = y
            )
          (newState,
            outputLine(
              fm"path.cubicTo(${state.hereX * 2 - state.lastSecondCubicControlX}, ${state.hereY * 2 - state.lastSecondCubicControlY}, $x2, $y2, $x, $y);",
              indentation = state.indentation
            )
          )
        }

      override def smoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double): Paths =
        State { state =>
          val newControlX = state.hereX - state.lastSecondCubicControlX
          val newControlY = state.hereY - state.lastSecondCubicControlY
          val newState =
            state.copy(
              lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN,
              lastSecondCubicControlX = x2 + state.hereX, lastSecondCubicControlY = y2 + state.hereY,
              hereX = state.hereX + dx, hereY = state.hereY + dy
            )
          (newState,
            outputLine(
              fm"path.rCubicTo($newControlX, $newControlY, $x2, $y2, $dx, $dy);",
              indentation = state.indentation
            )
          )
        }

      override def quad(x1: Double, y1: Double, x: Double, y: Double): Paths =
        outputLineS(
          _ => fm"path.quadTo($x1, $y1, $x, $y);",
          _.copy(
            hereX = x, hereY = y,
            lastQuadraticControlX = x1, lastQuadraticControlY = y1,
            lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN
          )
        )

      override def quadRel(x1: Double, y1: Double, dx: Double, dy: Double): Paths = State(state =>
        (state.copy(
          hereX = state.hereX + dx, hereY = state.hereY + dy,
          lastQuadraticControlX = state.hereX + x1, lastQuadraticControlY = state.hereY + y1,
          lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN
        ), outputLine(
          fm"path.rQuadTo($x1, $y1, $dx, $dy);",
          indentation = state.indentation
        ))
      )

      override def smoothQuad(x: Double, y: Double): Paths = State { state =>
        val newControlX = state.hereX * 2 - state.lastQuadraticControlX
        val newControlY = state.hereY * 2 - state.lastQuadraticControlY
        (state.copy(
          hereX = x, hereY = y,
          lastQuadraticControlX = newControlX, lastQuadraticControlY = newControlY,
          lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN
        ), outputLine(
          fm"path.quadTo($newControlX, $newControlY, $x, $y);",
          indentation = state.indentation
        ))
      }

      override def smoothQuadRel(dx: Double, dy: Double): Paths = State { state =>
        val newControlX = state.hereX - state.lastQuadraticControlX
        val newControlY = state.hereY - state.lastQuadraticControlY
        (state.copy(
          hereX = state.hereX + dx, hereY = state.hereY + dy,
          lastQuadraticControlX = newControlX, lastQuadraticControlY = newControlY,
          lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN
        ), outputLine(
          fm"path.rQuadTo($newControlX, $newControlY, $dx, $dy);", indentation = state.indentation
        ))
      }

      override def elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double): State[PathState, A] =
        State.pure(outputLine(fm"???"))

      override def ellipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double): State[PathState, A] =
        State.pure(outputLine(fm"???"))

    }

  override def includePath(paths: Paths): A = {
    val result = paths.runA(
      PathState(
        hereX = 0, hereY = 0,
        lastSecondCubicControlX = Double.NaN, lastSecondCubicControlY = Double.NaN,
        lastQuadraticControlX = Double.NaN, lastQuadraticControlY = Double.NaN,
        indentation = 1
      )
    ).value
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
