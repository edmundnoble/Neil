package io
package enoble
package svg2d
package render

import io.enoble.svg2d.ast._
import io.enoble.svg2d.data.AndroidCode

import scalaz._
import Scalaz._

object AndroidRenderer extends FinalSVG[AndroidCode] {
  override val empty: AndroidCode = AndroidCode(Vector.empty)

  override def append(f1: AndroidCode, f2: AndroidCode): AndroidCode =
    AndroidCode(f1.ap ++ f2.ap)

  override def circle(x: Double, y: Double, r: Double) =
    AndroidCode(_ ++= s"c.drawCircle($x, $y, $r, p);\n")

  override def ellipse(x: Double, y: Double, rx: Double, ry: Double): AndroidCode = {
    val left = x - (rx / 2)
    val top = y + (ry / 2)
    val right = x + (rx / 2)
    val bottom = y - (ry / 2)
    AndroidCode(_ ++= s"RectF bounds = new RectF($left, $top, $right, $bottom);\nc.drawOval(bounds, p);\n")
  }

  override def text(text: String, x: Double, y: Double): AndroidCode = {
    AndroidCode(_ ++= s"c.drawText($text, $x, $y, p);\n")
  }

  case class PathState(here: Coords, indentation: Int)

  override type Paths = State[PathState, AndroidCode]

  def outputLine(indentation: Int, code: String): SBAction = { sb =>
    sb.ensureCapacity(sb.length + indentation * 4 + code.length() + 1)
    var i = 0
    while (i < indentation) {
      sb ++= "    "
      i += 1
    }
    sb ++= code
    sb += '\n'
  }

  override val path: FinalPath[Paths] =
    new FinalPath[Paths] {
      override val empty: Paths = State.state(AndroidCode(Vector()))

      def foldLast[C](vec: Vector[C])(con: (C, Coords) => String)(out: (C, Coords) => Coords): Paths =
        if (vec.isEmpty) empty
        else State[PathState, AndroidCode](state =>
          (state.copy(here = out(vec.last, state.here)),
            AndroidCode(vec.map(v => outputLine(state.indentation, con(v, state.here)))))
        )

      // assumes as an optimization that none of the intermediate coords changes can be observed by `out` or `con`.
      // this is always the case because if we are folding a vector of arguments for the same command, the arguments
      // must all observe and modify the same coordinates and none observe and modify the same coordinate.
      def foldSum[C](vec: Vector[C])(con: (C, Coords) => String)(out: (C, Coords) => Coords)(add: (C, C) => C): Paths =
      if (vec.isEmpty) empty
      else State[PathState, AndroidCode](state =>
        (state.copy(here = out(vec.reduce(add), state.here)),
          AndroidCode(vec.map(v => outputLine(state.indentation, con(v, state.here)))))
      )

      def addCoords(p1: Coords, p2: Coords): Coords =
        (p1._1 + p2._1, p1._2 + p2._2)

      def addThirdCoords(p1: (Coords, Coords, Coords), p2: (Coords, Coords, Coords)): (Coords, Coords, Coords) =
        (p1._1, p1._2, (p1._3._1 + p2._3._1, p1._3._2 + p2._3._2))

      def addSecondCoords(p1: (Coords, Coords), p2: (Coords, Coords)): (Coords, Coords) =
        (p1._1, (p1._2._1 + p2._2._1, p1._2._2 + p2._2._2))

      def addXCoords(p1: Coords, p2: Coords): Coords =
        (p1._1 + p2._1, p1._2)

      def addYCoords(p1: Coords, p2: Coords): Coords =
        (p1._1, p1._2 + p2._2)

      override def append(fst: Paths, snd: Paths): Paths =
        StateT.stateMonad.lift2((a: AndroidCode, b: AndroidCode) => AndroidCode(a.ap ++ b.ap))(fst, snd)

      override def closePath(): Paths =
        State[PathState, AndroidCode](state => (state, AndroidCode(outputLine(state.indentation, "path.close();"))))

      override def moveTo(points: Vector[Coords]): Paths =
        foldLast(points) { case (_, (x, y)) => s"path.moveTo($x, $y);" }((c, _) => c)

      override def moveToRel(points: Vector[Coords]): Paths =
        foldSum(points) { case (_, (x, y)) => s"path.rMoveTo($x, $y);" }(addCoords)(addCoords)

      override def lineTo(points: Vector[Coords]): Paths =
        foldLast(points) { case (_, (x, y)) => s"path.lineTo($x, $y);" }((c, _) => c)

      override def lineToRel(points: Vector[Coords]): Paths =
        foldSum(points) { case (_, (x, y)) => s"path.rLineTo($x, $y);" }(addCoords)(addCoords)

      override def verticalLineTo(y: Vector[Double]): Paths =
        foldLast(y)((g, c) => s"path.lineTo(${c._1}, $g);")((c, a) => a.copy(_2 = c))

      override def verticalLineToRel(y: Vector[Double]): Paths =
        foldSum(y)((g, c) => s"path.rLineTo(0, $g);")((c, a) => a.copy(_2 = c))(_ + _)

      override def horizLineTo(x: Vector[Double]): Paths =
        foldLast(x)((g, c) => s"path.lineTo($g, ${c._2});")((c, a) => a.copy(_1 = c))

      override def horizLineToRel(x: Vector[Double]): Paths =
        foldSum(x)((g, c) => s"path.rLineTo($g, 0);")((c, a) => a.copy(_1 = c))(_ + _)

      override def cubic(params: Vector[(Coords, Coords, Coords)]): Paths =
        foldLast(params)(
          (g, c) => s"path.cubicTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2}, ${g._3._1}, ${g._3._2});"
        )((c, d) => c._3)

      override def cubicRel(params: Vector[(Coords, Coords, Coords)]): Paths =
        foldSum(params)(
          (g, c) => s"path.rCubicTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2}, ${g._3._1}, ${g._3._2});"
        )((c, d) => addCoords(c._3, d))(addThirdCoords)

      override def smoothCubic(params: Vector[(Coords, Coords, Coords)]): Paths =
        State.state(AndroidCode(s"???"))

      override def smoothCubicRel(params: Vector[(Coords, Coords, Coords)]): Paths =
        State.state(AndroidCode(s"???"))

      override def quad(params: Vector[(Coords, Coords)]): Paths =
        foldLast(params)(
          (g, c) => s"path.quadTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2});"
        )((c, d) => c._2)

      override def quadRel(params: Vector[(Coords, Coords)]): Paths =
        foldSum(params)(
          (g, c) => s"path.quadTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2});"
        )((c, d) => addCoords(c._2, d))(addSecondCoords)

      override def elliptic(params: Vector[EllipticParam]): Paths =
        State.state(AndroidCode(s"???"))

      override def ellipticRel(params: Vector[EllipticParam]): Paths =
        State.state(AndroidCode(s"???"))
    }

  override def includePath(paths: Paths): AndroidCode = {
    val result = paths.eval(PathState(here = (0, 0), indentation = 1))
    val intro: SBAction =
      outputLine(indentation = 0, "{") |+|
        outputLine(indentation = 1, "Path path = new Path();")
    val outro: SBAction =
      outputLine(indentation = 0, "}")
    AndroidCode(intro +: result.ap :+ outro)
  }
}
