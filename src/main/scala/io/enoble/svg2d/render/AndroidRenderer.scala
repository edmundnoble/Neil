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

      def foldLast[C](vec: Vector[C])(con: (C, Coords) => A)(out: (C, Coords) => Coords): Paths =
        if (vec.isEmpty) empty
        else State[PathState, A](state =>
          (state.copy(here = out(vec.last, state.here)),
            vec.foldMap(v => outputLine(con(v, state.here), indentation = state.indentation)))
        )

      // assumes as an optimization that none of the intermediate coords changes can be observed by `out` or `con`.
      // this is always the case because if we are folding a vector of arguments for the same command, the arguments
      // must all observe and modify the same coordinates and none observe and modify the same coordinate.
      def foldSum[C](vec: Vector[C])(con: (C, Coords) => A)(out: (C, Coords) => Coords)(add: (C, C) => C): Paths =
      if (vec.isEmpty) empty
      else State[PathState, A](state =>
        (state.copy(here = out(vec.reduce(add), state.here)),
          vec.foldMap(v => outputLine(con(v, state.here), indentation = state.indentation)))
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
        StateT.catsDataMonadForStateT[Eval, PathState].map2(fst, snd)(self.append)

      override def closePath(): Paths =
        State[PathState, A](state => (state, outputLine(in("path.close();"), state.indentation)))

      override def moveTo(points: Vector[Coords]): Paths =
        foldLast(points) { case ((x, y), _) => fm"path.moveTo($x, $y);" }((c, _) => c)

      override def moveToRel(points: Vector[Coords]): Paths =
        foldSum(points) { case ((x, y), _) => fm"path.rMoveTo($x, $y);" }(addCoords)(addCoords)

      override def lineTo(points: Vector[Coords]): Paths =
        foldLast(points) { case ((x, y), _) => fm"path.lineTo($x, $y);" }((c, _) => c)

      override def lineToRel(points: Vector[Coords]): Paths =
        foldSum(points) { case ((x, y), _) => fm"path.rLineTo($x, $y);" }(addCoords)(addCoords)

      override def verticalLineTo(y: Vector[Double]): Paths =
        foldLast(y)((g, c) => fm"path.lineTo(${c._1}, $g);")((c, a) => a.copy(_2 = c))

      override def verticalLineToRel(y: Vector[Double]): Paths =
        foldSum(y)((g, _) => fm"path.rLineTo(0.0, $g);")((c, a) => a.copy(_2 = c))(_ + _)

      override def horizLineTo(x: Vector[Double]): Paths =
        foldLast(x)((g, c) => fm"path.lineTo($g, ${c._2});")((c, a) => a.copy(_1 = c))

      override def horizLineToRel(x: Vector[Double]): Paths =
        foldSum(x)((g, _) => fm"path.rLineTo($g, 0.0);")((c, a) => a.copy(_1 = c))(_ + _)

      override def cubic(params: Vector[(Coords, Coords, Coords)]): Paths =
        foldLast(params)(
          (g, _) => fm"path.cubicTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2}, ${g._3._1}, ${g._3._2});"
        )((c, _) => c._3)

      override def cubicRel(params: Vector[(Coords, Coords, Coords)]): Paths =
        foldSum(params)(
          (g, _) => fm"path.rCubicTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2}, ${g._3._1}, ${g._3._2});"
        )((c, d) => addCoords(c._3, d))(addThirdCoords)

      override def smoothCubic(params: Vector[(Coords, Coords, Coords)]): Paths =
        State.pure(in(s"???"))

      override def smoothCubicRel(params: Vector[(Coords, Coords, Coords)]): Paths =
        State.pure(in(s"???"))

      override def quad(params: Vector[(Coords, Coords)]): Paths =
        foldLast(params)(
          (g, _) => fm"path.quadTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2});"
        )((c, _) => c._2)

      override def quadRel(params: Vector[(Coords, Coords)]): Paths =
        foldSum(params)(
          (g, _) => fm"path.quadTo(${g._1._1}, ${g._1._2}, ${g._2._1}, ${g._2._2});"
        )((c, d) => addCoords(c._2, d))(addSecondCoords)

      override def elliptic(params: Vector[EllipticParam]): Paths =
        State.pure(in(s"???"))

      override def ellipticRel(params: Vector[EllipticParam]): Paths =
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
