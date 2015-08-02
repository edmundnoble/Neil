package io.enoble.svg2d.parsing

import io.enoble.svg2d.parsing.Code.Named
import io.enoble.svg2d.parsing.Path.PathCommand

import scala.util.parsing.combinator._
import scalaz._
import Scalaz._
import scalaz.std.list._

object PathParser extends Model {

  import Path._
  import xml.Elem

  override def isDefinedAt(x: Elem): Boolean = x.label =~= "path"

  override def apply(v1: Elem): Option[Code] = {
    val pathCoords = v1.getOpt("d")
    pathCoords foreach println
    None
  }
}

object Path extends JavaTokenParsers {
  type Coords = (Double, Double)
  sealed trait PathCommand
  case class ClosePath() extends PathCommand
  case class MoveTo(points: Seq[Coords]) extends PathCommand
  case class MoveToRel(points: Seq[Coords]) extends PathCommand
  case class LineTo(points: Seq[Coords]) extends PathCommand
  case class LineToRel(point: Seq[Coords]) extends PathCommand
  case class VerticalLineTo(y: Seq[Double]) extends PathCommand
  case class VerticalLineToRel(y: Seq[Double]) extends PathCommand
  case class HorizLineTo(x: Seq[Double]) extends PathCommand
  case class HorizLineToRel(x: Seq[Double]) extends PathCommand
  case class Cubic(params: List[(Coords, Coords, Coords)]) extends PathCommand
  case class CubicRel(params: List[(Coords, Coords, Coords)]) extends PathCommand
  case class SmoothCubic(params: List[(Coords, Coords, Coords)]) extends PathCommand
  case class SmoothCubicRel(params: List[(Coords, Coords, Coords)]) extends PathCommand
  case class Quad(params: List[(Coords, Coords)]) extends PathCommand
  case class QuadRel(params: List[(Coords, Coords)]) extends PathCommand
  case class EllipticParam(r: Coords, rotX: Double, largeArc: Boolean, sweep: Boolean, p: Coords)
  case class Elliptic(params: List[EllipticParam]) extends PathCommand
  case class EllipticRel(params: List[EllipticParam]) extends PathCommand

  object Parsers extends JavaTokenParsers {

    import scala.language.implicitConversions
    import scalaz.std.list._

    // I'd like to know why I need to do this. I don't want to have a billion (uncomposable) abstractions
    // to switch between every time I go between the parser world and the real world
    implicit def tildeToTupleFun[A, B, C](tf: ((A, B)) => C): ((~[A, B]) => C) = (t: ~[A, B]) => tf((t._1, t._2))
    implicit def tildeToTupleK[F[_] : Functor, A, B, C](tf: (F[(A, B)]) => C): (F[~[A, B]]) => C = (t: F[~[A, B]]) => tf(t.map(tildeToTuple2))
    implicit def tildeToTuple2[A, B](tf: ~[A, B]): (A, B) = (tf._1, tf._2)
    implicit def tildeToTuple3[A, B, C](tf: ~[~[A, B], C]): (A, B, C) = (tf._1._1, tf._1._2, tf._2)
    implicit def tildeToTuple4[A, B, C, D](tf: ~[~[A, B], ~[C, D]]): ((A, B), (C, D)) = ((tf._1._1, tf._1._2), (tf._2._1, tf._2._2))
    implicit def tildeToTuple4K[F[_] : Functor, A, B, C, D](tf: F[~[~[A, B], ~[C, D]]]): F[((A, B), (C, D))] = tf.map(tildeToTuple4)
    def makeTwoArg[T](f: (List[(Coords, Coords)]) => T) = (a: List[~[~[Double, Double], ~[Double, Double]]]) => f(tildeToTuple4K(a))
    def makeThreeArg[T](f: (List[(Coords, Coords, Coords)]) => T) = {
      (a: List[~[~[~[Double, Double], ~[Double, Double]], ~[Double, Double]]]) => {
        f(a.map { case (c1 ~ c2) ~ (c3 ~ c4) ~ (c5 ~ c6) => ((c1, c2), (c3, c4), (c5, c6)) })
      }
    }

    def parsedDouble = floatingPointNumber ^^ (_.toDouble)
    def wsp = whiteSpace
    def commaWsp = (wsp.+ ~> ",".? ~> wsp.*) | ("," ~> wsp.*)
    def wspDouble = wsp.* ~> parsedDouble

    def coordPair: Parser[~[Double, Double]] = (parsedDouble <~ commaWsp.?) ~ parsedDouble
    def wspPair: Parser[~[Double, Double]] = coordPair <~ commaWsp.?
    def twoCoordPairs: Parser[~[~[Double, Double], ~[Double, Double]]] = wspPair ~ coordPair
    def threeCoordPairs = wspPair ~ wspPair ~ coordPair
    def wspAndCoordPairs: Parser[~[~[Double, Double], ~[Double, Double]]] = commaWsp.* ~> twoCoordPairs
    def wspAndCoord: Parser[~[Double, Double]] = commaWsp.* ~> coordPair
    def moveToArgs = rep1sep(wspAndCoord, commaWsp.?)
    def moveTo: Parser[PathCommand] = ("M" ~> moveToArgs) ^^ MoveTo
    def lineToArgs = moveToArgs
    def lineTo: Parser[PathCommand] = (("L" ~> lineToArgs) ^^ LineToRel) | (("l" ~> lineToArgs) ^^ LineTo)
    def singleLineToArgs = rep1sep(wspDouble, commaWsp.?)
    def horizLineTo: Parser[PathCommand] = (("h" ~> singleLineToArgs) ^^ HorizLineToRel) | (("H" ~> singleLineToArgs) ^^ HorizLineTo)
    def vertLineTo: Parser[PathCommand] = (("v" ~> singleLineToArgs) ^^ VerticalLineToRel) | (("V" ~> singleLineToArgs) ^^ VerticalLineTo)
    // TODO: Find out what this is for
    //def moveToRel: Parser[PathCommand] = ("m" ~> spaceAndCoords) ^^ MoveToRel
    def quadArgs = rep1sep(twoCoordPairs, commaWsp.?)
    def quad: Parser[PathCommand] = ("q" ~> quadArgs ^^ makeTwoArg(Quad)) | ("Q" ~> quadArgs ^^ makeTwoArg(QuadRel))
    //    def ellipticArgs: Parser[EllipticParam] =
    //    def ellipticalArc: Parser[PathCommand] = ("a" ~> ellipticArgs ^^ Elliptic) | ("A" ~> ellipticArgs ^^ EllipticRel)
    def cubicArgs = rep1sep(threeCoordPairs, commaWsp.?)
    def cubic: Parser[PathCommand] = ("c" ~> cubicArgs ^^ makeThreeArg(Cubic)) | (("C" ~> cubicArgs) ^^ makeThreeArg(CubicRel))
    def smoothCubic: Parser[PathCommand] = ("s" ~> cubicArgs ^^ makeThreeArg(SmoothCubic)) | ("S" ~> cubicArgs ^^ makeThreeArg(SmoothCubicRel))
    def closePath: Parser[PathCommand] = "z" ^^ (_ => ClosePath())
    def command: Parser[PathCommand] = closePath | lineTo | horizLineTo | vertLineTo | cubic | smoothCubic | quad
    //| ellipticalArc
    def path: Parser[Seq[~[PathCommand, Seq[PathCommand]]]] = rep1sep((moveTo <~ wsp) ~ rep1sep(command, wsp), wsp)
  }

}


case class Path(commands: PathCommand*) extends Code {

  import Path._

  def trackCoords(x: Double, y: Double): String =
    s"""
      x = $x;
      x = $y;
     """

  override def toAndroidCode: Named[AndroidCode] =
    ("{" +|+
      "Path path = new Path()" +|+
      "double x = 0" +|+
      "double y = 0" +|+
      commands.foldLeft("") { (str, cmd) =>
        str +|+ (cmd match {
          case ClosePath() => "path.close()"
          case MoveTo(coords) => foldCmd(coords, { case (x, y) => s"path.moveTo($x, $y)" +|+ setCoords(x, y) })
          case MoveToRel(coords) => foldCmd(coords, { case (x, y) => s"path.rMoveTo($x, $y)" +|+ changeCoords(x, y)})
          case LineTo(coords) => foldCmd(coords, { case (x, y) => s"path.lineTo($x, $y)" +|+ setCoords(x, y) })
          case LineToRel(coords) => foldCmd(coords, { case (x, y) => s"path.rLineTo($x, $y)" +|+ changeCoords(x, y) })
          case VerticalLineTo(coords) => foldCmd(coords, y => s"path.lineTo(x, $y)" +|+ setYCoord(y) )
          case VerticalLineToRel(coords) => foldCmd(coords, y => s"path.rLineTo(0, $y)" +|+ changeYCoord(y))
          case HorizLineTo(coords) => foldCmd(coords, x => s"path.lineTo($x, y)" +|+ setXCoord(x) )
          case HorizLineToRel(coords) => foldCmd(coords, x => s"path.rLineTo($x, 0)" +|+ changeXCoord(x))
          case Cubic(coords) => foldCmd(coords, { case ((x1, y1), (x2, y2), (x3, y3)) => s"path.cubicTo($x1, $y1, $x2, $y2, $x3, $y3)" }) // TODO
          case CubicRel(coords) => foldCmd(coords, { case ((x1, y1), (x2, y2), (x3, y3)) => s"path.rCubicTo($x1, $y1, $x2, $y2, $x3, $y3)" }) // TODO
          case SmoothCubic(coords) => ??? // TODO
          case SmoothCubicRel(coords) => ??? // TODO
          case Quad(coords) => foldCmd(coords, { case ((x1, y1), (x2, y2)) => s"path.quadTo($x1, $y1, $x2, $y2)" }) // TODO
          case QuadRel(coords) => foldCmd(coords, { case ((x1, y1), (x2, y2)) => s"path.rQuadTo($x1, $y1, $x2, $y2)" }) // TODO
          case Elliptic(_) => ??? // TODO
          case EllipticRel(_) => ??? // TODO
        })
      }
      ).pure[Named]

  def setCoords(x: Double, y: Double): String = setXCoord(x) +|+ setYCoord(y)

  def setYCoord(y: Double): String = s"y = $y"
  def setXCoord(x: Double): String = s"x = $x"

  def changeYCoord(dy: Double): String = s"y += $dy"
  def changeXCoord(dx: Double): String = s"x += $dx"

  def changeCoords(dx: Double, dy: Double): String = changeXCoord(dx) +|+ changeYCoord(dy)

  def foldCmd[T](s: Seq[T], f: T => String): String = s.map(f) match {
    case x if x.isEmpty => ""
    case xs => xs.reduce(_ +|+ _)
  }

  override def toIOSCode: Named[IOSCode] = {
    "".pure[Named]
  }
}
