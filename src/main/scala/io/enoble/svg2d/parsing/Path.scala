package io.enoble.svg2d.parsing

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
    pathCoords map { coords =>
      Path(MoveTo((1, 2)))
    }
  }
}

object Path extends JavaTokenParsers {
  type Coords = (Double, Double)
  sealed trait PathCommand
  trait Absolute
  trait Relative
  case class ClosePath() extends PathCommand
  case class MoveTo(point: Coords) extends PathCommand
  case class MoveToRel(point: Coords) extends PathCommand
  case class LineTo(point: Coords) extends PathCommand
  case class LineToRel(point: Coords) extends PathCommand
  case class VerticalLineTo(y: Double) extends PathCommand
  case class VerticalLineToRel(y: Double) extends PathCommand
  case class HorizLineTo(x: Double) extends PathCommand
  case class HorizLineToRel(x: Double) extends PathCommand
  case class Cubic(c1: Coords, c2: Coords, c: Coords) extends PathCommand
  case class CubicRel(c1: Coords, c2: Coords, c: Coords) extends PathCommand
  case class CurveTo(params: List[(Coords, Coords)]) extends PathCommand
  case class CurveToRel(params: List[(Coords, Coords)]) extends PathCommand
  case class SmoothCurveTo(params: List[(Coords, Coords)]) extends PathCommand
  case class SmoothCurveToRel(params: List[(Coords, Coords)]) extends PathCommand
  case class Quad(c1: Coords, c: Coords) extends PathCommand

  object Parsers extends JavaTokenParsers {

    import scala.language.implicitConversions
    import scalaz.std.list._

    implicit def tildeToTupleFun[A, B, C](tf: ((A, B)) => C): ((~[A, B]) => C) = (t: ~[A, B]) => tf((t._1, t._2))
    implicit def tildeToTuple2[A, B](tf: ~[A, B]): (A, B) = (tf._1, tf._2)
    implicit def tildeToTuple3[A, B, C](tf: ~[~[A, B], C]): (A, B, C) = (tf._1._1, tf._1._2, tf._2)
    implicit def tildeToTuple4[A, B, C, D](tf: ~[~[A, B], ~[C, D]]): ((A, B), (C, D)) = ((tf._1._1, tf._1._2), (tf._2._1, tf._2._2))
    implicit def tildeToTuple4K[F[_] : Functor, A, B, C, D](tf: F[~[~[A, B], ~[C, D]]]): F[((A, B), (C, D))] = tf.map(tildeToTuple4)
    def makeCurveTo[T](f: (List[(Coords, Coords)]) => T) = (a: List[~[~[Double, Double], ~[Double, Double]]]) => f(tildeToTuple4K(a))

    def parsedDouble = floatingPointNumber ^^ (_.toDouble)
    def wsp = whiteSpace
    def commaWsp = (wsp.+ ~> ",".? ~> wsp.*) | ("," ~> wsp.*)
    def doubleWsp = wsp ~> parsedDouble

    def twoCoords: Parser[~[Double, Double]] = (parsedDouble <~ commaWsp) ~ parsedDouble
    def wspAndCoords: Parser[~[Double, Double]] = commaWsp.? ~> twoCoords
    def lineTo: Parser[PathCommand] = (("L" ~> wspAndCoords) ^^ LineToRel) | (("l" ~> wspAndCoords) ^^ LineTo)
    def horizLineTo: Parser[PathCommand] = (("h" ~> doubleWsp) ^^ HorizLineToRel) | (("H" ~> doubleWsp) ^^ HorizLineTo)
    def vertLineTo: Parser[PathCommand] = (("v" ~> doubleWsp) ^^ VerticalLineToRel) | (("V" ~> doubleWsp) ^^ VerticalLineTo)
    def curveToArgs = ((wspAndCoords <~ commaWsp) ~ wspAndCoords).*
    def curveTo: Parser[PathCommand] = ("c" ~> curveToArgs ^^ makeCurveTo(CurveTo)) | ("C" ~> curveToArgs ^^ makeCurveTo(CurveToRel))
    def smoothCurveTo: Parser[PathCommand] = ("c" ~> curveToArgs ^^ makeCurveTo(SmoothCurveTo)) | ("C" ~> curveToArgs ^^ makeCurveTo(SmoothCurveToRel))
    def moveTo: Parser[PathCommand] = ("M" ~> wspAndCoords) ^^ MoveTo
    //def quadTo: Parser[PathCommand] =
    // TODO: Find out what this is for
    //def moveToRel: Parser[PathCommand] = ("m" ~> spaceAndCoords) ^^ MoveToRel
    def threeCoords = wspAndCoords ~ wspAndCoords ~ wspAndCoords
    def cubic: Parser[PathCommand] = ("c" ~> threeCoords) ^^ {
      case c1 ~ c2 ~ c => Cubic(c1, c2, c)
    } | ("C" ~> threeCoords) ^^ {
      case c1 ~ c2 ~ c => CubicRel(c1, c2, c)
    }
    def closePath: Parser[PathCommand] = "z" ^^ (_ => ClosePath())
    def command: Parser[PathCommand] = closePath | lineTo | horizLineTo | vertLineTo | curveTo | smoothCurveTo //|
      //quadCurveTo | ellipticalArc
    def path: Parser[~[PathCommand, Seq[PathCommand]]] = moveTo ~ command.*
  }

}


case class Path(commands: PathCommand*) extends Code {

  import Path._

  override def toAndroidCode: AndroidCode =
    java"""
      {
      Path path = new Path()
      ${
      commands.foldLeft("") { (str, cmd) =>
        str + "\n" + (cmd match {
          case LineTo((x, y)) => s"path.lineTo($x, $y)"
          case LineToRel((x, y)) => s"path.rLineTo($x, $y)"
          case MoveTo((x, y)) => s"path.moveTo($x, $y)"
          case MoveToRel((x, y)) => s"path.rMoveTo($x, $y)"
          case ClosePath() => s"path.close()"
        })
      }
    }
      }
    """.stripMargin

  override def toIOSCode: IOSCode = {
    ""
  }
}
