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
  case class Quad(c1: Coords, c: Coords) extends PathCommand

  object Parsers extends JavaTokenParsers {

    import scala.language.implicitConversions

    implicit def tildeToTupleFun[A, B, C](tf: ((A, B)) => C): ((~[A, B]) => C) = (t: ~[A, B]) => tf((t._1, t._2))
    implicit def tildeToTuple2[A, B](tf: ~[A, B]): (A, B) = (tf._1, tf._2)
    implicit def tildeToTuple3[A, B, C](tf: ~[~[A, B], C]): (A, B, C) = (tf._1._1, tf._1._2, tf._2)
    implicit def tildeToTuple4[A, B, C, D](tf: ~[~[A, B], ~[C, D]]): ((A, B), (C, D)) = ((tf._1._1, tf._1._2), (tf._2._1, tf._2._2))
    implicit def tildeToTuple4K[F[_] : Functor, A, B, C, D](tf: F[~[~[A, B], ~[C, D]]]): F[((A, B), (C, D))] = tf.map(tildeToTuple4)

    def parsedDouble = floatingPointNumber ^^ (_.toDouble)
    def twoCoords: Parser[~[Double, Double]] = (parsedDouble <~ ",") ~ parsedDouble
    def spaceAndCoords: Parser[~[Double, Double]] = regex(whiteSpace).? ~> twoCoords
    def lineTo: Parser[PathCommand] = (("L" ~> spaceAndCoords) ^^ LineToRel) | (("l" ~> spaceAndCoords) ^^ LineTo)
    def horizLineTo: Parser[PathCommand] = (("H" ~> whiteSpace ~> parsedDouble) ^^ HorizLineToRel) | (("h" ~> whiteSpace ~> parsedDouble) ^^ HorizLineTo)
    def vertLineTo: Parser[PathCommand] = (("v" ~> whiteSpace ~> parsedDouble) ^^ VerticalLineToRel) | (("V" ~> whiteSpace ~> parsedDouble) ^^ VerticalLineTo)
    def makeCurveTo[T](f: (List[(Coords, Coords)]) => T) = (a: List[~[~[Double, Double], ~[Double, Double]]]) => f(tildeToTuple4K(a))
    def curveToArgs = (spaceAndCoords ~ spaceAndCoords).*
    def curveTo: Parser[PathCommand] = ("c" ~> curveToArgs ^^ makeCurveTo(CurveTo)) | ("C" ~> curveToArgs ^^ makeCurveTo(CurveToRel))
    def moveTo: Parser[PathCommand] = ("M" ~> spaceAndCoords) ^^ MoveTo
    // TODO: Find out what this is for
    //def moveToRel: Parser[PathCommand] = ("m" ~> spaceAndCoords) ^^ MoveToRel
    def cubic: Parser[PathCommand] = ("c" ~> spaceAndCoords ~ spaceAndCoords ~ spaceAndCoords) ^^ {
      case c1 ~ c2 ~ c => Cubic(c1, c2, c)
    }
    def cubicRel: Parser[PathCommand] = ("C" ~> spaceAndCoords ~ spaceAndCoords ~ spaceAndCoords) ^^ {
      case c1 ~ c2 ~ c => CubicRel(c1, c2, c)
    }
    def closePath: Parser[PathCommand] = "z" ^^ (_ => ClosePath())
    def command: Parser[PathCommand] = closePath | lineTo | horizLineTo | vertLineTo | curveTo | smoothCurveTo | quadCurveTo | smoothQuadCurveTo | ellipticalArc
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
          case MoveTo((x, y)) => s"path.moveTo($x, $y)"
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
