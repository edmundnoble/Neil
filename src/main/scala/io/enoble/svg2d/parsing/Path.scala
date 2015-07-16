package io.enoble.svg2d.parsing

import io.enoble.svg2d.parsing.Path.PathCommand

import scala.util.parsing.combinator._

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
  case class LineTo(point: Coords) extends PathCommand
  case class Cubic(c1: Coords, c2: Coords, c: Coords) extends PathCommand

  object Parsers extends JavaTokenParsers {
    import scala.language.implicitConversions

    implicit def TildeToTuple[A, B, C](tf: ((A, B)) => C): ((~[A, B]) => C) = (t: ~[A, B]) => tf((t._1, t._2))
    implicit def TildeToTuple[A, B](tf: ~[A, B]): (A, B) = (tf._1, tf._2)

    def parsedDouble = floatingPointNumber ^^ (_.toDouble)
    def twoCoords: Parser[~[Double, Double]] = (parsedDouble <~ ",") ~ parsedDouble
    def spaceAndCoords: Parser[~[Double, Double]] = whiteSpace ~> twoCoords
    def lineTo: Parser[PathCommand] = ("l" ~> spaceAndCoords) ^^ LineTo
    def moveTo: Parser[PathCommand] = ("m" ~> spaceAndCoords) ^^ MoveTo
    def cubic: Parser[PathCommand] = ("c" ~> spaceAndCoords ~ spaceAndCoords ~ spaceAndCoords ) ^^ {
      case c1 ~ c2 ~ c  => Cubic(c1, c2, c)
    }
    def closePath: Parser[PathCommand] = "z" ^^ (_ => ClosePath())
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
