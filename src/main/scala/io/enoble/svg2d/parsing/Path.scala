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
  sealed trait PathCommand
  trait Absolute
  trait Relative
  type Coords = (Double, Double)

  def parsedDouble = floatingPointNumber ^^ (_.toDouble)

  def twoCoords = (floatingPointNumber <~ ",") ~ floatingPointNumber

  def lineTo = "l" ~> whiteSpace ~> twoCoords
  def moveTo = "m" ~> whiteSpace ~> twoCoords

  case class ClosePath() extends PathCommand
  case class MoveTo(point: Coords) extends PathCommand
  case class LineTo(point: Coords) extends PathCommand
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
