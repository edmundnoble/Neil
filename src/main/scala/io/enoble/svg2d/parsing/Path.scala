package io.enoble.svg2d.parsing

import io.enoble.svg2d.parsing.Path.PathCommand

import scala.xml.Elem

object PathParser extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "path"
  override def apply(v1: Elem): Option[Code] = {
    val pathCoords = v1.attribute("d")
    pathCoords map { coords =>
      Code.empty
    }
  }
}

object Path {
  sealed trait PathCommand
  trait Absolute
  trait Relative
  type Coords = (Double, Double)

  case class ClosePath() extends PathCommand
  case class MoveTo(point: Coords)
  case class LineTo(point: Coords)

}

case class Path(commands: Seq[PathCommand]) extends Code {

  override def toAndroidCode: AndroidCode = {
    ""
  }
  override def toIOSCode: IOSCode = {
    ""
  }
}
