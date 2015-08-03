package io.enoble.svg2d.parsing

import io.enoble.svg2d.parsing.Code.Named
import io.enoble.svg2d.parsing.Path.PathCommand

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

object Path {
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
  case class Cubic(params: Seq[(Coords, Coords, Coords)]) extends PathCommand
  case class CubicRel(params: Seq[(Coords, Coords, Coords)]) extends PathCommand
  case class SmoothCubic(params: Seq[(Coords, Coords, Coords)]) extends PathCommand
  case class SmoothCubicRel(params: Seq[(Coords, Coords, Coords)]) extends PathCommand
  case class Quad(params: Seq[(Coords, Coords)]) extends PathCommand
  case class QuadRel(params: Seq[(Coords, Coords)]) extends PathCommand
  case class EllipticParam(r: Coords, rotX: Double, largeArc: Boolean, sweep: Boolean, p: Coords)
  case class Elliptic(params: Seq[EllipticParam]) extends PathCommand
  case class EllipticRel(params: Seq[EllipticParam]) extends PathCommand

  object Parsers {

    import fastparse.all._

    import scala.language.implicitConversions
    import scalaz.std.list._

    case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
      def apply(t: T) = f(t)
      override def toString() = name
    }

    // Here is the parser
    val Whitespace = NamedFunction(" \n".contains(_: Char), "Whitespace")
    val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
    val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

    val space         = P( CharsWhile(Whitespace).? )
    val digits        = P( CharsWhile(Digits))
    val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
    val fractional    = P( "." ~ digits )
    val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

    val number: P[Double] = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(_.toDouble)

    val wsp = space
    val commaWsp = (wsp.rep(1) ~ ",".? ~ wsp.rep) | ("," ~ wsp.rep)
    val wspDouble = wsp.rep ~ number

    val coordPair: P[Coords] = (number ~ commaWsp.?) ~ number
    val twoCoordPairs: P[(Coords, Coords)] = (coordPair ~ commaWsp.? ~ coordPair).map {
      case (x1, y1, c1) => ((x1, y1), c1)
    }
    val threeCoordPairs: P[(Coords, Coords, Coords)] = (coordPair ~ commaWsp.? ~ coordPair ~ commaWsp.? ~ coordPair).map {
      case (x1, y1, c1, c2) => ((x1, y1), c1, c2)
    }
    val wspAndCoordPairs: P[(Coords, Coords)] = commaWsp.rep ~ twoCoordPairs
    val wspAndCoord: P[Coords] = commaWsp.rep ~ coordPair
    val moveToArgs = (wspAndCoord ~ commaWsp.?).rep(1)
    val moveTo: P[PathCommand] = ("M" ~ moveToArgs) map MoveTo
    val lineToArgs = moveToArgs
    val lineTo: P[PathCommand] = (("L" ~ lineToArgs) map LineToRel) | (("l" ~ lineToArgs) map LineTo)
    def singleLineToArgs = (wspDouble ~ commaWsp.?).rep(1)
    def horizLineTo: P[PathCommand] = (("h" ~ singleLineToArgs) map HorizLineToRel) | (("H" ~ singleLineToArgs) map HorizLineTo)
    def vertLineTo: P[PathCommand] = (("v" ~ singleLineToArgs) map VerticalLineToRel) | (("V" ~ singleLineToArgs) map VerticalLineTo)
    // TODO: Find out what this is for
    //def moveToRel: Parser[PathCommand] = ("m" ~> spaceAndCoords) ^^ MoveToRel
    def quadArgs = (twoCoordPairs ~ commaWsp.?).rep(1)
    def quad: P[PathCommand] = (("q" ~ quadArgs) map Quad) | (("Q" ~ quadArgs) map QuadRel)
    //    def ellipticArgs: Parser[EllipticParam] =
    //    def ellipticalArc: Parser[PathCommand] = ("a" ~> ellipticArgs ^^ Elliptic) | ("A" ~> ellipticArgs ^^ EllipticRel)
    def cubicArgs = (threeCoordPairs ~ commaWsp.?).rep(1)
    def cubic: Parser[PathCommand] = ("c" ~ cubicArgs map Cubic) | ("C" ~ cubicArgs map CubicRel)
    def smoothCubic: Parser[PathCommand] = ("s" ~ cubicArgs map SmoothCubic) | ("S" ~ cubicArgs map SmoothCubicRel)
    def closePath: Parser[PathCommand] = CharIn("z") map (_ => ClosePath())
    def command: Parser[PathCommand] = closePath | lineTo | horizLineTo | vertLineTo | cubic | smoothCubic | quad
    //| ellipticalArc
    def path: Parser[Seq[(PathCommand, Seq[PathCommand])]] = ((moveTo ~ wsp) ~ (command ~ wsp).rep(1) ~ wsp).rep(1)
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
    AndroidCode("{" +|+
      "Path path = new Path()" +|+
      "double x = 0" +|+
      "double y = 0" +|+
      commands.foldLeft("") { (str, cmd) =>
        str +|+ (cmd match {
          case ClosePath() => "path.close()"
          case MoveTo(coords) => foldCmd[Coords](coords, { case (x, y) => s"path.moveTo($x, $y)" +|+ setCoords(x, y) })
          case MoveToRel(coords) => foldCmd[Coords](coords, { case (x, y) => s"path.rMoveTo($x, $y)" +|+ changeCoords(x, y)})
          case LineTo(coords) => foldCmd[Coords](coords, { case (x, y) => s"path.lineTo($x, $y)" +|+ setCoords(x, y) })
          case LineToRel(coords) => foldCmd[Coords](coords, { case (x, y) => s"path.rLineTo($x, $y)" +|+ changeCoords(x, y) })
          case VerticalLineTo(coords) => foldCmd[Double](coords, y => s"path.lineTo(x, $y)" +|+ setYCoord(y) )
          case VerticalLineToRel(coords) => foldCmd[Double](coords, y => s"path.rLineTo(0, $y)" +|+ changeYCoord(y))
          case HorizLineTo(coords) => foldCmd[Double](coords, x => s"path.lineTo($x, y)" +|+ setXCoord(x) )
          case HorizLineToRel(coords) => foldCmd[Double](coords, x => s"path.rLineTo($x, 0)" +|+ changeXCoord(x))
          case Cubic(coords) => foldCmd[(Coords, Coords, Coords)](coords, { case ((x1, y1), (x2, y2), (x3, y3)) => s"path.cubicTo($x1, $y1, $x2, $y2, $x3, $y3)" }) // TODO
          case CubicRel(coords) => foldCmd[(Coords, Coords, Coords)](coords, { case ((x1, y1), (x2, y2), (x3, y3)) => s"path.rCubicTo($x1, $y1, $x2, $y2, $x3, $y3)" }) // TODO
          case SmoothCubic(coords) => ??? // TODO
          case SmoothCubicRel(coords) => ??? // TODO
          case Quad(coords) => foldCmd[(Coords, Coords)](coords, { case ((x1, y1), (x2, y2)) => s"path.quadTo($x1, $y1, $x2, $y2)" }) // TODO
          case QuadRel(coords) => foldCmd[(Coords, Coords)](coords, { case ((x1, y1), (x2, y2)) => s"path.rQuadTo($x1, $y1, $x2, $y2)" }) // TODO
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
