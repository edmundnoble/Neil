package io.enoble.svg2d.parsing

import io.enoble.svg2d.parsing.Code.Named
import io.enoble.svg2d.parsing.Path.PathCommand

import scalaz._
import Scalaz._
import scalaz.std.list._

object PathParser extends Model {

  import Path._
  import xml.Elem
  import fastparse.core._
  import fastparse.core.Result._

  override def isDefinedAt(x: Elem): Boolean = x.label =~= "path"

  override def apply(v1: Elem): Option[Code] = {
    val pathCoords = v1.getOpt("d")
    val parsedPath: Option[Result[Path]] = pathCoords.map(s => Path.Parsers.path.parse(s))
    if (parsedPath.isEmpty) System.err.println("No 'd' attribute found in path element")
    parsedPath.flatMap {
      case Success(path, _) => Some(path)
      case x@Failure(e, _) => System.err.println(s"Failed to parse path: ${pathCoords.get}"); None
    }
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

    case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
      def apply(t: T) = f(t)
      override def toString() = name
    }

    // Here is the parser
    val Whitespace = NamedFunction(" \n".contains(_: Char), "Whitespace")
    val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
    val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

    val space = P(CharsWhile(Whitespace).?)
    val digits = P(CharsWhile(Digits))
    val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
    val fractional = P("." ~ digits)
    val integral = P("0" | CharIn('1' to '9') ~ digits.?)

    val number: P[Double] = P(CharIn("+-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toDouble)

    val commaWsp = P((space ~ ",".? ~ space) | ("," ~ space))
    val wspDouble = P(space ~ number)

    val coordPair: P[Coords] = (number ~ commaWsp) ~! number
    val twoCoordPairs: P[(Coords, Coords)] = P((coordPair ~ commaWsp ~ coordPair).map {
      case (x1, y1, c1) => ((x1, y1), c1)
    })
    val threeCoordPairs: P[(Coords, Coords, Coords)] = P((coordPair ~ commaWsp.? ~ coordPair ~ commaWsp.? ~ coordPair).map {
      case (x1, y1, c1, c2) => ((x1, y1), c1, c2)
    })
    val wspAndCoordPairs: P[(Coords, Coords)] = P(commaWsp ~ twoCoordPairs)
    val wspAndCoord: P[Coords] = P(commaWsp ~ coordPair)
    // TODO: All of the extra arguments to moveTo should be interpreted as lineTo's, according to the spec
    val moveToArgs = P(wspAndCoord.rep(1))
    val moveTo: P[PathCommand] = P(("m" ~ moveToArgs map MoveToRel) | ("M" ~ moveToArgs map MoveTo))
    val lineToArgs = P(moveToArgs)
    val lineTo: P[PathCommand] = P(("L" ~ lineToArgs map LineTo) | ("l" ~ lineToArgs map LineToRel))
    val singleLineToArgs = P((wspDouble ~ commaWsp).rep(1))
    val horizLineTo: P[PathCommand] = P((("h" ~ singleLineToArgs) map HorizLineToRel) | (("H" ~ singleLineToArgs) map HorizLineTo))
    val vertLineTo: P[PathCommand] = P((("v" ~ singleLineToArgs) map VerticalLineToRel) | (("V" ~ singleLineToArgs) map VerticalLineTo))
    val quadArgs = P((twoCoordPairs ~ commaWsp.?).rep(1))
    val quad: P[PathCommand] = P((("q" ~ space ~ quadArgs) map QuadRel) | (("Q" ~ space ~ quadArgs) map Quad))
    val flag: P[Boolean] = CharIn("01").!.map{
      case "0" => false
      case "1" => true
    }
    val ellipticParam: P[EllipticParam] = (number ~ commaWsp ~ number ~ commaWsp ~ number ~ commaWsp ~ flag ~ commaWsp ~ flag ~ commaWsp ~ coordPair) map {
      case (x, y, z, f, f2, coords) => EllipticParam((x, y), z, f, f2, coords)
    }
    val ellipticArgs: P[Seq[EllipticParam]] = P((ellipticParam ~ commaWsp).rep(1))
    val ellipticalArc: P[PathCommand] = P(("a" ~ space ~ ellipticArgs map EllipticRel) | ("A" ~ space ~ ellipticArgs map Elliptic))
    val cubicArgs = P((threeCoordPairs ~ commaWsp).rep(1))
    val cubic: Parser[PathCommand] = P(("c" ~ space ~ cubicArgs map CubicRel) | ("C" ~ space ~ cubicArgs map Cubic))
    val smoothCubic: Parser[PathCommand] = P(("s" ~ cubicArgs map SmoothCubicRel) | ("S" ~ cubicArgs map SmoothCubic))
    val closePath: Parser[PathCommand] = P(CharIn("zZ") map (_ => ClosePath()))
    val command: Parser[PathCommand] = P(closePath | lineTo | horizLineTo | vertLineTo | cubic | smoothCubic | quad | ellipticalArc)
    val pathCommands: Parser[Seq[(PathCommand, Seq[PathCommand])]] = P(((moveTo ~ space) ~ (command ~ space).rep ~ space).rep(1))
    val path: Parser[Path] = P(pathCommands.map(seq => Path(seq.flatMap { case (m: PathCommand, c: Seq[PathCommand]) => m +: c })))
  }

}


case class Path(commands: Seq[PathCommand]) extends Code {

  import Path._
  import scalaz._
  import Scalaz._

  def trackCoords(x: Double, y: Double): String =
    s"""
      x = $x;
      x = $y;
     """

  override def toAndroidCode: Named[AndroidCode] =
    AndroidCode("{\n" +
      "Path path = new Path()", {
      var xNow = 0.0
      var yNow = 0.0
      def changeCoords(dx: Double, dy: Double) = {
        xNow += dx
        yNow += dy
      }
      def setCoords(x: Double, y: Double) = {
        xNow = x
        yNow = y
      }
      commands.foldLeft("") { (sofar, cmd) =>
        sofar +|+ (cmd match {
          case ClosePath() => "path.close()"
          case MoveTo(coords) => foldCmd[Coords](coords, { case (x, y) => setCoords(x, y); s"path.moveTo($x, $y)" })
          case MoveToRel(coords) => foldCmd[Coords](coords, { case (x, y) => changeCoords(x, y); s"path.rMoveTo($x, $y)" })
          case LineTo(coords) => foldCmd[Coords](coords, { case (x, y) => setCoords(x, y); s"path.lineTo($x, $y)" })
          case LineToRel(coords) => foldCmd[Coords](coords, { case (x, y) => changeCoords(x, y); s"path.rLineTo($x, $y)" })
          case VerticalLineTo(coords) => foldCmd[Double](coords, y => {
            yNow = y
            s"path.lineTo($xNow, $y)"
          })
          case VerticalLineToRel(coords) => foldCmd[Double](coords, y => {
            yNow += y
            s"path.rLineTo(0, $y)"
          })
          case HorizLineTo(coords) => foldCmd[Double](coords, x => {
            xNow = x
            s"path.lineTo($x, $yNow)"
          })
          case HorizLineToRel(coords) => foldCmd[Double](coords, x => {
            xNow += x
            s"path.rLineTo($x, 0)"
          })
          case Cubic(args) => foldCmd[(Coords, Coords, Coords)](args, { case ((x1, y1), (x2, y2), (x, y)) => setCoords(x, y); s"path.cubicTo($x1, $y1, $x2, $y2, $x, $y)" }) // TODO
          case CubicRel(args) => foldCmd[(Coords, Coords, Coords)](args, { case ((x1, y1), (x2, y2), (x, y)) => changeCoords(x, y); s"path.rCubicTo($x1, $y1, $x2, $y2, $x, $y)" }) // TODO
          case SmoothCubic(args) => ??? // TODO
          case SmoothCubicRel(args) => ??? // TODO
          case Quad(args) => foldCmd[(Coords, Coords)](args, { case ((x1, y1), (x, y)) => setCoords(x, y); s"path.quadTo($x1, $y1, $x, $y)" }) // TODO
          case QuadRel(args) => foldCmd[(Coords, Coords)](args, { case ((x1, y1), (x, y)) => changeCoords(x, y); s"path.rQuadTo($x1, $y1, $x, $y)" }) // TODO
          case Elliptic(_) => ??? // TODO
          case EllipticRel(_) => ??? // TODO
        })
      }
    }
    ,
    "}"
    ).pure[Named]

  def foldCmd[T](s: Seq[T], f: T => String): String = s.map(f) match {
    case x if x.isEmpty => ""
    case xs => xs.reduce(_ +|+ _)
  }

  override def toIOSCode: Named[IOSCode] = {
    "".pure[Named]
  }
}
