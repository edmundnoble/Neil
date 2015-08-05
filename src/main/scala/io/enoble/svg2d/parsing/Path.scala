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
    parsedPath.flatMap {
      case Success(path, _) => Some(path)
      case Failure(_, _) => None
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

    val coordPair: P[Coords] = (number ~ commaWsp.?) ~! number
    val twoCoordPairs: P[(Coords, Coords)] = P((coordPair ~ commaWsp.? ~ coordPair).map {
      case (x1, y1, c1) => ((x1, y1), c1)
    })
    val threeCoordPairs: P[(Coords, Coords, Coords)] = P((coordPair ~ commaWsp.? ~ coordPair ~ commaWsp.? ~ coordPair).map {
      case (x1, y1, c1, c2) => ((x1, y1), c1, c2)
    })
    val wspAndCoordPairs: P[(Coords, Coords)] = P(commaWsp ~ twoCoordPairs)
    val wspAndCoord: P[Coords] = P(commaWsp ~ coordPair)
    val moveToArgs = P(wspAndCoord.rep(1))
    val moveTo: P[PathCommand] = P(("m" ~ moveToArgs map MoveToRel) | ("M" ~ moveToArgs map MoveTo))
    val lineToArgs = P(moveToArgs)
    val lineTo: P[PathCommand] = P(("L" ~ lineToArgs map LineTo) | ("l" ~ lineToArgs map LineToRel))
    val singleLineToArgs = P((wspDouble ~ commaWsp).rep(1))
    val horizLineTo: P[PathCommand] = P((("h" ~ singleLineToArgs) map HorizLineToRel) | (("H" ~ singleLineToArgs) map HorizLineTo))
    val vertLineTo: P[PathCommand] = P((("v" ~ singleLineToArgs) map VerticalLineToRel) | (("V" ~ singleLineToArgs) map VerticalLineTo))
    val quadArgs = P((twoCoordPairs ~ commaWsp).rep(1))
    val quad: P[PathCommand] = P((("q" ~ quadArgs) map QuadRel) | (("Q" ~ quadArgs) map Quad))
    //    def ellipticArgs: Parser[EllipticParam] =
    //    def ellipticalArc: Parser[PathCommand] = ("a" ~> ellipticArgs ^^ Elliptic) | ("A" ~> ellipticArgs ^^ EllipticRel)
    val cubicArgs = P((threeCoordPairs ~ commaWsp).rep(1))
    val cubic: Parser[PathCommand] = P(("c" ~ space ~ cubicArgs map CubicRel) | ("C" ~ space ~ cubicArgs map Cubic))
    val smoothCubic: Parser[PathCommand] = P(("s" ~ cubicArgs map SmoothCubicRel) | ("S" ~ cubicArgs map SmoothCubic))
    val closePath: Parser[PathCommand] = P((CharIn("z") | CharIn("Z")) map (_ => ClosePath()))
    val command: Parser[PathCommand] = P(closePath | lineTo | horizLineTo | vertLineTo | cubic | smoothCubic | quad)
    //| ellipticalArc
    val pathCommands: Parser[Seq[(PathCommand, Seq[PathCommand])]] = P(((moveTo ~ space) ~ (command ~ space).rep(1) ~ space).rep(1))
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

  type CoordAware[T] = State[(Double, Double), T]

  override def toAndroidCode: Named[AndroidCode] =
    AndroidCode("{\n" +
      "Path path = new Path()",
      (commands.foldLeft("".pure[CoordAware]) { (sofar, cmd) =>
        for {
          str <- sofar
          cmds <- cmd match {
            case ClosePath() => "path.close()".pure[CoordAware]
            case MoveTo(coords) => foldCmd[Coords](coords, { case (x, y) => for (_ <- put((x, y))) yield s"path.moveTo($x, $y)" })
            case MoveToRel(coords) => foldCmd[Coords](coords, { case (x, y) => for (_ <- changeCoords(x, y)) yield (s"path.rMoveTo($x, $y)") })
            case LineTo(coords) => foldCmd[Coords](coords, { case (x, y) => for (_ <- put((x, y))) yield s"path.lineTo($x, $y)" })
            case LineToRel(coords) => foldCmd[Coords](coords, { case (x, y) => for (_ <- changeCoords(x, y)) yield s"path.rLineTo($x, $y)" })
            case VerticalLineTo(coords) => foldCmd[Double](coords, y => for (_ <- setYCoord(y); p <- get[(Double, Double)]) yield s"path.lineTo(${p._1}, $y)")
            case VerticalLineToRel(coords) => foldCmd[Double](coords, y => for (_ <- changeYCoord(y)) yield s"path.rLineTo(0, $y)")
            case HorizLineTo(coords) => foldCmd[Double](coords, x => for (_ <- setXCoord(x); p <- get[(Double, Double)]) yield s"path.lineTo($x, ${p._2})")
            case HorizLineToRel(coords) => foldCmd[Double](coords, x => for (_ <- changeXCoord(x)) yield s"path.rLineTo($x, 0)")
            case Cubic(coords) => foldCmd[(Coords, Coords, Coords)](coords, { case ((x1, y1), (x2, y2), (x, y)) => for (_ <- put((x, y))) yield s"path.cubicTo($x1, $y1, $x2, $y2, $x, $y)" }) // TODO
            case CubicRel(coords) => foldCmd[(Coords, Coords, Coords)](coords, { case ((x1, y1), (x2, y2), (x, y)) => for (_ <- changeCoords(x, y)) yield s"path.rCubicTo($x1, $y1, $x2, $y2, $x, $y)" }) // TODO
            case SmoothCubic(coords) => ??? // TODO
            case SmoothCubicRel(coords) => ??? // TODO
            case Quad(coords) => foldCmd[(Coords, Coords)](coords, { case ((x1, y1), (x, y)) => for (_ <- put((x, y))) yield s"path.quadTo($x1, $y1, $x, $y)" }) // TODO
            case QuadRel(coords) => foldCmd[(Coords, Coords)](coords, { case ((x1, y1), (x, y)) => for (_ <- changeCoords(x, y)) yield s"path.rQuadTo($x1, $y1, $x, $y)" }) // TODO
            case Elliptic(_) => ??? // TODO
            case EllipticRel(_) => ??? // TODO
          }
        } yield (str +|+ cmds)
      }).run((0.0, 0.0))._2,
      "}"
    ).pure[Named]

  def changeYCoord(dy: Double): CoordAware[Unit] = for {
    p <- get[(Double, Double)]
    (oldX, oldY) = p
    _ <- put((oldX, oldY + dy))
  } yield ()

  def changeXCoord(dx: Double): CoordAware[Unit] = for {
    p <- get[(Double, Double)]
    (oldX, oldY) = p
    _ <- put((oldX + dx, oldY))
  } yield ()

  def setYCoord(y: Double): CoordAware[Unit] = for {
    p <- get[(Double, Double)]
    (oldX, oldY) = p
    _ <- put((oldX, y))
  } yield ()

  def setXCoord(x: Double): CoordAware[Unit] = for {
    p <- get[(Double, Double)]
    (oldX, oldY) = p
    _ <- put((x, oldY))
  } yield ()

  def changeCoords(dx: Double, dy: Double): CoordAware[Unit] = for {
    _ <- changeYCoord(dy)
    _ <- changeXCoord(dx)
  } yield ()

  def foldCmd[T](s: Seq[T], f: T => CoordAware[String]): CoordAware[String] = s.map(f) match {
    case x if x.isEmpty => "".pure[CoordAware]
    case xs => xs.reduce((a, b) => for (x <- a; y <- b) yield (x +|+ y))
  }

  override def toIOSCode: Named[IOSCode] = {
    "".pure[Named]
  }
}
