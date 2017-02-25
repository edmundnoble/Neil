package io
package enoble
package svg2d
package xmlparse

import fastparse.core.Implicits.Repeater
import fastparse.core.Implicits.Repeater.UnitRepeater
import io.enoble.svg2d.ast._

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import cats.instances.vector._
import cats.syntax.foldable._

import scala.annotation.switch

object Path extends Model {

  import fastparse.all._

  import xml.Elem

  override val label: String = "path"

  override def apply[A](v1: Elem, svg: FinalSVG[A]): Option[A] = {
    val pathCoords = v1.getOpt("d")
    val parsedPath: Option[Parsed[svg.Paths]] = pathCoords.map(s => new Path.Parsers[svg.Paths](svg.path).path.parse(s))
    if (parsedPath.isEmpty) System.err.println("No 'd' attribute found in path element")
    parsedPath.flatMap {
      case Parsed.Success(path, _) => Some(svg.includePath(path))
      case _: Parsed.Failure =>
        System.err.println(s"Failed to parse path: ${pathCoords.get}")
        None
    }
  }

  case class Parsers[A](pathCtx: FinalPath[A]) {

    type Coords = (Double, Double)

    import fastparse.all._

    import scala.language.implicitConversions

    case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
      def apply(t: T) = f(t)

      override def toString() = name
    }

    //
    //    implicit def vectorRepeater[R]: Repeater[R, Vector[R]] = new Repeater[R, Vector[R]] {
    //      override type Acc = mutable.Builder[R, Vector[R]]
    //
    //      override def initial: Acc = Vector.newBuilder[R]
    //
    //      override def accumulate(t: R, acc: Acc): Unit = acc += t
    //
    //      override def result(acc: Acc): Vector[R] = acc.result()
    //    }

    class PathBuilder {
      var soFar: A = pathCtx.empty
    }

    implicit def pathRepeater: Repeater[A, A] = new Repeater[A, A] {
      override type Acc = PathBuilder

      override def initial: PathBuilder = new PathBuilder

      override def accumulate(t: A, acc: PathBuilder): Unit =
        acc.soFar = pathCtx.append(acc.soFar, t)

      override def result(acc: PathBuilder): A =
        acc.soFar
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

    val coordPair: P[Coords] = (number ~ commaWsp) ~/ number
    val twoCoordPairs: P[(Double, Double, Double, Double)] =
      P(number ~ commaWsp ~/ number ~ commaWsp ~/
        number ~ commaWsp ~/ number)
    val threeCoordPairs: P[(Double, Double, Double, Double, Double, Double)] =
      P(twoCoordPairs ~ commaWsp ~/ number ~ commaWsp ~/ number)
    val wspAndCoordPairs: P[(Double, Double, Double, Double)] = P(commaWsp ~ twoCoordPairs)
    val wspAndCoord: P[Coords] = P(commaWsp ~ coordPair)
    // TODO: All of the extra arguments to moveTo should be interpreted as lineTo's, according to the spec
    val moveTo = P(
      ("m" ~/ wspAndCoord.map((pathCtx.moveToRel _).tupled).rep(1)) |
        ("M" ~/ wspAndCoord.map((pathCtx.moveTo _).tupled).rep(1))
    )
    val lineTo: P[A] = P(
      ("L" ~ wspAndCoord.map((pathCtx.lineTo _).tupled).rep(1)) |
        ("l" ~ wspAndCoord.map((pathCtx.lineToRel _).tupled).rep(1))
    )
    val singleLineToArgs: Parser[Double] = P((wspDouble ~ commaWsp))
    val horizLineTo: P[A] = P(
      ("h" ~ singleLineToArgs.map(pathCtx.horizLineToRel).rep(1)) |
        ("H" ~ singleLineToArgs.map(pathCtx.horizLineTo).rep(1))
    )
    val vertLineTo: P[A] = P(
      ("v" ~ singleLineToArgs.map(pathCtx.verticalLineToRel).rep(1)) |
        ("V" ~ singleLineToArgs.map(pathCtx.verticalLineTo).rep(1))
    )

    val quadArgs = P(twoCoordPairs ~ commaWsp)

    val quad: P[A] = P(
      ("q" ~ space ~ quadArgs.map((pathCtx.quadRel _).tupled).rep(1)) |
        ("Q" ~ space ~ quadArgs.map((pathCtx.quad _).tupled).rep(1))
    )

    val flag: P[Boolean] = CharIn("01").!.map(s => (s.charAt(0): @switch) match {
      case '0' => false
      case '1' => true
    })

    val ellipticParam: P[(Double, Double, Double, Boolean, Boolean, Double, Double)] =
      number ~ commaWsp ~ number ~ commaWsp ~
        number ~ commaWsp ~
        flag ~ commaWsp ~ flag ~ commaWsp ~
        number ~ commaWsp ~ number

    val ellipticalArc: P[A] = P(
      ("a" ~ space ~ ellipticParam.map((pathCtx.ellipticRel _).tupled).rep(1, sep = commaWsp)) |
        ("A" ~ space ~ ellipticParam.map((pathCtx.elliptic _).tupled).rep(1, sep = commaWsp))
    )

    val cubic: Parser[A] = P(
      ("c" ~ space ~ threeCoordPairs.map((pathCtx.cubicRel _).tupled).rep(1, sep = commaWsp)) |
        ("C" ~ space ~ threeCoordPairs.map((pathCtx.cubic _).tupled).rep(1, sep = commaWsp))
    )

    val smoothCubic: Parser[A] = P(
      ("s" ~ twoCoordPairs.map((pathCtx.smoothCubicRel _).tupled).rep(1, sep = commaWsp)) |
        ("S" ~ twoCoordPairs.map((pathCtx.smoothCubic _).tupled).rep(1, sep = commaWsp))
    )

    val closePath: Parser[A] = P(CharIn("zZ") map (_ => pathCtx.closePath()))

    val command: Parser[A] = P(
      closePath | lineTo | horizLineTo | vertLineTo | cubic | smoothCubic | quad | ellipticalArc
    )
    val pathCommands: Parser[A] =
      P(((moveTo ~ space) ~ (command ~ space).rep(pathRepeater))
        .map((pathCtx.append _).tupled)
        .rep(1))
    val path: Parser[A] = P(pathCommands)
  }

}
