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
import scala.xml.MetaData

object Path extends Model {

  import fastparse.all._

  import xml.Elem

  override val label: String = "path"

  override def apply[A](v1: MetaData, svg: FinalSVG[A]): Option[A] = {
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

  // parsers that do not use a tagless action, and so can be initialized together.
  // this saves *massive* time.
  object BaseParsers {

    final case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
      def apply(t: T) = f(t)

      override def toString() = name
    }

    val Whitespace = NamedFunction(" \n".contains(_: Char), "Whitespace")
    val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
    val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

    val space = P(CharsWhile(Whitespace).?)
    val digits = P(CharsWhile(Digits))
    val exponent = P(CharIn("eE") ~ CharIn("+-").? ~ digits)
    val number: P[Double] = P(CharIn("-").? ~ ("0" | CharIn('1' to '9') ~/ digits.?) ~/ ("." ~ digits).? ~ exponent.?).!.map(_.toDouble)

    val commaWsp = P((space ~ ",".? ~ space) | ("," ~ space))
    val wspDouble = P(space ~ number)

    val coordPair: P[Coords] = (number ~ commaWsp) ~/ number
    val twoCoordPairs: P[(Double, Double, Double, Double)] =
      P(number ~ commaWsp ~/ number ~ commaWsp ~/
        number ~ commaWsp ~/ number)
    val threeCoordPairs: P[(Double, Double, Double, Double, Double, Double)] =
      P(twoCoordPairs ~ commaWsp ~/ number ~ commaWsp ~/ number)

    val flag: P[Boolean] = CharIn("01").!.map(s => s.charAt(0) == '1')

    val ellipticParam: P[(Double, Double, Double, Boolean, Boolean, Double, Double)] =
      number ~ commaWsp ~ number ~ commaWsp ~
        number ~ commaWsp ~
        flag ~ commaWsp ~ flag ~ commaWsp ~
        number ~ commaWsp ~ number

    val mSpace = "m" ~/ space
    val MSpace = "M" ~/ space
    val lSpace = "l" ~/ space
    val LSpace = "L" ~/ space
    val hSpace = "h" ~/ space
    val HSpace = "H" ~/ space
    val vSpace = "v" ~/ space
    val VSpace = "V" ~/ space
    val qSpace = "q" ~/ space
    val QSpace = "Q" ~/ space
    val aSpace = "a" ~/ space
    val ASpace = "A" ~/ space
    val cSpace = "c" ~/ space
    val CSpace = "C" ~/ space
    val sSpace = "s" ~/ space
    val SSpace = "S" ~/ space
  }

  final case class Parsers[A](pathCtx: FinalPath[A]) {

    import BaseParsers._

    import fastparse.all._

    import scala.language.implicitConversions

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

    // TODO: All of the extra arguments to moveTo should be interpreted as lineTo's, according to the spec
    val moveTo = P(
      (mSpace ~ coordPair.map((pathCtx.moveToRel _).tupled).rep(1, sep = commaWsp)) |
        (MSpace ~ coordPair.map((pathCtx.moveTo _).tupled).rep(1, sep = commaWsp))
    )
    val lineTo: P[A] = P(
      (LSpace ~ coordPair.map((pathCtx.lineTo _).tupled).rep(1, sep = commaWsp)) |
        (lSpace ~ coordPair.map((pathCtx.lineToRel _).tupled).rep(1, sep = commaWsp))
    )
    val horizLineTo: P[A] = P(
      (hSpace ~ wspDouble.map(pathCtx.horizLineToRel).rep(1, sep = commaWsp)) |
        (HSpace ~ wspDouble.map(pathCtx.horizLineTo).rep(1, sep = commaWsp))
    )
    val vertLineTo: P[A] = P(
      (vSpace ~ wspDouble.map(pathCtx.verticalLineToRel).rep(1, sep = commaWsp)) |
        (VSpace ~ wspDouble.map(pathCtx.verticalLineTo).rep(1, sep = commaWsp))
    )

    val quad: P[A] = P(
     (qSpace ~ twoCoordPairs.map((pathCtx.quadRel _).tupled).rep(1, sep = commaWsp)) |
        (QSpace ~ twoCoordPairs.map((pathCtx.quad _).tupled).rep(1, sep = commaWsp))
    )

    val ellipticalArc: P[A] = P(
      (aSpace ~ ellipticParam.map((pathCtx.ellipticRel _).tupled).rep(1, sep = commaWsp)) |
        (ASpace ~ ellipticParam.map((pathCtx.elliptic _).tupled).rep(1, sep = commaWsp))
    )

    val cubic: Parser[A] = P(
       (cSpace ~ threeCoordPairs.map((pathCtx.cubicRel _).tupled).rep(1, sep = commaWsp)) |
        (CSpace ~ threeCoordPairs.map((pathCtx.cubic _).tupled).rep(1, sep = commaWsp))
    )

    val smoothCubic: Parser[A] = P(
      (sSpace ~ twoCoordPairs.map((pathCtx.smoothCubicRel _).tupled).rep(1, sep = commaWsp)) |
        (SSpace ~ twoCoordPairs.map((pathCtx.smoothCubic _).tupled).rep(1, sep = commaWsp))
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
