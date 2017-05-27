package io
package enoble
package svg2d
package xmlparse

import fastparse.core.Implicits.Repeater
import io.enoble.svg2d.ast._

import scala.collection.mutable.ListBuffer
import scala.xml.MetaData

object Path extends Model {

  import fastparse.all._

  override val label: String = "path"

  override def apply[A](v1: MetaData, svg: FinalSVG[A]): Option[A] = {
    val pathCoords = v1.getOpt("d")
    val parsedPath: Option[Parsed[PathFun]] =
      pathCoords.map(s => BaseParsers.path.parse(s))
    if (parsedPath.isEmpty) System.err.println("No 'd' attribute found in path element")
    parsedPath.flatMap {
      case Parsed.Success(path, _) => Some(svg.includePath(path(svg.path)))
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

    val plusminusOpt: P[Unit] = P(CharIn("+-").?)
    val space = P(CharsWhileIn(Array(' ', '\n')).?)
    val digits = P(CharsWhileIn('0' to '9'))
    val exponent = P(CharIn("eE") ~ plusminusOpt ~ digits)
    val number: P[Double] = P(
      plusminusOpt ~ ("0" | CharIn('1' to '9') ~/ digits.?) ~/
        ("." ~ digits.?).? ~ exponent.?
    ).!.map(_.toDouble)

    val commaWsp = P((space ~ ",".? ~ space) | ("," ~ space))
    val wspDouble = P(space ~ number)

    val coordPair: P[Coords] = (number ~ commaWsp) ~ number
    val twoCoordPairs: P[(Double, Double, Double, Double)] =
      P(coordPair ~ commaWsp ~
        number ~ commaWsp ~ number)
    val threeCoordPairs: P[(Double, Double, Double, Double, Double, Double)] =
      P(twoCoordPairs ~ commaWsp ~ number ~ commaWsp ~ number)

    val flag: P[Boolean] = CharIn("01").!.map(s => s.charAt(0) == '1')

    val ellipticParam: P[(Double, Double, Double, Boolean, Boolean, Double, Double)] =
      coordPair ~ commaWsp ~
        number ~ commaWsp ~
        flag ~ commaWsp ~ flag ~ commaWsp ~
        number ~ commaWsp ~ number

    type PathParser = P[PathFun]

    import fastparse.all._

    implicit def testRepeater[A]: Repeater[A, List[A]] = new Repeater[A, List[A]] {
      override type Acc = ListBuffer[A]

      override def initial: ListBuffer[A] = new ListBuffer

      override def accumulate(t: A, acc: ListBuffer[A]): Unit =
        acc += t

      override def result(acc: ListBuffer[A]): List[A] =
        acc.result()
    }

    def foldPathFun[T](is: List[T])(ts: T => PathFun): PathFun = new PathFun {
      def apply[A](pathCtx: FinalPath[A]): A =
        if (is.isEmpty) pathCtx.empty
        else is.foldLeft(pathCtx.empty)((b, t) => pathCtx.append(b, ts(t)(pathCtx)))
    }

    def foldPathFunE[T](is: List[PathFun]): PathFun = new PathFun {
      def apply[A](pathCtx: FinalPath[A]): A =
        if (is.isEmpty) pathCtx.empty
        else is.foldLeft(pathCtx.empty)((b, t) => pathCtx.append(b, t(pathCtx)))
    }


    // TODO: All of the extra arguments to moveTo should be interpreted as lineTo's, according to the spec
    val moveTo: P[PathFun] = P(
      ("m" ~/ space ~ coordPair.rep(1, sep = commaWsp)
        .map(foldPathFun(_)((PathFun.moveToRel _).tupled))) |
        ("M" ~/ space ~ coordPair.rep(1, sep = commaWsp)
          .map(foldPathFun(_)((PathFun.moveTo _).tupled)))
    )
    val lineTo: P[PathFun] = P(
      ("L" ~/ space ~ coordPair.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.lineTo _).tupled))) |
        ("l" ~/ space ~ coordPair.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.lineToRel _).tupled)))
    )
    val horizLineTo: P[PathFun] = P(
      ("h" ~/ space ~ wspDouble.rep(1, sep = commaWsp).map(foldPathFun(_)(PathFun.horizLineToRel))) |
        ("H" ~/ space ~ wspDouble.rep(1, sep = commaWsp).map(foldPathFun(_)(PathFun.horizLineTo)))
    )
    val vertLineTo: P[PathFun] = P(
      ("v" ~/ space ~ wspDouble.rep(1, sep = commaWsp).map(foldPathFun(_)(PathFun.verticalLineToRel))) |
        ("V" ~/ space ~ wspDouble.rep(1, sep = commaWsp).map(foldPathFun(_)(PathFun.verticalLineTo)))
    )

    val quad: P[PathFun] = P(
      ("q" ~/ space ~ twoCoordPairs.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.quadRel _).tupled))) |
        ("Q" ~/ space ~ twoCoordPairs.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.quad _).tupled)))
    )

    val ellipticalArc: P[PathFun] = P(
      ("a" ~/ space ~ ellipticParam.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.ellipticRel _).tupled))) |
        ("A" ~/ space ~ ellipticParam.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.elliptic _).tupled)))
    )

    val cubic: P[PathFun] = P(
      ("c" ~/ space ~ threeCoordPairs.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.cubicRel _).tupled))) |
        ("C" ~/ space ~ threeCoordPairs.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.cubic _).tupled)))
    )

    val smoothCubic: P[PathFun] = P(
      ("s" ~/ space ~ twoCoordPairs.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.smoothCubicRel _).tupled))) |
        ("S" ~/ space ~ twoCoordPairs.rep(1, sep = commaWsp).map(foldPathFun(_)((PathFun.smoothCubic _).tupled)))
    )

    val closePath: P[PathFun] = P(CharIn("zZ") map (_ => PathFun.closePath))

    val command: P[PathFun] = P(
      closePath | lineTo | horizLineTo | vertLineTo | cubic | smoothCubic | quad | ellipticalArc
    )

    val path: P[PathFun] =
      P(
        ((moveTo ~ space) ~ (command ~ space).rep)
          .map(x => new PathFun {
            def apply[A](pathCtx: FinalPath[A]): A =
              x._2.foldLeft(x._1(pathCtx))((b, t) => pathCtx.append(b, t(pathCtx)))
          })
          .rep(1).map(foldPathFunE)
      )
  }
}

