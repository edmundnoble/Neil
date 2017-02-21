package io
package enoble
package svg2d
package ast

trait InitialSVG

case class DrawText(text: String, posX: Double, posY: Double) extends InitialSVG
case class DrawEllipse(x: Double, y: Double, rx: Double, ry: Double) extends InitialSVG
case class DrawCircle(x: Double, y: Double, r: Double) extends InitialSVG
case class DrawPath(commands: Seq[InitialPath]) extends InitialSVG

trait InitialPath

object InitialPath {
  type Coords = (Double, Double)
}

import io.enoble.svg2d.ast.InitialPath.Coords

case class ClosePath() extends InitialPath
case class MoveTo(points: Vector[Coords]) extends InitialPath
case class MoveToRel(points: Vector[Coords]) extends InitialPath
case class LineTo(points: Vector[Coords]) extends InitialPath
case class LineToRel(point: Vector[Coords]) extends InitialPath
case class VerticalLineTo(y: Vector[Double]) extends InitialPath
case class VerticalLineToRel(y: Vector[Double]) extends InitialPath
case class HorizLineTo(x: Vector[Double]) extends InitialPath
case class HorizLineToRel(x: Vector[Double]) extends InitialPath
case class Cubic(params: Vector[(Coords, Coords, Coords)]) extends InitialPath
case class CubicRel(params: Vector[(Coords, Coords, Coords)]) extends InitialPath
case class SmoothCubic(params: Vector[(Coords, Coords, Coords)]) extends InitialPath
case class SmoothCubicRel(params: Vector[(Coords, Coords, Coords)]) extends InitialPath
case class Quad(params: Vector[(Coords, Coords)]) extends InitialPath
case class QuadRel(params: Vector[(Coords, Coords)]) extends InitialPath
case class Elliptic(params: Vector[EllipticParam]) extends InitialPath
case class EllipticRel(params: Vector[EllipticParam]) extends InitialPath
