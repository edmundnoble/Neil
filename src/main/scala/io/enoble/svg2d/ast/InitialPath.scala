package io
package enoble
package svg2d
package ast

trait InitialPath

case class ClosePath() extends InitialPath
case class MoveTo(x: Double, y: Double) extends InitialPath
case class MoveToRel(dx: Double, dy: Double) extends InitialPath
case class LineTo(x: Double, y: Double) extends InitialPath
case class LineToRel(dx: Double, dy: Double) extends InitialPath
case class VerticalLineTo(y: Double) extends InitialPath
case class VerticalLineToRel(dy: Double) extends InitialPath
case class HorizLineTo(x: Double) extends InitialPath
case class HorizLineToRel(dx: Double) extends InitialPath
case class Cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double) extends InitialPath
case class CubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double) extends InitialPath
case class SmoothCubic(x2: Double, y2: Double, x: Double, y: Double) extends InitialPath
case class SmoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double) extends InitialPath
case class Quad(x1: Double, y1: Double, x: Double, y: Double) extends InitialPath
case class QuadRel(x1: Double, y1: Double, dx: Double, dy: Double) extends InitialPath
case class Elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double) extends InitialPath
case class EllipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double) extends InitialPath
