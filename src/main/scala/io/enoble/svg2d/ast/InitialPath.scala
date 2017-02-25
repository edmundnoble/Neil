package io
package enoble
package svg2d
package ast

trait InitialPath

case object ClosePath extends InitialPath
final case class MoveTo(x: Double, y: Double) extends InitialPath
final case class MoveToRel(dx: Double, dy: Double) extends InitialPath
final case class LineTo(x: Double, y: Double) extends InitialPath
final case class LineToRel(dx: Double, dy: Double) extends InitialPath
final case class VerticalLineTo(y: Double) extends InitialPath
final case class VerticalLineToRel(dy: Double) extends InitialPath
final case class HorizLineTo(x: Double) extends InitialPath
final case class HorizLineToRel(dx: Double) extends InitialPath
final case class Cubic(x1: Double, y1: Double, x2: Double, y2: Double, x: Double, y: Double) extends InitialPath
final case class CubicRel(x1: Double, y1: Double, x2: Double, y2: Double, dx: Double, dy: Double) extends InitialPath
final case class SmoothCubic(x2: Double, y2: Double, x: Double, y: Double) extends InitialPath
final case class SmoothCubicRel(x2: Double, y2: Double, dx: Double, dy: Double) extends InitialPath
final case class Quad(x1: Double, y1: Double, x: Double, y: Double) extends InitialPath
final case class QuadRel(x1: Double, y1: Double, dx: Double, dy: Double) extends InitialPath
final case class Elliptic(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, x: Double, y: Double) extends InitialPath
final case class EllipticRel(rx: Double, ry: Double, rotX: Double, largeArc: Boolean, sweep: Boolean, dx: Double, dy: Double) extends InitialPath
