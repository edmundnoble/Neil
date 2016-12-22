package io.enoble.svg2d.render

import io.enoble.svg2d.ast._

import scalaz.Monoid

object InitialPathRenderer extends FinalPath[Vector[InitialPath]] {
  override implicit def monoid[AA >: Vector[InitialPath]] =
    scalaz.std.vector.vectorMonoid[InitialPath].asInstanceOf[Monoid[AA]]
  override def closePath() = Vector(ClosePath())
  override def moveTo(points: Vector[(Double, Double)]) = Vector(MoveTo(points))
  override def moveToRel(points: Vector[(Double, Double)]) = Vector(MoveToRel(points))
  override def lineTo(points: Vector[(Double, Double)]) = Vector(LineTo(points))
  override def lineToRel(points: Vector[(Double, Double)]) = Vector(LineToRel(points))
  override def verticalLineTo(y: Vector[Double]) = Vector(VerticalLineTo(y))
  override def verticalLineToRel(y: Vector[Double]) = Vector(VerticalLineToRel(y))
  override def horizLineTo(y: Vector[Double]) = Vector(HorizLineTo(y))
  override def horizLineToRel(y: Vector[Double]) = Vector(HorizLineToRel(y))
  override def cubic(params: Vector[((Double, Double), (Double, Double), (Double, Double))]) = Vector(Cubic(params))
  override def cubicRel(params: Vector[((Double, Double), (Double, Double), (Double, Double))]) = Vector(CubicRel(params))
  override def smoothCubic(params: Vector[((Double, Double), (Double, Double), (Double, Double))]) = Vector(SmoothCubic(params))
  override def smoothCubicRel(params: Vector[((Double, Double), (Double, Double), (Double, Double))]) = Vector(SmoothCubicRel(params))
  override def quad(params: Vector[((Double, Double), (Double, Double))]) = Vector(Quad(params))
  override def quadRel(params: Vector[((Double, Double), (Double, Double))]) = Vector(QuadRel(params))
  override def elliptic(params: Vector[EllipticParam]) = Vector(Elliptic(params))
  override def ellipticRel(params: Vector[EllipticParam]) = Vector(EllipticRel(params))
}
