package io
package enoble
package svg2d
package ast

trait FinalSVG[A] {
  type Paths

  val path: FinalPath[Paths]

  val empty: A

  def append(fst: A, snd: A): A

  def circle(x: Double, y: Double, r: Double): A

  def ellipse(x: Double, y: Double, rx: Double, ry: Double): A

  def text(text: String, x: Double, y: Double): A

  def includePath(paths: Paths): A
}

trait FinalPath[A] {

  val empty: A

  def append(fst: A, snd: A): A

  def closePath(): A

  def moveTo(points: Vector[Coords]): A

  def moveToRel(points: Vector[Coords]): A

  def lineTo(points: Vector[Coords]): A

  def lineToRel(points: Vector[Coords]): A

  def verticalLineTo(y: Vector[Double]): A

  def verticalLineToRel(y: Vector[Double]): A

  def horizLineTo(y: Vector[Double]): A

  def horizLineToRel(y: Vector[Double]): A

  def cubic(params: Vector[(Coords, Coords, Coords)]): A

  def cubicRel(params: Vector[(Coords, Coords, Coords)]): A

  def smoothCubic(params: Vector[(Coords, Coords, Coords)]): A

  def smoothCubicRel(params: Vector[(Coords, Coords, Coords)]): A

  def quad(params: Vector[(Coords, Coords)]): A

  def quadRel(params: Vector[(Coords, Coords)]): A

  def elliptic(params: Vector[EllipticParam]): A

  def ellipticRel(params: Vector[EllipticParam]): A

  def renderInitial(ins: Vector[InitialPath]): A = ins.foldLeft(empty) { (a, ip) =>
    append(a, ip match {
      case ClosePath() => closePath()
      case MoveTo(points: Vector[Coords]) => moveTo(points)
      case MoveToRel(points: Vector[Coords]) => moveToRel(points)
      case LineTo(points: Vector[Coords]) => lineTo(points)
      case LineToRel(points: Vector[Coords]) => lineToRel(points)
      case VerticalLineTo(y: Vector[Double]) => verticalLineTo(y)
      case VerticalLineToRel(y: Vector[Double]) => verticalLineToRel(y)
      case HorizLineTo(x: Vector[Double]) => horizLineTo(x)
      case HorizLineToRel(x: Vector[Double]) => horizLineToRel(x)
      case Cubic(params: Vector[(Coords, Coords, Coords)]) => cubic(params)
      case CubicRel(params: Vector[(Coords, Coords, Coords)]) => cubicRel(params)
      case SmoothCubic(params: Vector[(Coords, Coords, Coords)]) => smoothCubicRel(params)
      case SmoothCubicRel(params: Vector[(Coords, Coords, Coords)]) => smoothCubicRel(params)
      case Quad(params: Vector[(Coords, Coords)]) => quad(params)
      case QuadRel(params: Vector[(Coords, Coords)]) => quadRel(params)
      case Elliptic(params: Vector[EllipticParam]) => elliptic(params)
      case EllipticRel(params: Vector[EllipticParam]) => ellipticRel(params)
    })
  }
}
