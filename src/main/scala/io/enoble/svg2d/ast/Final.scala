package io.enoble.svg2d.ast

trait FinalSVG[A] {
  type Paths

  val path: FinalPath[Paths]

  val empty: A

  def append(fst: A, snd: A): A

  def circle(x: Double, y: Double, r: Double): A

  def ellipse(x: Double, y: Double, rx: Double, ry: Double): A

  def text(text: String, x: Double, y: Double): A

  def path(paths: Paths): A
}

trait FinalPath[A] {

  type Coords = (Double, Double)

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
}
