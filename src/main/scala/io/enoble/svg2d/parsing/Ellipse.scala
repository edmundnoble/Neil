package io.enoble.svg2d.parsing


import io.enoble.svg2d.parsing.Code.Named

import scala.xml.Elem
import scalaz._
import Scalaz._

object EllipseParser extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "ellipse"
  override def apply(v1: Elem): Option[Vector[Code]] = {
    val centerX = v1.getOrDefault("cx", "0")
    val centerY = v1.getOrDefault("cy", "0")
    val radiusX = v1.getOrDefault("rx", "0")
    val radiusY = v1.getOrDefault("ry", "0")
    if (radiusX == "0" && radiusY == "0") {
      Some(Vector.empty[Code])
    } else {
      for {
        x <- centerX.asDouble if x >= 0
        y <- centerY.asDouble if y >= 0
        rx <- radiusX.asDouble
        ry <- radiusY.asDouble
      } yield Vector(DrawEllipse(x, y, rx, ry))
    }
  }
}
case class DrawEllipse(x: Double, y: Double, rx: Double, ry: Double) extends Code {
  def left = x - (rx / 2)
  def top = y + (ry / 2)
  def right = x + (rx / 2)
  def bottom = y - (ry / 2)
  override def toAndroidCode: Named[AndroidCode] =
    AndroidCode(s"RectF bounds = new RectF($left, $top, $right, $bottom)", "c.drawOval(bounds, p)").pure[Named]
  override def toIOSCode: Named[IOSCode] = ???
}