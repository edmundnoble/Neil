package io.enoble.svg2d.drawables

import com.squareup.javapoet.MethodSpec.Builder

import scala.xml.Elem
import scalaz._
import Scalaz._

object Ellipse extends Model {
  override def isDefinedAt(x: Elem): Boolean = x.label =~= "ellipse"
  override def apply(v1: Elem): Option[(AndroidCode, IOSCode)] = {
    val centerX = v1.getOrDefault("cx", "0")
    val centerY = v1.getOrDefault("cy", "0")
    val radiusX = v1.getOrDefault("rx", "0")
    val radiusY = v1.getOrDefault("ry", "0")
    if (radiusX == "0" || radiusY == "0") {
      Some(("", ""))
    } else {
      for {
        x <- centerX.asDouble if x >= 0
        y <- centerY.asDouble if y >= 0
        rx <- radiusX.asDouble
        ry <- radiusY.asDouble
        left = x - (rx / 2)
        top = y + (ry / 2)
        right = x + (rx / 2)
        bottom = y - (ry / 2)
        androidCode = s"RectF bounds = new RectF($left, $top, $right, $bottom);" + "c.drawOval(bounds, p);"
        iosCode = ""
      } yield (androidCode, iosCode)
    }
  }
}