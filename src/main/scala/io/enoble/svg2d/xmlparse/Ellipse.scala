package io
package enoble
package svg2d
package xmlparse


import io.enoble.svg2d.ast.FinalSVG
import io.enoble.svg2d.utils.Named.Named

import scala.xml.Elem
import scalaz._
import Scalaz._

object Ellipse extends Model {
  override val label: String = "ellipse"

  override def apply[A](v1: Elem, svg: FinalSVG[A]): Option[A] = {
    val centerX = v1.getOrDefault("cx", "0")
    val centerY = v1.getOrDefault("cy", "0")
    val radiusX = v1.getOrDefault("rx", "0")
    val radiusY = v1.getOrDefault("ry", "0")
    if (radiusX == "0" && radiusY == "0") {
      None
    } else {
      for {
        x <- centerX.asDouble if x >= 0
        y <- centerY.asDouble if y >= 0
        rx <- radiusX.asDouble
        ry <- radiusY.asDouble
      } yield svg.ellipse(x, y, rx, ry)
    }
  }
}
