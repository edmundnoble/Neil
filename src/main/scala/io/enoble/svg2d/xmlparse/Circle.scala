package io
package enoble
package svg2d
package xmlparse


import io.enoble.svg2d.ast.FinalSVG

import scala.xml.{Elem, MetaData}

object Circle extends Model {
  override val label: String = "circle"

  override def apply[A](v1: MetaData, svg: FinalSVG[A]): Option[A] = {
    val centerX = v1.getOrDefault("cx", "0")
    val centerY = v1.getOrDefault("cy", "0")
    val radius = v1.getOrDefault("r", "0")

    if (radius == "0") {
      None
    } else {
      for {x <- centerX.asDouble if x >= 0
           y <- centerY.asDouble if y >= 0
           r <- radius.asDouble
      } yield svg.circle(x, y, r)
    }
  }
}
