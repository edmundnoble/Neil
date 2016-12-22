package io.enoble.svg2d.parsing


import io.enoble.svg2d.ast.FinalSVG

import scala.xml.Elem

object Circle extends Model {
  override def apply[A](v1: Elem, svg: FinalSVG[A]): Option[Option[A]] = if (v1.label ~= "circle") {
    val centerX = v1.getOrDefault("cx", "0")
    val centerY = v1.getOrDefault("cy", "0")
    val radius = v1.getOrDefault("r", "0")

    if (radius == "0") {
      Some(None)
    } else {
      for {x <- centerX.asDouble if x >= 0
           y <- centerY.asDouble if y >= 0
           r <- radius.asDouble
      } yield Some(svg.circle(x, y, r))
    }
  } else {
    None
  }
}
