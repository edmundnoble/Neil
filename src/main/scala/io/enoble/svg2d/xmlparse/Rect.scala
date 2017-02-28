package io
package enoble
package svg2d
package xmlparse

import io.enoble.svg2d.ast.FinalSVG

import scala.xml.MetaData

object Rect extends Model {
  override val label: String = "rect"
  override def apply[A](v1: MetaData, svg: FinalSVG[A]): Option[A] = {
    for {
      x <- v1.getOpt("x").flatMap(_.asDouble)
      y <- v1.getOpt("y").flatMap(_.asDouble)
      w <- v1.getOpt("width").flatMap(_.asDouble)
      h <- v1.getOpt("height").flatMap(_.asDouble)
    } yield svg.rect(x, y, w, h)
  }
}

